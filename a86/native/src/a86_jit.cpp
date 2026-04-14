#include "a86_jit.h"

#include <atomic>
#include <cstdint>
#include <memory>
#include <string>
#include <utility>
#include <vector>

#include "llvm/ADT/SmallVector.h"
#include "llvm/ExecutionEngine/JITSymbol.h"
#include "llvm/ExecutionEngine/Orc/Core.h"
#include "llvm/ExecutionEngine/Orc/ExecutionUtils.h"
#include "llvm/ExecutionEngine/Orc/LLJIT.h"
#include "llvm/MC/MCAsmBackend.h"
#include "llvm/MC/MCAsmInfo.h"
#include "llvm/MC/MCCodeEmitter.h"
#include "llvm/MC/MCContext.h"
#include "llvm/MC/MCInstrInfo.h"
#include "llvm/MC/MCObjectFileInfo.h"
#include "llvm/MC/MCObjectWriter.h"
#include "llvm/MC/MCParser/MCAsmParser.h"
#include "llvm/MC/MCParser/MCTargetAsmParser.h"
#include "llvm/MC/MCRegisterInfo.h"
#include "llvm/MC/MCStreamer.h"
#include "llvm/MC/MCSubtargetInfo.h"
#include "llvm/MC/MCTargetOptions.h"
#include "llvm/MC/TargetRegistry.h"
#include "llvm/Support/Error.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/SourceMgr.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/TargetParser/Host.h"
#include "llvm/TargetParser/SubtargetFeature.h"

using namespace llvm;

struct a86_jit {
  std::unique_ptr<orc::LLJIT> jit;
  std::string last_error;
  std::string last_session_error;

  void clear_error() { last_error.clear(); }

  void clear_session_error() { last_session_error.clear(); }

  void set_error(std::string msg) { last_error = std::move(msg); }

  const char *error_cstr() const {
    return last_error.empty() ? nullptr : last_error.c_str();
  }

  void record_session_error(Error err) {
    std::string msg = toString(std::move(err));
    if (last_session_error.empty()) {
      last_session_error = std::move(msg);
    } else {
      last_session_error += "\n";
      last_session_error += msg;
    }
  }

  std::string combine_with_session_error(std::string high_level) const {
    if (last_session_error.empty()) {
      return high_level;
    }
    if (high_level.empty()) {
      return last_session_error;
    }
    return last_session_error + "\n" + high_level;
  }
};

struct a86_program {
  a86_jit *parent = nullptr;
  orc::JITDylib *jd = nullptr;
  orc::ResourceTrackerSP tracker;
};

namespace {

std::atomic<uint64_t> NextProgramId{0};

static std::unique_ptr<MemoryBuffer>
assemble_to_object(a86_jit *jit, StringRef asm_text) {
  jit->clear_session_error();

  std::string triple_name = sys::getProcessTriple();
  Triple TT(triple_name);

  std::string lookup_err;
  const Target *target = TargetRegistry::lookupTarget(TT, lookup_err);
  if (!target) {
    jit->set_error("lookupTarget failed: " + lookup_err);
    return nullptr;
  }

  MCTargetOptions mc_opts;

  auto mri = std::unique_ptr<MCRegisterInfo>(target->createMCRegInfo(TT));
  auto mai =
      std::unique_ptr<MCAsmInfo>(target->createMCAsmInfo(*mri, TT, mc_opts));
  auto mii = std::unique_ptr<MCInstrInfo>(target->createMCInstrInfo());

  std::string cpu = sys::getHostCPUName().str();

  auto host_features = sys::getHostCPUFeatures();
  SubtargetFeatures features;
  for (const auto &kv : host_features) {
    features.AddFeature(kv.getKey(), kv.getValue());
  }
  std::string feature_string = features.getString();

  auto sti = std::unique_ptr<MCSubtargetInfo>(
      target->createMCSubtargetInfo(TT, cpu, feature_string));

  if (!mri || !mai || !mii || !sti) {
    jit->set_error("failed to create MC target components");
    return nullptr;
  }

  SourceMgr sm;
  sm.AddNewSourceBuffer(
      MemoryBuffer::getMemBufferCopy(asm_text, "<a86-jit-asm>"), SMLoc());

  MCContext ctx(TT, mai.get(), mri.get(), sti.get(), &sm, &mc_opts);

  auto mofi =
      std::unique_ptr<MCObjectFileInfo>(target->createMCObjectFileInfo(ctx, true));
  ctx.setObjectFileInfo(mofi.get());

  SmallVector<char, 0> obj_bytes;
  raw_svector_ostream obj_stream(obj_bytes);

  auto mab = std::unique_ptr<MCAsmBackend>(
      target->createMCAsmBackend(*sti, *mri, mc_opts));
  auto mce =
      std::unique_ptr<MCCodeEmitter>(target->createMCCodeEmitter(*mii, ctx));

  if (!mab || !mce) {
    jit->set_error("failed to create MC backend or code emitter");
    return nullptr;
  }

  auto obj_writer = mab->createObjectWriter(obj_stream);

  auto streamer = std::unique_ptr<MCStreamer>(
      target->createMCObjectStreamer(
          TT, ctx, std::move(mab), std::move(obj_writer), std::move(mce), *sti));

  if (!streamer) {
    jit->set_error("failed to create object streamer");
    return nullptr;
  }

  auto parser =
      std::unique_ptr<MCAsmParser>(createMCAsmParser(sm, ctx, *streamer, *mai));
  auto tap = std::unique_ptr<MCTargetAsmParser>(
      target->createMCAsmParser(*sti, *parser, *mii, mc_opts));

  if (!parser || !tap) {
    jit->set_error("failed to create asm parser");
    return nullptr;
  }

  parser->setTargetParser(*tap);

  if (parser->Run(/*NoInitialTextSection=*/false, /*NoFinalize=*/false)) {
    // Prefer any lower-level ORC/LLVM session error if present, otherwise use
    // a direct parse/emit message.
    std::string high = "assembly parse/emit failed";
    jit->set_error(jit->combine_with_session_error(std::move(high)));
    return nullptr;
  }

  return MemoryBuffer::getMemBufferCopy(
      StringRef(obj_bytes.data(), obj_bytes.size()), "<a86-jit-object>");
}

static bool
set_jit_error_and_cleanup(a86_jit *jit,
                          a86_program *prog,
                          std::string high_level_error) {
  jit->set_error(jit->combine_with_session_error(std::move(high_level_error)));
  if (prog && prog->tracker) {
    consumeError(prog->tracker->remove());
    prog->tracker.reset();
  }
  return false;
}

} // namespace

extern "C" {

a86_jit_t *a86_jit_create(void) {
  auto jit = std::make_unique<a86_jit>();

  static bool llvm_initialized = false;
  if (!llvm_initialized) {
    InitializeNativeTarget();
    InitializeNativeTargetAsmParser();
    InitializeNativeTargetAsmPrinter();
    llvm_initialized = true;
  }

  auto jtmb_or_err = orc::JITTargetMachineBuilder::detectHost();
  if (!jtmb_or_err) {
    jit->set_error(toString(jtmb_or_err.takeError()));
    return nullptr;
  }

  auto dl_or_err = jtmb_or_err->getDefaultDataLayoutForTarget();
  if (!dl_or_err) {
    jit->set_error(toString(dl_or_err.takeError()));
    return nullptr;
  }

  auto lljit_or_err = orc::LLJITBuilder()
                          .setJITTargetMachineBuilder(std::move(*jtmb_or_err))
                          .setDataLayout(*dl_or_err)
                          .create();
  if (!lljit_or_err) {
    jit->set_error(toString(lljit_or_err.takeError()));
    return nullptr;
  }

  jit->jit = std::move(*lljit_or_err);

  // Capture lower-level ORC session errors instead of letting them escape only
  // to stderr.
  jit->jit->getExecutionSession().setErrorReporter(
      [raw = jit.get()](Error err) {
        raw->record_session_error(std::move(err));
      });

  // Let the main JITDylib resolve symbols from the current process. Programs
  // will also get their own generator when loaded.
  auto gen_or_err =
      orc::DynamicLibrarySearchGenerator::GetForCurrentProcess(
          jit->jit->getDataLayout().getGlobalPrefix());
  if (!gen_or_err) {
    jit->set_error(toString(gen_or_err.takeError()));
    return nullptr;
  }
  jit->jit->getMainJITDylib().addGenerator(std::move(*gen_or_err));

  return jit.release();
}

void a86_jit_destroy(a86_jit_t *jit) {
  delete jit;
}

const char *a86_jit_last_error(a86_jit_t *jit) {
  if (!jit) {
    return "invalid jit handle";
  }
  return jit->error_cstr();
}

a86_program_t *a86_jit_load(a86_jit_t *jit,
                            const char *asm_text,
                            const char *const *object_files,
                            int object_file_count,
                            const a86_extern_binding_t *externs,
                            int extern_count) {
  if (!jit || !jit->jit) {
    return nullptr;
  }

  jit->clear_error();
  jit->clear_session_error();

  if (!asm_text) {
    jit->set_error("asm_text is null");
    return nullptr;
  }
  if (object_file_count < 0) {
    jit->set_error("object_file_count is negative");
    return nullptr;
  }
  if (extern_count < 0) {
    jit->set_error("extern_count is negative");
    return nullptr;
  }
  if (object_file_count > 0 && !object_files) {
    jit->set_error("object_files is null but object_file_count > 0");
    return nullptr;
  }
  if (extern_count > 0 && !externs) {
    jit->set_error("externs is null but extern_count > 0");
    return nullptr;
  }

  auto prog = std::make_unique<a86_program>();
  prog->parent = jit;

  std::string jd_name = "a86_prog_" + std::to_string(NextProgramId++);
  auto jd_or_err = jit->jit->createJITDylib(jd_name);
  if (!jd_or_err) {
    jit->set_error(toString(jd_or_err.takeError()));
    return nullptr;
  }

  prog->jd = &*jd_or_err;
  prog->tracker = prog->jd->createResourceTracker();

  // Let this program's namespace resolve libc / current-process symbols too.
  auto gen_or_err =
      orc::DynamicLibrarySearchGenerator::GetForCurrentProcess(
          jit->jit->getDataLayout().getGlobalPrefix());
  if (!gen_or_err) {
    jit->set_error(toString(gen_or_err.takeError()));
    consumeError(prog->tracker->remove());
    return nullptr;
  }
  prog->jd->addGenerator(std::move(*gen_or_err));

  // 1. Install host-provided externs into this program's tracker.
  orc::SymbolMap symbol_map;
  for (int i = 0; i < extern_count; ++i) {
    const auto &b = externs[i];
    if (!b.name) {
      return set_jit_error_and_cleanup(
                 jit, prog.get(), "extern binding has null name"),
             nullptr;
    }

    auto sym_name = jit->jit->mangleAndIntern(b.name);

    // For both FUNCTION and GLOBAL, `value` is the address to bind to the
    // symbol. For GLOBAL, that address should point to storage.
    orc::ExecutorAddr addr = orc::ExecutorAddr::fromPtr(b.value);
    symbol_map[sym_name] =
        orc::ExecutorSymbolDef(addr, JITSymbolFlags::Exported);
  }

  if (!symbol_map.empty()) {
    if (auto err =
            prog->jd->define(orc::absoluteSymbols(std::move(symbol_map)),
                             prog->tracker)) {
      return set_jit_error_and_cleanup(
                 jit, prog.get(), toString(std::move(err))),
             nullptr;
    }
  }

  // 2. Add linked object files.
  for (int i = 0; i < object_file_count; ++i) {
    if (!object_files[i]) {
      return set_jit_error_and_cleanup(
                 jit, prog.get(), "object file path is null"),
             nullptr;
    }

    auto mb_or_err = MemoryBuffer::getFile(object_files[i]);
    if (!mb_or_err) {
      return set_jit_error_and_cleanup(
                 jit, prog.get(),
                 "failed to read object file " + std::string(object_files[i]) +
                     ": " + mb_or_err.getError().message()),
             nullptr;
    }

    if (auto err =
            jit->jit->addObjectFile(prog->tracker, std::move(*mb_or_err))) {
      return set_jit_error_and_cleanup(
                 jit, prog.get(),
                 "failed to add object file " + std::string(object_files[i]) +
                     ": " + toString(std::move(err))),
             nullptr;
    }
  }

  // 3. Assemble and add the a86 program itself.
  auto obj = assemble_to_object(jit, asm_text);
  if (!obj) {
    consumeError(prog->tracker->remove());
    return nullptr;
  }

  if (auto err = jit->jit->addObjectFile(prog->tracker, std::move(obj))) {
    return set_jit_error_and_cleanup(
               jit, prog.get(), toString(std::move(err))),
           nullptr;
  }

  return prog.release();
}

void a86_program_unload(a86_program_t *program) {
  if (!program) {
    return;
  }

  if (program->tracker) {
    consumeError(program->tracker->remove());
    program->tracker.reset();
  }

  delete program;
}

a86_call_result_t a86_program_call(a86_program_t *program,
                                   const char *label,
                                   const uint64_t *argv,
                                   int argc) {
  a86_call_result_t result{};
  result.ok = 0;
  result.value = 0;
  result.error_message = nullptr;

  if (!program || !program->parent || !program->parent->jit) {
    result.error_message = "invalid program handle";
    return result;
  }

  auto *jit = program->parent;
  jit->clear_error();
  jit->clear_session_error();

  if (!label) {
    jit->set_error("label is null");
    result.error_message = jit->error_cstr();
    return result;
  }

  if (argc < 0) {
    jit->set_error("argc is negative");
    result.error_message = jit->error_cstr();
    return result;
  }

  if (argc > 0 && !argv) {
    jit->set_error("argv is null but argc > 0");
    result.error_message = jit->error_cstr();
    return result;
  }

  auto sym_or_err = jit->jit->lookup(*program->jd, label);
  if (!sym_or_err) {
    std::string high =
        "lookup of label '" + std::string(label) +
        "' failed: " + toString(sym_or_err.takeError());
    jit->set_error(jit->combine_with_session_error(std::move(high)));
    result.error_message = jit->error_cstr();
    return result;
  }

  uint64_t value = 0;

  switch (argc) {
    case 0: {
      auto *fn = sym_or_err->toPtr<uint64_t (*)()>();
      value = fn();
      break;
    }
    case 1: {
      auto *fn = sym_or_err->toPtr<uint64_t (*)(uint64_t)>();
      value = fn(argv[0]);
      break;
    }
    case 2: {
      auto *fn = sym_or_err->toPtr<uint64_t (*)(uint64_t, uint64_t)>();
      value = fn(argv[0], argv[1]);
      break;
    }
    case 3: {
      auto *fn =
          sym_or_err->toPtr<uint64_t (*)(uint64_t, uint64_t, uint64_t)>();
      value = fn(argv[0], argv[1], argv[2]);
      break;
    }
    case 4: {
      auto *fn = sym_or_err
                     ->toPtr<uint64_t (*)(uint64_t, uint64_t, uint64_t, uint64_t)>();
      value = fn(argv[0], argv[1], argv[2], argv[3]);
      break;
    }
    case 5: {
      auto *fn = sym_or_err->toPtr<uint64_t (*)(uint64_t, uint64_t, uint64_t,
                                                uint64_t, uint64_t)>();
      value = fn(argv[0], argv[1], argv[2], argv[3], argv[4]);
      break;
    }
    case 6: {
      auto *fn = sym_or_err->toPtr<uint64_t (*)(uint64_t, uint64_t, uint64_t,
                                                uint64_t, uint64_t, uint64_t)>();
      value = fn(argv[0], argv[1], argv[2], argv[3], argv[4], argv[5]);
      break;
    }
    default:
      jit->set_error("a86_program_call currently supports at most 6 arguments");
      result.error_message = jit->error_cstr();
      return result;
  }

  result.ok = 1;
  result.value = value;
  result.error_message = nullptr;
  return result;
}

} // extern "C"
