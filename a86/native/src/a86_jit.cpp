#include "a86_jit.h"

#include <atomic>
#include <cstdlib>
#include <cstring>
#include <memory>
#include <mutex>
#include <string>
#include <utility>
#include <vector>

#include "llvm/ADT/SmallVector.h"
#include "llvm/Config/llvm-config.h"
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

namespace {

std::atomic<uint64_t> NextCallId{0};

struct shared_error_state {
  mutable std::mutex mu;
  std::string last_error;
  std::string last_session_error;

  void clear() {
    std::lock_guard<std::mutex> lock(mu);
    last_error.clear();
    last_session_error.clear();
  }

  void clear_error_only() {
    std::lock_guard<std::mutex> lock(mu);
    last_error.clear();
  }

  void clear_session_error_only() {
    std::lock_guard<std::mutex> lock(mu);
    last_session_error.clear();
  }

  void set_error(std::string msg) {
    std::lock_guard<std::mutex> lock(mu);
    last_error = std::move(msg);
  }

  void record_session_error(Error err) {
    std::string msg = toString(std::move(err));
    std::lock_guard<std::mutex> lock(mu);
    if (last_session_error.empty()) {
      last_session_error = std::move(msg);
    } else {
      last_session_error += "\n";
      last_session_error += msg;
    }
  }

  std::string combine_with_session_error(std::string high_level) const {
    std::lock_guard<std::mutex> lock(mu);
    if (last_session_error.empty()) {
      return high_level;
    }
    if (high_level.empty()) {
      return last_session_error;
    }
    return last_session_error + "\n" + high_level;
  }

  const char *error_cstr_unsafe() const {
    return last_error.empty() ? nullptr : last_error.c_str();
  }
};

struct extern_binding_copy {
  std::string name;
  a86_extern_kind_t kind;
  void *value;
};

static std::unique_ptr<MemoryBuffer>
assemble_to_object(shared_error_state *errs, StringRef asm_text) {
  errs->clear_session_error_only();

  std::string triple_name = sys::getProcessTriple();
  Triple TT(triple_name);

  std::string lookup_err;
  const Target *target = TargetRegistry::lookupTarget(TT, lookup_err);
  if (!target) {
    errs->set_error("lookupTarget failed: " + lookup_err);
    return nullptr;
  }

  MCTargetOptions mc_opts;

#if LLVM_VERSION_MAJOR >= 22
  auto mri = std::unique_ptr<MCRegisterInfo>(target->createMCRegInfo(TT));
  auto mai =
      std::unique_ptr<MCAsmInfo>(target->createMCAsmInfo(*mri, TT, mc_opts));
  auto mii = std::unique_ptr<MCInstrInfo>(target->createMCInstrInfo());
#else
  auto mri =
      std::unique_ptr<MCRegisterInfo>(target->createMCRegInfo(triple_name));
  auto mai = std::unique_ptr<MCAsmInfo>(
      target->createMCAsmInfo(*mri, triple_name, mc_opts));
  auto mii = std::unique_ptr<MCInstrInfo>(target->createMCInstrInfo());
#endif

  std::string cpu = sys::getHostCPUName().str();
  auto host_features = sys::getHostCPUFeatures();
  SubtargetFeatures features;
  for (const auto &kv : host_features) {
    features.AddFeature(kv.getKey(), kv.getValue());
  }
  std::string feature_string = features.getString();

#if LLVM_VERSION_MAJOR >= 22
  auto sti = std::unique_ptr<MCSubtargetInfo>(
      target->createMCSubtargetInfo(TT, cpu, feature_string));
#else
  auto sti = std::unique_ptr<MCSubtargetInfo>(
      target->createMCSubtargetInfo(triple_name, cpu, feature_string));
#endif

  if (!mri || !mai || !mii || !sti) {
    errs->set_error("failed to create MC target components");
    return nullptr;
  }

  SourceMgr sm;
  sm.AddNewSourceBuffer(MemoryBuffer::getMemBufferCopy(asm_text, ""), SMLoc());

  MCContext ctx(TT, mai.get(), mri.get(), sti.get(), &sm, &mc_opts);

  auto mofi =
      std::unique_ptr<MCObjectFileInfo>(target->createMCObjectFileInfo(ctx, true));
  ctx.setObjectFileInfo(mofi.get());

  SmallVector<char, 0> obj_bytes;
  raw_svector_ostream obj_stream(obj_bytes);

  auto mab = std::unique_ptr<MCAsmBackend>(
      target->createMCAsmBackend(*sti, *mri, mc_opts));
  auto mce = std::unique_ptr<MCCodeEmitter>(target->createMCCodeEmitter(*mii, ctx));
  if (!mab || !mce) {
    errs->set_error("failed to create MC backend or code emitter");
    return nullptr;
  }

  auto obj_writer = mab->createObjectWriter(obj_stream);
  auto streamer = std::unique_ptr<MCStreamer>(target->createMCObjectStreamer(
      TT, ctx, std::move(mab), std::move(obj_writer), std::move(mce), *sti));
  if (!streamer) {
    errs->set_error("failed to create object streamer");
    return nullptr;
  }

  auto parser = std::unique_ptr<MCAsmParser>(createMCAsmParser(sm, ctx, *streamer, *mai));
  auto tap = std::unique_ptr<MCTargetAsmParser>(
      target->createMCAsmParser(*sti, *parser, *mii, mc_opts));
  if (!parser || !tap) {
    errs->set_error("failed to create asm parser");
    return nullptr;
  }

  parser->setTargetParser(*tap);

  if (parser->Run(/*NoInitialTextSection=*/false, /*NoFinalize=*/false)) {
    errs->set_error(errs->combine_with_session_error("assembly parse/emit failed"));
    return nullptr;
  }

  return MemoryBuffer::getMemBufferCopy(
      StringRef(obj_bytes.data(), obj_bytes.size()), "");
}

}  // namespace

struct a86_jit {
  std::unique_ptr<orc::LLJIT> jit;
  orc::JITDylib *support_jd = nullptr;
  std::shared_ptr<shared_error_state> errs = std::make_shared<shared_error_state>();

  void clear_error() { errs->clear_error_only(); }
  void clear_session_error() { errs->clear_session_error_only(); }
  void set_error(std::string msg) { errs->set_error(std::move(msg)); }

  const char *error_cstr() const {
    std::lock_guard<std::mutex> lock(errs->mu);
    return errs->error_cstr_unsafe();
  }

  std::string combine_with_session_error(std::string high_level) const {
    return errs->combine_with_session_error(std::move(high_level));
  }
};

struct a86_program {
  a86_jit *parent = nullptr;
  std::string assembled_object_bytes;
  std::vector<std::string> object_files;
  std::vector<extern_binding_copy> externs;
};

extern "C" {

a86_jit_t *a86_jit_create(void) {
  auto jit = std::make_unique<a86_jit>();

  static std::once_flag llvm_initialized;
  std::call_once(llvm_initialized, [] {
    InitializeNativeTarget();
    InitializeNativeTargetAsmParser();
    InitializeNativeTargetAsmPrinter();
  });

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

  {
    auto errs = jit->errs;
    jit->jit->getExecutionSession().setErrorReporter(
        [errs](Error err) {
          errs->record_session_error(std::move(err));
        });
  }

  auto support_jd_or_err = jit->jit->createJITDylib("a86_support");
  if (!support_jd_or_err) {
    jit->set_error(toString(support_jd_or_err.takeError()));
    return nullptr;
  }

  jit->support_jd = &*support_jd_or_err;

  auto gen_or_err = orc::DynamicLibrarySearchGenerator::GetForCurrentProcess(
      jit->jit->getDataLayout().getGlobalPrefix());
  if (!gen_or_err) {
    jit->set_error(toString(gen_or_err.takeError()));
    return nullptr;
  }

  jit->support_jd->addGenerator(std::move(*gen_or_err));
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
  if (!jit || !jit->jit || !jit->support_jd) {
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

  for (int i = 0; i < object_file_count; ++i) {
    if (!object_files[i]) {
      jit->set_error("object file path is null");
      return nullptr;
    }
    prog->object_files.emplace_back(object_files[i]);
  }

  for (int i = 0; i < extern_count; ++i) {
    if (!externs[i].name) {
      jit->set_error("extern binding has null name");
      return nullptr;
    }
    prog->externs.push_back(extern_binding_copy{
        std::string(externs[i].name),
        externs[i].kind,
        externs[i].value});
  }

  auto obj = assemble_to_object(jit->errs.get(), asm_text);
  if (!obj) {
    return nullptr;
  }

  prog->assembled_object_bytes.assign(obj->getBufferStart(), obj->getBufferEnd());
  return prog.release();
}

void a86_program_unload(a86_program_t *program) {
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

  if (!program || !program->parent || !program->parent->jit ||
      !program->parent->support_jd) {
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

  std::string jd_name = "a86_call_" + std::to_string(NextCallId++);
  auto jd_or_err = jit->jit->createJITDylib(jd_name);
  if (!jd_or_err) {
    jit->set_error(toString(jd_or_err.takeError()));
    result.error_message = jit->error_cstr();
    return result;
  }

  orc::JITDylib *call_jd = &*jd_or_err;
  auto tracker = call_jd->createResourceTracker();
  call_jd->addToLinkOrder(*jit->support_jd);

  auto cleanup = [&]() {
    if (tracker) {
      consumeError(tracker->remove());
      tracker.reset();
    }
  };

  // 1. Install host-provided externs into this call's tracker.
  orc::SymbolMap symbol_map;
  for (const auto &b : program->externs) {
    auto sym_name = jit->jit->mangleAndIntern(b.name);
    orc::ExecutorAddr addr = orc::ExecutorAddr::fromPtr(b.value);
    symbol_map[sym_name] = orc::ExecutorSymbolDef(addr, JITSymbolFlags::Exported);
  }

  if (!symbol_map.empty()) {
    if (auto err = call_jd->define(orc::absoluteSymbols(std::move(symbol_map)),
                                   tracker)) {
      jit->set_error(jit->combine_with_session_error(toString(std::move(err))));
      result.error_message = jit->error_cstr();
      cleanup();
      return result;
    }
  }

  // 2. Add linked object files.
  for (const auto &path : program->object_files) {
    auto mb_or_err = MemoryBuffer::getFile(path);
    if (!mb_or_err) {
      jit->set_error(
          "failed to read object file " + path + ": " +
          mb_or_err.getError().message());
      result.error_message = jit->error_cstr();
      cleanup();
      return result;
    }

    if (auto err = jit->jit->addObjectFile(tracker, std::move(*mb_or_err))) {
      jit->set_error(
          jit->combine_with_session_error(
              "failed to add object file " + path + ": " +
              toString(std::move(err))));
      result.error_message = jit->error_cstr();
      cleanup();
      return result;
    }
  }

  // 3. Add the preassembled a86 program object.
  auto obj = MemoryBuffer::getMemBufferCopy(
      StringRef(program->assembled_object_bytes.data(),
                program->assembled_object_bytes.size()),
      "a86_program.o");

  if (auto err = jit->jit->addObjectFile(tracker, std::move(obj))) {
    jit->set_error(
        jit->combine_with_session_error(toString(std::move(err))));
    result.error_message = jit->error_cstr();
    cleanup();
    return result;
  }

  // 4. Lookup and call within the fresh per-call program dylib.
  auto sym_or_err = jit->jit->lookup(*call_jd, label);
  if (!sym_or_err) {
    std::string high =
        "lookup of label '" + std::string(label) + "' failed: " +
        toString(sym_or_err.takeError());
    jit->set_error(jit->combine_with_session_error(std::move(high)));
    result.error_message = jit->error_cstr();
    cleanup();
    return result;
  }

  uint64_t value = 0;
  switch (argc) {
    case 0: {
      using Fn = uint64_t (*)();
      auto *fn = sym_or_err->toPtr<Fn>();
      value = fn();
      break;
    }
    case 1: {
      using Fn = uint64_t (*)(uint64_t);
      auto *fn = sym_or_err->toPtr<Fn>();
      value = fn(argv[0]);
      break;
    }
    case 2: {
      using Fn = uint64_t (*)(uint64_t, uint64_t);
      auto *fn = sym_or_err->toPtr<Fn>();
      value = fn(argv[0], argv[1]);
      break;
    }
    case 3: {
      using Fn = uint64_t (*)(uint64_t, uint64_t, uint64_t);
      auto *fn = sym_or_err->toPtr<Fn>();
      value = fn(argv[0], argv[1], argv[2]);
      break;
    }
    case 4: {
      using Fn = uint64_t (*)(uint64_t, uint64_t, uint64_t, uint64_t);
      auto *fn = sym_or_err->toPtr<Fn>();
      value = fn(argv[0], argv[1], argv[2], argv[3]);
      break;
    }
    case 5: {
      using Fn =
          uint64_t (*)(uint64_t, uint64_t, uint64_t, uint64_t, uint64_t);
      auto *fn = sym_or_err->toPtr<Fn>();
      value = fn(argv[0], argv[1], argv[2], argv[3], argv[4]);
      break;
    }
    case 6: {
      using Fn =
          uint64_t (*)(uint64_t, uint64_t, uint64_t, uint64_t, uint64_t,
                       uint64_t);
      auto *fn = sym_or_err->toPtr<Fn>();
      value = fn(argv[0], argv[1], argv[2], argv[3], argv[4], argv[5]);
      break;
    }
    default:
      jit->set_error("a86_program_call currently supports at most 6 arguments");
      result.error_message = jit->error_cstr();
      cleanup();
      return result;
  }

  cleanup();
  result.ok = 1;
  result.value = value;
  result.error_message = nullptr;
  return result;
}

}  // extern "C"
