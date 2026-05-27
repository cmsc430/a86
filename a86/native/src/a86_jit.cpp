#include "a86_jit.h"

#include <atomic>
#include <cstdarg>
#include <cstdlib>
#include <cstring>
#include <cstdio>
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

std::atomic<uint64_t> NextLoadId{0};

bool native_trace_enabled() {
  const char *v = std::getenv("A86_JIT_TRACE");
  if (!v) {
    return false;
  }
  return std::strcmp(v, "") != 0 &&
         std::strcmp(v, "0") != 0 &&
         std::strcmp(v, "false") != 0 &&
         std::strcmp(v, "FALSE") != 0 &&
         std::strcmp(v, "False") != 0;
}

void native_trace(const char *fmt, ...) {
  if (!native_trace_enabled()) {
    return;
  }

  std::fputs("a86-jit-native: ", stderr);
  va_list args;
  va_start(args, fmt);
  std::vfprintf(stderr, fmt, args);
  va_end(args);
  std::fputc('\n', stderr);
  std::fflush(stderr);
}

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
  orc::JITDylib *program_jd = nullptr;
  orc::JITDylib *extern_jd = nullptr;
  orc::ResourceTrackerSP program_tracker;
  orc::ResourceTrackerSP extern_tracker;
  uint64_t load_id = 0;
  std::string jd_name;
  std::string extern_jd_name;
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
                            const char *const *extern_names,
                            const int *extern_kinds,
                            void *const *extern_values,
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
  if (extern_count > 0 &&
      (!extern_names || !extern_kinds || !extern_values)) {
    jit->set_error("extern arrays are null but extern_count > 0");
    return nullptr;
  }

  auto obj = assemble_to_object(jit->errs.get(), asm_text);
  if (!obj) {
    return nullptr;
  }

  // Create a fresh JITDylib for this program - no pooling!
  uint64_t load_id = NextLoadId++;
  std::string jd_name = "a86_prog_" + std::to_string(load_id);
  native_trace("load id=%llu jd=%s extern_count=%d object_file_count=%d asm_bytes=%zu obj_bytes=%zu",
               static_cast<unsigned long long>(load_id),
               jd_name.c_str(),
               extern_count,
               object_file_count,
               std::strlen(asm_text),
               obj->getBufferSize());
  auto jd_or_err = jit->jit->createJITDylib(jd_name);
  if (!jd_or_err) {
    jit->set_error(toString(jd_or_err.takeError()));
    return nullptr;
  }
  orc::JITDylib *program_jd = &*jd_or_err;
  auto program_tracker = program_jd->createResourceTracker();
  std::string extern_jd_name = jd_name + "_externs";
  auto extern_jd_or_err = jit->jit->createJITDylib(extern_jd_name);
  if (!extern_jd_or_err) {
    jit->set_error(toString(extern_jd_or_err.takeError()));
    consumeError(program_tracker->remove());
    return nullptr;
  }
  orc::JITDylib *extern_jd = &*extern_jd_or_err;
  auto extern_tracker = extern_jd->createResourceTracker();
  extern_jd->addToLinkOrder(*jit->support_jd);
  program_jd->addToLinkOrder(*extern_jd);
  program_jd->addToLinkOrder(*jit->support_jd);

  auto cleanup = [&]() {
    if (program_tracker) {
      consumeError(program_tracker->remove());
      program_tracker.reset();
    }
    if (extern_tracker) {
      consumeError(extern_tracker->remove());
      extern_tracker.reset();
    }
    // No pooling - JITDylib will be cleaned up when tracker removes all resources
  };

  auto prog = std::make_unique<a86_program>();
  prog->parent = jit;
  prog->program_jd = program_jd;
  prog->extern_jd = extern_jd;
  prog->program_tracker = program_tracker;
  prog->extern_tracker = extern_tracker;
  prog->load_id = load_id;
  prog->jd_name = jd_name;
  prog->extern_jd_name = extern_jd_name;
  prog->externs.reserve(extern_count);

  // Copy host-provided externs into program-owned storage before defining
  // symbols so their full payload remains stable for the program lifetime.
  for (int i = 0; i < extern_count; ++i) {
    if (!extern_names[i]) {
      jit->set_error("extern binding has null name");
      cleanup();
      return nullptr;
    }
    prog->externs.push_back(
        extern_binding_copy{extern_names[i],
                            static_cast<a86_extern_kind_t>(extern_kinds[i]),
                            extern_values[i]});
  }

  // Install host-provided externs as absolute symbols.
  orc::SymbolMap symbol_map;
  for (const auto &ext : prog->externs) {
    native_trace("load id=%llu extern-jd=%s define extern name=%s kind=%d addr=%p",
                 static_cast<unsigned long long>(load_id),
                 extern_jd_name.c_str(),
                 ext.name.c_str(),
                 static_cast<int>(ext.kind),
                 ext.value);
    auto sym_name = jit->jit->mangleAndIntern(ext.name);
    orc::ExecutorAddr addr = orc::ExecutorAddr::fromPtr(ext.value);
    symbol_map[sym_name] = orc::ExecutorSymbolDef(addr, JITSymbolFlags::Exported);
  }

  if (!symbol_map.empty()) {
    native_trace("load id=%llu extern-jd=%s defining %zu absolute symbols",
                 static_cast<unsigned long long>(load_id),
                 extern_jd_name.c_str(),
                 symbol_map.size());
    if (auto err = extern_jd->define(orc::absoluteSymbols(std::move(symbol_map)),
                                     extern_tracker)) {
      jit->set_error(jit->combine_with_session_error(toString(std::move(err))));
      cleanup();
      return nullptr;
    }
  }

  // Add linked object files.
  for (int i = 0; i < object_file_count; ++i) {
    if (!object_files[i]) {
      jit->set_error("object file path is null");
      cleanup();
      return nullptr;
    }
    native_trace("load id=%llu jd=%s add object file path=%s",
                 static_cast<unsigned long long>(load_id),
                 jd_name.c_str(),
                 object_files[i]);
    auto mb_or_err = MemoryBuffer::getFile(object_files[i]);
    if (!mb_or_err) {
      jit->set_error(
          std::string("failed to read object file ") + object_files[i] + ": " +
          mb_or_err.getError().message());
      cleanup();
      return nullptr;
    }
    if (auto err = jit->jit->addObjectFile(program_tracker, std::move(*mb_or_err))) {
      jit->set_error(
          jit->combine_with_session_error(
              std::string("failed to add object file ") + object_files[i] + ": " +
              toString(std::move(err))));
      cleanup();
      return nullptr;
    }
  }

  // Add the assembled a86 program object.
  native_trace("load id=%llu jd=%s add assembled object bytes=%zu",
               static_cast<unsigned long long>(load_id),
               jd_name.c_str(),
               obj->getBufferSize());
  if (auto err = jit->jit->addObjectFile(program_tracker, std::move(obj))) {
    jit->set_error(jit->combine_with_session_error(toString(std::move(err))));
    cleanup();
    return nullptr;
  }

  prog->program_tracker = std::move(program_tracker);
  prog->extern_tracker = std::move(extern_tracker);
  native_trace("load id=%llu jd=%s complete",
               static_cast<unsigned long long>(load_id),
               prog->jd_name.c_str());
  return prog.release();
}

void a86_program_unload(a86_program_t *program) {
  if (program) {
    native_trace("unload id=%llu jd=%s extern-jd=%s",
                 static_cast<unsigned long long>(program->load_id),
                 program->jd_name.c_str(),
                 program->extern_jd_name.c_str());
    if (program->parent && program->parent->jit) {
      if (program->program_tracker) {
        consumeError(program->program_tracker->remove());
        program->program_tracker.reset();
      }
      if (program->extern_tracker) {
        consumeError(program->extern_tracker->remove());
        program->extern_tracker.reset();
      }
      // Do NOT return JITDylib to a pool - let ORC manage its lifecycle.
      program->program_jd = nullptr;
      program->extern_jd = nullptr;
    }
    delete program;
  }
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
      !program->program_jd) {
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

  native_trace("lookup id=%llu jd=%s label=%s argc=%d",
               static_cast<unsigned long long>(program->load_id),
               program->jd_name.c_str(),
               label,
               argc);
  auto sym_or_err = jit->jit->lookup(*program->program_jd, label);
  if (!sym_or_err) {
    native_trace("lookup id=%llu jd=%s label=%s failed",
                 static_cast<unsigned long long>(program->load_id),
                 program->jd_name.c_str(),
                 label);
    std::string high =
        "lookup of label '" + std::string(label) + "' failed: " +
        toString(sym_or_err.takeError());
    jit->set_error(jit->combine_with_session_error(std::move(high)));
    result.error_message = jit->error_cstr();
    return result;
  }

  native_trace("lookup id=%llu jd=%s label=%s addr=0x%llx",
               static_cast<unsigned long long>(program->load_id),
               program->jd_name.c_str(),
               label,
               static_cast<unsigned long long>(sym_or_err->getValue()));

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
      return result;
  }

  native_trace("call id=%llu jd=%s label=%s returned value=0x%llx",
               static_cast<unsigned long long>(program->load_id),
               program->jd_name.c_str(),
               label,
               static_cast<unsigned long long>(value));

  result.ok = 1;
  result.value = value;
  result.error_message = nullptr;
  return result;
}

}  // extern "C"
