#include "a86_jit.h"

#include <cstdint>
#include <memory>
#include <string>
#include <utility>
#include <vector>
#include <unordered_map>

#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/StringMap.h"
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

class A86Jit {
public:
  A86Jit() = default;
  ~A86Jit() = default;

  bool init() {
    static bool llvmInitialized = false;
    if (!llvmInitialized) {
      InitializeNativeTarget();
      InitializeNativeTargetAsmParser();
      InitializeNativeTargetAsmPrinter();
      llvmInitialized = true;
    }

    TripleName_ = sys::getProcessTriple();
    TT_ = Triple(TripleName_);

    std::string err;
    Target_ = TargetRegistry::lookupTarget(TT_, err);
    if (!Target_) {
      setError("lookupTarget failed: " + err);
      return false;
    }

    CPU_ = sys::getHostCPUName().str();

    auto hostFeatures = sys::getHostCPUFeatures();
    SubtargetFeatures features;
    for (const auto &kv : hostFeatures) {
      features.AddFeature(kv.getKey(), kv.getValue());
    }
    Features_ = features.getString();

    auto jtmbOrErr = orc::JITTargetMachineBuilder::detectHost();
    if (!jtmbOrErr) {
      setError(toString(jtmbOrErr.takeError()));
      return false;
    }

    auto jitOrErr = orc::LLJITBuilder()
			.setJITTargetMachineBuilder(std::move(*jtmbOrErr))
			.create();
    if (!jitOrErr) {
      setError(toString(jitOrErr.takeError()));
      return false;
    }

    Jit_ = std::move(*jitOrErr);

    auto genOrErr =
	orc::DynamicLibrarySearchGenerator::GetForCurrentProcess(
	    Jit_->getDataLayout().getGlobalPrefix());
    if (!genOrErr) {
      setError(toString(genOrErr.takeError()));
      return false;
    }

    Jit_->getMainJITDylib().addGenerator(std::move(*genOrErr));
    return true;
  }

  void clearError() {
    LastError_.clear();
  }

  const char *lastError() const {
    return LastError_.empty() ? nullptr : LastError_.c_str();
  }

  bool defineSymbol(const char *name, void *addr) {
    if (!name) {
      setError("defineSymbol: name is null");
      return false;
    }
    Symbols_[std::string(name)] = addr;
    return true;
  }

  bool clearSymbols() {
    Symbols_.clear();
    return true;
  }

  bool setGlobal(const char *name, void *value) {
    if (!name) {
      setError("setGlobal: name is null");
      return false;
    }
    Globals_[std::string(name)] = value;
    return true;
  }

  bool clearGlobals() {
    Globals_.clear();
    return true;
  }

  bool patchGlobals(orc::JITDylib &jd) {
    for (const auto &[name, value] : Globals_) {
      auto symOrErr = Jit_->lookup(jd, name);
      if (!symOrErr) {
        consumeError(symOrErr.takeError());
        continue;
      }

      auto *slot = symOrErr->toPtr<void **>();
      *slot = value;
    }

    return true;
  }

  bool addObjectFilePath(const char *path) {
    if (!path) {
      setError("addObjectFilePath: path is null");
      return false;
    }
    ObjectFiles_.push_back(path);
    return true;
  }

  bool clearObjectFiles() {
    ObjectFiles_.clear();
    return true;
  }

  a86_jit_result_t run(const char *asmText, const char *entryName, void *heap) {
    clearError();
    a86_jit_result_t result{};

    if (LastRunTracker_) {
      if (auto err = LastRunTracker_->remove()) {
        setError(toString(std::move(err)));
        LastRunTracker_.reset();

        result.ok = 0;
        result.value = 0;
        result.error_message = lastError();
        return result;
      }
      LastRunTracker_.reset();
    }

    result.ok = 0;
    result.value = 0;
    result.error_message = nullptr;

    if (!asmText) {
      setError("run: asm_text is null");
      result.error_message = lastError();
      return result;
    }

    if (!entryName) {
      setError("run: entry_name is null");
      result.error_message = lastError();
      return result;
    }

    auto obj = assembleObject(asmText);
    if (!obj) {
      result.error_message = lastError();
      return result;
    }

    auto &jd = Jit_->getMainJITDylib();
    auto tracker = jd.createResourceTracker();

    // 1. install absolute symbols into THIS jd / THIS tracker
    if (!installSymbols(jd, tracker)) {
      consumeError(tracker->remove());
      result.error_message = lastError();
      return result;
    }

    // 2. add all current-objs into THIS tracker
    for (const auto &path : ObjectFiles_) {
      auto mbOrErr = MemoryBuffer::getFile(path);
      if (!mbOrErr) {
	setError("failed to read object file " + path + ": " +
		 mbOrErr.getError().message());
	consumeError(tracker->remove());
	result.error_message = lastError();
	return result;
      }

      if (auto err = Jit_->addObjectFile(tracker, std::move(*mbOrErr))) {
	setError("failed to add object file " + path + ": " +
		 toString(std::move(err)));
	consumeError(tracker->remove());
	result.error_message = lastError();
	return result;
      }
    }

    // 3. add the assembled student object into THIS tracker
    if (auto err = Jit_->addObjectFile(tracker, std::move(obj))) {
      setError(toString(std::move(err)));
      consumeError(tracker->remove());
      result.error_message = lastError();
      return result;
    }

    // 4. patch globals in linked objects
    if (!patchGlobals(jd)) {
      consumeError(tracker->remove());
      result.error_message = lastError();
      return result;
    }

    // 5. lookup entry in THIS jd explicitly
    auto symOrErr = Jit_->lookup(jd, entryName);
    if (!symOrErr) {
      setError(toString(symOrErr.takeError()));
      consumeError(tracker->remove());
      result.error_message = lastError();
      return result;
    }

    auto *fn = symOrErr->toPtr<int64_t (*)(void *)>();
    int64_t value = fn(heap);

    // Keep this run alive until the next run starts, so returned pointers
    // into static object data remain valid while the caller decodes them.
    LastRunTracker_ = tracker;

    result.ok = 1;
    result.value = value;
    result.error_message = nullptr;
    return result;
  }

private:
  std::unique_ptr<MemoryBuffer> assembleObject(StringRef asmText) {
    MCTargetOptions mcOpts;

    auto mri = std::unique_ptr<MCRegisterInfo>(Target_->createMCRegInfo(TT_));
    auto mai =
	std::unique_ptr<MCAsmInfo>(Target_->createMCAsmInfo(*mri, TT_, mcOpts));
    auto mii = std::unique_ptr<MCInstrInfo>(Target_->createMCInstrInfo());
    auto sti = std::unique_ptr<MCSubtargetInfo>(
	Target_->createMCSubtargetInfo(TT_, CPU_, Features_));

    if (!mri || !mai || !mii || !sti) {
      setError("failed to create MC target components");
      return nullptr;
    }

    SourceMgr sm;
    sm.AddNewSourceBuffer(
	MemoryBuffer::getMemBufferCopy(asmText, "<a86-jit-asm>"), SMLoc());

    MCContext ctx(TT_, mai.get(), mri.get(), sti.get(), &sm, &mcOpts);

    auto mofi =
	std::unique_ptr<MCObjectFileInfo>(Target_->createMCObjectFileInfo(ctx, true));
    ctx.setObjectFileInfo(mofi.get());

    SmallVector<char, 0> objBytes;
    raw_svector_ostream objOS(objBytes);

    auto mab = std::unique_ptr<MCAsmBackend>(
	Target_->createMCAsmBackend(*sti, *mri, mcOpts));
    auto mce =
	std::unique_ptr<MCCodeEmitter>(Target_->createMCCodeEmitter(*mii, ctx));

    if (!mab || !mce) {
      setError("failed to create MC backend or code emitter");
      return nullptr;
    }

    auto ow = mab->createObjectWriter(objOS);

    auto streamer = std::unique_ptr<MCStreamer>(
	Target_->createMCObjectStreamer(
	    TT_, ctx, std::move(mab), std::move(ow), std::move(mce), *sti));

    if (!streamer) {
      setError("failed to create object streamer");
      return nullptr;
    }

    auto parser =
	std::unique_ptr<MCAsmParser>(createMCAsmParser(sm, ctx, *streamer, *mai));
    auto tap = std::unique_ptr<MCTargetAsmParser>(
	Target_->createMCAsmParser(*sti, *parser, *mii, mcOpts));

    if (!parser || !tap) {
      setError("failed to create asm parser");
      return nullptr;
    }

    parser->setTargetParser(*tap);

    if (parser->Run(false, false)) {
      setError("assembly parse/emit failed");
      return nullptr;
    }

    return MemoryBuffer::getMemBufferCopy(
	StringRef(objBytes.data(), objBytes.size()), "<a86-jit-object>");
  }

  bool installSymbols(orc::JITDylib &jd, orc::ResourceTrackerSP tracker) {
    if (Symbols_.empty()) {
      return true;
    }

    orc::SymbolMap symbolMap;
    auto &es = Jit_->getExecutionSession();
    auto prefix = Jit_->getDataLayout().getGlobalPrefix();

    for (const auto &[name, addr] : Symbols_) {
      auto mangled = es.intern(mangle(prefix, name));
      auto jitAddr = orc::ExecutorAddr::fromPtr(addr);
      symbolMap[mangled] =
	  orc::ExecutorSymbolDef(jitAddr, JITSymbolFlags::Exported);
    }

    if (auto err = jd.define(orc::absoluteSymbols(std::move(symbolMap)), tracker)) {
      setError(toString(std::move(err)));
      return false;
    }

    return true;
  }

  static std::string mangle(char prefix, const std::string &name) {
    if (prefix == '\0') {
      return name;
    }
    return std::string(1, prefix) + name;
  }

  void setError(std::string msg) {
    LastError_ = std::move(msg);
  }

  std::string TripleName_;
  Triple TT_;
  const Target *Target_ = nullptr;
  std::string CPU_;
  std::string Features_;
  std::unique_ptr<orc::LLJIT> Jit_;
  std::string LastError_;
  std::unordered_map<std::string, void *> Symbols_;
  std::unordered_map<std::string, void *> Globals_;
  std::vector<std::string> ObjectFiles_;
  orc::ResourceTrackerSP LastRunTracker_;
};

}  // namespace

struct a86_jit {
  std::unique_ptr<A86Jit> impl;
};

extern "C" {

a86_jit_t *a86_jit_create(void) {
  auto *jit = new a86_jit_t;
  jit->impl = std::make_unique<A86Jit>();
  if (!jit->impl->init()) {
    delete jit;
    return nullptr;
  }
  return jit;
}

void a86_jit_destroy(a86_jit_t *jit) {
  delete jit;
}

void a86_jit_clear_error(a86_jit_t *jit) {
  if (!jit || !jit->impl) {
    return;
  }
  jit->impl->clearError();
}

const char *a86_jit_last_error(a86_jit_t *jit) {
  if (!jit || !jit->impl) {
    return "invalid jit handle";
  }
  return jit->impl->lastError();
}

int a86_jit_define_symbol(a86_jit_t *jit, const char *name, void *addr) {
  if (!jit || !jit->impl) {
    return 0;
  }
  return jit->impl->defineSymbol(name, addr) ? 1 : 0;
}

int a86_jit_clear_symbols(a86_jit_t *jit) {
  if (!jit || !jit->impl) {
    return 0;
  }
  return jit->impl->clearSymbols() ? 1 : 0;
}

int a86_jit_set_global(a86_jit_t *jit, const char *name, void *value) {
  if (!jit || !jit->impl) {
    return 0;
  }
  return jit->impl->setGlobal(name, value) ? 1 : 0;
}

int a86_jit_clear_globals(a86_jit_t *jit) {
  if (!jit || !jit->impl) {
    return 0;
  }
  return jit->impl->clearGlobals() ? 1 : 0;
}

int a86_jit_add_object_file(a86_jit_t *jit, const char *path) {
  if (!jit || !jit->impl) {
    return 0;
  }
  return jit->impl->addObjectFilePath(path) ? 1 : 0;
}

int a86_jit_clear_object_files(a86_jit_t *jit) {
  if (!jit || !jit->impl) {
    return 0;
  }
  return jit->impl->clearObjectFiles() ? 1 : 0;
}

a86_jit_result_t a86_jit_run(a86_jit_t *jit,
			     const char *asm_text,
			     const char *entry_name,
			     void *heap) {
  a86_jit_result_t result{};
  result.ok = 0;
  result.value = 0;
  result.error_message = "invalid jit handle";

  if (!jit || !jit->impl) {
    return result;
  }

  return jit->impl->run(asm_text, entry_name, heap);
}

}  // extern "C"
