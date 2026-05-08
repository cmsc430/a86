CXX := clang++
LLVM_CONFIG ?= llvm-config

SRC_DIR := src
INC_DIR := include
BUILD_DIR := build
LIB_DIR := lib

UNAME_S := $(shell uname -s)
SO_SUFFIX := $(shell racket -e '(display (system-type (quote so-suffix)))' 2>/dev/null || echo .so)

ifeq ($(UNAME_S),Darwin)
  SHARED_LDFLAGS := -dynamiclib
  LLVM_LINK_MODE := --link-static
  SHARED_LDFLAGS += -Wl,-dead_strip -Wl,-dead_strip_dylibs
  LLVM_SYSTEM_LIBS := /usr/local/lib/libzstd.a -lz -lxml2
else
  SHARED_LDFLAGS := -shared
  LLVM_LINK_MODE := --link-static
  LLVM_SYSTEM_LIBS := $(shell $(LLVM_CONFIG) --system-libs)
endif

LIB_NAME := liba86_jit$(SO_SUFFIX)
LIB_PATH := $(LIB_DIR)/$(LIB_NAME)

SRCS := $(SRC_DIR)/a86_jit.cpp
OBJS := $(BUILD_DIR)/a86_jit.o

CXXFLAGS := -std=c++17 -O2 -g -fPIC -I$(INC_DIR) $(shell $(LLVM_CONFIG) --cxxflags)
LLVM_COMPONENTS := orcjit executionengine runtimedyld native mcparser asmparser mc object targetparser support core
LDFLAGS := $(SHARED_LDFLAGS) \
	$(shell $(LLVM_CONFIG) --ldflags) \
	$(shell $(LLVM_CONFIG) --libs $(LLVM_COMPONENTS) $(LLVM_LINK_MODE)) \
	$(LLVM_SYSTEM_LIBS)

.PHONY: all clean dirs

all: $(LIB_PATH)

dirs:
	mkdir -p $(BUILD_DIR) $(LIB_DIR)

$(BUILD_DIR)/a86_jit.o: $(SRC_DIR)/a86_jit.cpp $(INC_DIR)/a86_jit.h | dirs
	$(CXX) $(CXXFLAGS) -c $< -o $@

$(LIB_PATH): $(OBJS) | dirs
	$(CXX) $(OBJS) $(LDFLAGS) -o $@

clean:
	rm -rf $(BUILD_DIR) $(LIB_DIR)
