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
else
  SHARED_LDFLAGS := -shared
endif

LIB_NAME := liba86_jit$(SO_SUFFIX)
LIB_PATH := $(LIB_DIR)/$(LIB_NAME)

SRCS := $(SRC_DIR)/a86_jit.cpp
OBJS := $(BUILD_DIR)/a86_jit.o

CXXFLAGS := -std=c++17 -O2 -g -fPIC -I$(INC_DIR) $(shell $(LLVM_CONFIG) --cxxflags)
LDFLAGS := $(SHARED_LDFLAGS) \
	$(shell $(LLVM_CONFIG) --ldflags --system-libs --libs all --link-shared)

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
