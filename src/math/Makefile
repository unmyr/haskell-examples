TARGET_DIR=../bin
TARGETS=\
$(TARGET_DIR)/collatz_with_guards \
$(TARGET_DIR)/collatz_with_pattern \
$(TARGET_DIR)/fac \
$(TARGET_DIR)/fib \
$(TARGET_DIR)/square_root_int \
$(TARGET_DIR)/squared

GHC_FLAGS=-Wall -fwarn-unused-do-bind

.PHONY: setup

all: setup $(TARGETS)

setup:
	mkdir -p $(TARGET_DIR)

$(TARGET_DIR)/collatz_with_guards: collatz_with_guards.hs
	ghc $(GHC_FLAGS) -o $@ $<

$(TARGET_DIR)/collatz_with_pattern: collatz_with_pattern.hs
	ghc $(GHC_FLAGS) -o $@ $<

$(TARGET_DIR)/fac: fac.hs
	ghc $(GHC_FLAGS) -o $@ $<

$(TARGET_DIR)/fib: fib.hs
	ghc $(GHC_FLAGS) -o $@ $<

$(TARGET_DIR)/square_root_int: square_root_int.hs
	ghc $(GHC_FLAGS) -o $@ $<

$(TARGET_DIR)/squared: squared.hs
	ghc $(GHC_FLAGS) -o $@ $<

clean:
	rm -f *.hi *.o

distclean: clean
	rm -f $(TARGETS)
