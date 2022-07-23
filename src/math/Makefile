GHC_FLAGS=-Wall

TARGET_DIR=bin
TARGETS=\
$(TARGET_DIR)/collatz_with_guards \
$(TARGET_DIR)/collatz_with_pattern \
$(TARGET_DIR)/fac \
$(TARGET_DIR)/fibonacci_sequence \
$(TARGET_DIR)/square_root_int

.PHONY: setup

all: setup $(TARGETS)

setup:
	mkdir -p bin

$(TARGET_DIR)/collatz_with_guards: collatz_with_guards.hs
	ghc $(GHC_FLAGS) -o $@ $<

$(TARGET_DIR)/collatz_with_pattern: collatz_with_pattern.hs
	ghc $(GHC_FLAGS) -o $@ $<

$(TARGET_DIR)/fac: fac.hs
	ghc $(GHC_FLAGS) -o $@ $<

$(TARGET_DIR)/fibonacci_sequence: fibonacci_sequence.hs
	ghc $(GHC_FLAGS) -o $@ $<

$(TARGET_DIR)/square_root_int: square_root_int.hs
	ghc $(GHC_FLAGS) -o $@ $<

clean:
	rm -f *.hi *.o
	rm -f $(TARGETS)