TARGET_DIR=../../bin
TARGETS=\
$(TARGET_DIR)/group_o2_c2_using_int \
$(TARGET_DIR)/group_o2_c2_using_monad \
$(TARGET_DIR)/group_o3_c3 \
$(TARGET_DIR)/group_o4_k4_with_tuple \
$(TARGET_DIR)/group_o4_k4_with_data \
$(TARGET_DIR)/group_o6_d3 \
$(TARGET_DIR)/group_o8_q8_c \
$(TARGET_DIR)/group_o8_q8_nc \
$(TARGET_DIR)/group_o8_q8_nc_debug

GHC_FLAGS=-Wall -fwarn-unused-do-bind

.PHONY: setup

all: setup $(TARGETS)

setup:
	mkdir -p $(TARGET_DIR)

$(TARGET_DIR)/group_o2_c2_using_int: group_o2_c2_using_int.hs
	ghc $(GHC_FLAGS) -o $@ $<

$(TARGET_DIR)/group_o2_c2_using_monad: group_o2_c2_using_monad.hs
	ghc $(GHC_FLAGS) -o $@ $<

$(TARGET_DIR)/group_o3_c3: group_o3_c3.hs
	ghc $(GHC_FLAGS) -o $@ $<

$(TARGET_DIR)/group_o4_k4_with_tuple: group_o4_k4_with_tuple.hs
	ghc $(GHC_FLAGS) -o $@ $<

$(TARGET_DIR)/group_o4_k4_with_data: group_o4_k4_with_data.hs
	ghc $(GHC_FLAGS) -o $@ $<

$(TARGET_DIR)/group_o6_d3: group_o6_d3.hs
	ghc $(GHC_FLAGS) -o $@ $<

$(TARGET_DIR)/group_o8_q8_c: group_o8_q8_c.hs
	ghc $(GHC_FLAGS) -o $@ $<

$(TARGET_DIR)/group_o8_q8_nc: group_o8_q8_nc.hs
	ghc $(GHC_FLAGS) -o $@ $<

$(TARGET_DIR)/group_o8_q8_nc_debug: group_o8_q8_nc_debug.hs
	ghc $(GHC_FLAGS) -o $@ $<

clean:
	rm -f *.hi *.o

distclean: clean
	rm -f $(TARGETS)
