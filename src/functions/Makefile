TARGET_DIR=../bin
TARGETS=\
$(TARGET_DIR)/add_2arg_int \
$(TARGET_DIR)/add_2arg_num \
$(TARGET_DIR)/add_2arg_tuple \
$(TARGET_DIR)/circle_area \
$(TARGET_DIR)/func_args_different_types \
$(TARGET_DIR)/func_int_to_list \
$(TARGET_DIR)/higher_order_args \
$(TARGET_DIR)/higher_order_tuple \
$(TARGET_DIR)/make_tuple_various_types

GHC_FLAGS=-Wall -fwarn-unused-do-bind

.PHONY: setup

all: setup $(TARGETS)

setup:
	mkdir -p $(TARGET_DIR)

$(TARGET_DIR)/add_2arg_int: add_2arg_int.hs
	ghc $(GHC_FLAGS) -o $@ $<

$(TARGET_DIR)/add_2arg_num: add_2arg_num.hs
	ghc $(GHC_FLAGS) -o $@ $<

$(TARGET_DIR)/add_2arg_tuple: add_2arg_tuple.hs
	ghc $(GHC_FLAGS) -o $@ $<

$(TARGET_DIR)/circle_area: circle_area.hs
	ghc $(GHC_FLAGS) -o $@ $<

$(TARGET_DIR)/func_args_different_types: func_args_different_types.hs
	ghc $(GHC_FLAGS) -o $@ $<

$(TARGET_DIR)/func_int_to_list: func_int_to_list.hs
	ghc $(GHC_FLAGS) -o $@ $<

$(TARGET_DIR)/higher_order_args: higher_order_args.hs
	ghc $(GHC_FLAGS) -o $@ $<

$(TARGET_DIR)/higher_order_tuple: higher_order_tuple.hs
	ghc $(GHC_FLAGS) -o $@ $<

$(TARGET_DIR)/make_tuple_various_types: make_tuple_various_types.hs
	ghc $(GHC_FLAGS) -o $@ $<

clean:
	rm -f *.hi *.o

distclean: clean
	rm -f $(TARGETS)
