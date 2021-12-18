TARGET_DIR=../../bin
TARGET=$(TARGET_DIR)/loop_1million

$(TARGET): loop_1million.hs
	@mkdir -p $(TARGET_DIR)
	ghc -O2 -o $@ $<

clean:
	rm -f *.hi *.o

distclean: clean
	rm -f $(TARGET)