TARGET=digits_sum_has_nth_roots
CFLAGS=-O2

$(TARGET): $(TARGET).hs
	ghc $(CFLAGS) -o $@ $<

clean:
	rm -f *.hi *.o

distclean: clean
	rm -f $(TARGET)