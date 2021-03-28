TARGET=factorial

$(TARGET): $(TARGET).hs
	ghc -o $@ $<

clean:
	rm -f *.hi *.o

distclean: clean
	rm -f $(TARGET)