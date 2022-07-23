TARGET=factorial

$(TARGET): $(TARGET).hs
	ghc -Wall -o $@ $<

clean:
	rm -f *.hi *.o

distclean: clean
	rm -f $(TARGET)
