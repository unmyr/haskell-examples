TARGET=sum
GHC_FLAGS=-O

$(TARGET): $(TARGET).hs
	ghc $(GHC_FLAGS) -o $@ $<

clean:
	rm -f *.hi *.o

distclean: clean
	rm -f $(TARGET)
