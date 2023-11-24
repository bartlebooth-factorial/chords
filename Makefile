chords: main.hs chords.hs parser.hs translation.hs
	ghc main.hs -o chords

clean:
	rm *.hi *.o chords

.PHONY: clean
