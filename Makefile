chords: Main.hs Chords.hs Parser.hs Translation.hs
	ghc Main.hs -o chords

clean:
	rm *.hi *.o chords

.PHONY: clean
