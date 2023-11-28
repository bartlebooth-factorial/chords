# Introduction

`chords` is a command-line utility to identify chord possibilities from a series
of notes. It takes a list of notes as string input, interpreted in
ascending order, and reports all possible chords that it can match the notes
against.

# Installation and Usage

To build `chords`, one will need a recent version of GHC along with the Cabal
build system. Both of these tools can be installed through `ghcup`, which can be
found [here](https://www.haskell.org/ghcup/).

To build the project, clone this repository and run `cabal build` in the cloned
directory. Then try out the program by running commands such as these:

```
> cabal run chords "C E G"
> cabal run chords "Bb D F A"
> cabal run chords "G# A C E"
```

If you then want to install the `chords` binary on your system, run `cabal
install` which will put the executable in `~/.cabal/bin`.

# Notes

`chords` is an early work-in-progress. It currently supports only exact matches
against a small set of chord patterns. The project aspires both to include a
comprehensive list of commonly known chord patterns, and to output best-guess
responses for unknown inputs that would align with the judgment of an
experienced musician.
