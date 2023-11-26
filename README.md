# Introduction

`chords` is a command-line utility to identify chord possibilities from a series
of notes. It takes a list of notes as string input, interpreted in
ascending order, and reports all possible chords that it can match the notes
against.

# Installation and Usage

To build `chords`, one will need a recent version of GHC. Clone this repository
and run `make` in the cloned directory to compile the `chords` executable.

Try out the program by running commands such as these.

```
> ./chords "C E G"
> ./chords "Bb D F A"
> ./chords "G# A C E"
```

# Notes

`chords` is an early work-in-progress. It currently supports only exact matches
against a small set of chord patterns. The project aspires both to include a
comprehensive list of commonly known chord patterns, and to output best-guess
responses for unknown inputs that would align with the judgment of an
experienced musician.
