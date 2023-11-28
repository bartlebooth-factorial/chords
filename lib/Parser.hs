module Parser
  ( Note
  , parseNotes
  ) where

import Text.Parsec
import Text.Parsec.String

{-
notes       ::= note { ' ' note }+
note        ::= noteName (accidentals)
noteName    ::= 'A' | 'B' | 'C' | 'D' | 'E' | 'F' | 'G'
accidentals ::= sharps | flats
sharps      ::= { '#' }+
flats       ::= { 'b' }+
-}

type Note = (Char, Int)

note :: Parser Note
note = do { noteName <- oneOf ['A', 'B', 'C', 'D', 'E', 'F', 'G']
          ; do { accidentals <- many1 (char '#') <|> many1 (char 'b')
               ; case accidentals of
                   '#' : sharps ->
                     return (noteName,   length accidentals)
                   'b' : flats ->
                     return (noteName, - length accidentals) }
            <|> return (noteName, 0) }

notes :: Parser [Note]
notes = do { spaces
           ; ns <- sepEndBy1 note spaces
           ; eof
           ; return ns }

parseNotes :: String -> Either ParseError [Note]
parseNotes = parse notes ""

