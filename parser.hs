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
notes = do { ns <- sepEndBy1 note spaces -- (skipMany1 space)
           ; eof
           ; return ns }

-- for internal testing
run :: Show a => Parser a -> String -> IO ()
run p input =
  case parse p "" input of
    Left err -> do { putStr "parse error at "
                   ; print err }
    Right x  -> print x

-- for use in =main=
readNotes :: String -> String
readNotes input =
  case parse notes "" input of
    Left err -> show err
    Right result -> show result

parseNotes :: String -> Either ParseError [Note]
parseNotes noteString = parse notes "" noteString

