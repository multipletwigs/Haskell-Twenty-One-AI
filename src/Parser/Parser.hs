-- | Parser combinator.

module Parser.Parser where

import           Parser.Instances

-- | Produces a parser that always fails with 'UnexpectedChar' using the given
-- character.
unexpectedCharParser :: Char -> Parser a
unexpectedCharParser = P . const . Error . UnexpectedChar

-- | Return a parser that always fails with the given error.
failed :: ParseError -> Parser a
failed = P . const . Error

-- | Return a parser that produces the given character but fails if:
--   * the input is empty; or
--   * the produced character is not equal to the given character.
-- >>> parse (is 'c') "c"
-- Result >< 'c'
-- >>> isErrorResult (parse (is 'c') "")
-- True
-- >>> isErrorResult (parse (is 'c') "b")
-- True
is :: Char -> Parser Char
is c = do
    v <- character
    let next = if v == c then pure else const $ unexpectedCharParser v
    next c

-- | Return a parser that succeeds with a character off the input or fails with
-- an error if the input is empty.
-- >>> parse character "abc"
-- Result >bc< 'a'
-- >>> isErrorResult (parse character "")
-- True
character :: Parser Char
character = P parseit
  where
    parseit ""      = Error UnexpectedEof
    parseit (c : s) = Result s c

-- | Return a parser that tries the first parser for a successful value, then:
--   * if the first parser succeeds then use this parser; or
--   * if the first parser fails, try the second parser.
--
-- >>> parse (character ||| pure 'v') ""
-- Result >< 'v'
-- >>> parse (failed UnexpectedEof ||| pure 'v') ""
-- Result >< 'v'
-- >>> parse (character ||| pure 'v') "abc"
-- Result >bc< 'a'
-- >>> parse (failed UnexpectedEof ||| pure 'v') "abc"
-- Result >abc< 'v'
(|||) :: Parser a -> Parser a -> Parser a
p1 ||| p2 = P
    (\i ->
        let f (Error _) = parse p2 i
            f r         = r
        in  f $ parse p1 i
    )

-- chain p op parses 1 or more instances of p
-- separated by op
-- (see chainl1 from Text.Parsec)
chain :: Parser a -> Parser (a -> a -> a) -> Parser a
chain p op = p >>= rest
  where
    rest a =
        (do
                f <- op
                b <- p
                rest (f a b)
            )
            ||| pure a
            
-- | Parses the suit of the card
-- >>> parse parseCardSuit "HT"
-- Result >T< "H"
--
-- >>> parse parseCardSuit "S2"
-- Result >2< "S"
--
-- >>> parse parseCardSuit "D9"
-- Result >9< "D"

parseCardSuit :: Parser Char
parseCardSuit = is 'S' ||| is 'H' ||| is 'D' ||| is 'C'

-- | Parses a value of a card
-- >>> parse parseCardValue "1"
-- Result >< "1"
parseCardRank :: Parser Char
parseCardRank = is '1' ||| is '2' ||| is '3' ||| 
                 is '4' ||| is '5' ||| is '6' ||| 
                 is '7' ||| is '8' ||| is '9' ||| 
                 is 'T' ||| is 'J' ||| is 'K' ||| is 'Q'

-- | Parses a card 
-- >>> parse parseOnlyRank "HT"
-- Result >< "T"
--
-- >>> parse parseOnlyRank "H2"
-- Result >< "2"
--
-- >>> parse parseOnlyRank "DK"
-- Result >< "K"
parseOnlyRank :: Parser Char
parseOnlyRank = parseCardSuit >> parseCardRank

list :: Parser a -> Parser [a]
list p1 = list1 p1 ||| pure []
list1 p = p >>= (\p' -> list p >>= (\p''-> pure (p':p'')))

sepby1 :: Parser a -> Parser s -> Parser [a]
sepby1 p1 sep = p1 >>= \i -> (list (sep >> p1) >>= \i' -> pure(i:i'))

-- | Parse card array
-- >>> parse parseCardArray "[HT,H2,D1]"
-- Result >< "T21"
parseCardArray :: Parser String
parseCardArray = is '[' >> sepby1 parseOnlyRank (is ',') <* is ']'
