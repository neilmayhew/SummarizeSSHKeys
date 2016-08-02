{-# LANGUAGE CPP #-}

module SSHKeys where

import QuietTesting

import Control.Applicative ((<*), (<*>), (*>), (<$>))
import Data.Char
import Test.HUnit
import Text.Parsec hiding (Line)
import Text.Parsec.Error

#if !MIN_VERSION_parsec(3,1,9)
instance Eq ParseError where
    l == r = errorPos l == errorPos r && errorMessages l == errorMessages r
#endif

file = line `sepEndBy` newline <* eof

type Option = (String, Maybe String)

data Line
    = Entry ([Option], String, String, String)
    | Comment String
    | EmptyLine
    deriving (Eq, Show)

line = commentLine <|> emptyLine <|> entry

commentLine = Comment <$> try (spaces' *> char '#' *> comment)

emptyLine = const EmptyLine <$> try (spaces' *> lookAhead newline)

entry = do
    o <- options
    spaces'
    k <- kind
    spaces'
    h <- hash
    spaces'
    c <- comment
    return $ Entry (o, k, h, c)

options = setting `sepBy` comma

setting = do
    notFollowedBy kind
    key <- many1 (alphaNum <|> char '-') <?> "option name"
    value <- optionMaybe $ do
        char '='
        between (char '"') (char '"') (anyChar `manyTill` lookAhead (char '"'))
            <|> anyChar `manyTill` lookAhead (space <|> comma)
    return (key, value)

kind = do
    base <- string "ssh" <|> string "ecdsa"
    char '-'
    rest <- many1 nonSpace
    return $ base ++ "-" ++ rest
  <?> "key kind"

hash = many1 base64 <?> "base64 hash"

comment = many nonNewline <?> "comment"

base64 = satisfy (\c -> isAsciiUpper c || isAsciiLower c || isDigit c || c `elem` "+/=") <?> "base64 character"

nonNewline = noneOf "\r\n" <?> "character"
nonSpace = satisfy (not . isSpace) <?> "non-space"

space' = oneOf " \t" <?> "space"
spaces' = many space' <?> "spaces"

comma = char ','

manyTill1 p end = (:) <$> p <*> manyTill p end

parseFile = runP file ()

testData = concat
    [ "ssh-dsa AAAAAAAA me@somewhere OK?\n"
    , " \n"
    , "opt-1 ssh-dsa AAAAAAAA me@somewhere OK?\n"
    , " #  A comment line\n"
    , "opt-1,opt2 ssh-dsa AAAAAAAA me@somewhere OK?\n"
    , "opt-1=\"a value\",opt2 ssh-dsa AAAAAAAA me@somewhere OK?" ]

testResult = Right
    [ Entry ([], "ssh-dsa","AAAAAAAA","me@somewhere OK?")
    , EmptyLine
    , Entry ([("opt-1",Nothing)], "ssh-dsa","AAAAAAAA","me@somewhere OK?")
    , Comment "  A comment line"
    , Entry ([("opt-1",Nothing),("opt2",Nothing)], "ssh-dsa","AAAAAAAA","me@somewhere OK?")
    , Entry ([("opt-1",Just "a value"),("opt2",Nothing)], "ssh-dsa","AAAAAAAA","me@somewhere OK?") ]

tests = test
    [ parseFile "testData" testData ~?= testResult ]

runTests = runTestTTquiet tests
