module SSHKeys where

import Text.Parsec hiding (Line)

file = line `sepEndBy` newline <* eof

type Option = (String, Maybe String)

data Line
    = Entry ([Option], String, String, String)
    | Comment String
    | EmptyLine
    deriving (Eq, Show)

line = commentLine <|> emptyLine <|> entry

commentLine = Comment <$> (char '#' *> spaces' *> comment)

emptyLine = const EmptyLine <$> lookAhead newline

entry = do
    o <- options
    spaces'
    k <- kind
    spaces'
    h <- hash
    spaces'
    c <- comment
    return $ Entry (o, k, h, c)

options = do
    setting `sepBy` comma

setting = do
    notFollowedBy kind
    key <- anyChar `manyTill` lookAhead (space <|> comma <|> char '=')
    value <- optionMaybe $ do
        char '='
        between (char '"') (char '"') (anyChar `manyTill` lookAhead (char '"'))
            <|> anyChar `manyTill` lookAhead (space <|> comma)
    return (key, value)

kind = do
    base <- string "ssh" <|> string "ecdsa" <?> "key kind"
    char '-'
    rest <- anyChar `manyTill` lookAhead space
    return $ base ++ "-" ++ rest

hash = anyChar `manyTill` lookAhead space

comment = many $ notFollowedBy newline >> anyChar

spaces' = skipMany $ notFollowedBy newline >> space

comma = char ','

parseFile = runP file ()

testData =
    [ "ssh-dsa AAAAAAAA me@somewhere OK?\n"
    , "\n"
    , "opt1 ssh-dsa AAAAAAAA me@somewhere OK?\n"
    , "#  A comment line\n"
    , "opt1,opt2 ssh-dsa AAAAAAAA me@somewhere OK?\n"
    , "opt1=\"a value\",opt2 ssh-dsa AAAAAAAA me@somewhere OK?" ]

test = runP file () "testData" $ concat testData
