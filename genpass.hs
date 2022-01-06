#!/usr/bin/env stack
-- stack --resolver lts-18.21 script

import           Control.Monad       (replicateM)
import           Options.Applicative
import           System.Random.MWC

printables, alphabets, numbers, specials :: [Char]
printables = ['!' .. '~']
alphabets  = ['A' .. 'Z'] ++ ['a' .. 'z']
numbers    = ['0' .. '9']
specials   = "!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~"

data Argument = Argument
    { useAlphabets   :: Bool
    , useNumbers     :: Bool
    , useSpecials    :: Bool
    , passwordLength :: Int
    }

argumentParser :: Parser Argument
argumentParser = Argument
    <$> switch (short 'a' <> help "Use alphabets")
    <*> switch (short 'n' <> help "Use numbers")
    <*> switch (short 's' <> help "Use special characters")
    <*> argument auto (metavar "length" <> value 32
        <> help "Password length (Default: 32)")

argumentParserInfo :: ParserInfo Argument
argumentParserInfo = info (argumentParser <**> helper)
    $ progDesc "Generate a random password."

main :: IO ()
main = do
    arg <- execParser argumentParserInfo
    genpass arg >>= putStrLn

genpass :: Argument -> IO String
genpass arg = withSystemRandomST $ \g ->
    fmap (map (chars !!)) $
    replicateM n $ uniformRM (0, length chars - 1) g
  where
    n = passwordLength arg

    -- Default to all printable characters
    chars  = if null chars' then printables else chars'
    chars' =  (if useAlphabets arg then alphabets else "")
           ++ (if useNumbers arg then numbers else "")
           ++ (if useSpecials arg then specials else "")

