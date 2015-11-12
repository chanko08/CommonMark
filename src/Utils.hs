{-# LANGUAGE OverloadedStrings #-}
module Utils where


import Control.Applicative
import Data.Text (Text)
import qualified Data.Text as Text

import Data.Maybe (fromMaybe)

import Data.Attoparsec.Text hiding (space, skipSpace)
requiredSpace = many1 space >> return ()
optionalSpace = skipSpace

space = string " "
skipSpace = skipWhile (== ' ')

notSpace = do
    c <- peekChar
    let isSpace = fromMaybe False $ (==' ') <$> c

    if isSpace
        then fail "found unexpected space"
        else return ()
        

parseLessThan n p
    | n > 0     = choice . map (flip count p) $ [n, n-1 .. 0]
    | otherwise = return []



parseAtLeast n p
    | n > 0     = (++) <$> count n p <*> many p
    | otherwise = return []


optionalSpaceIndentation = do
    parseLessThan 3 space
    return ()



cleanText = id



ltrim txt trimmer = ltrim' txt trimmer
    where
        ltrim' [] _          = txt
        ltrim' x  []         = x
        ltrim' (s:ss) (t:tt) = if s == t then ltrim' ss tt else txt

rtrim txt trimmer = reverse . flip ltrim trimmer . reverse $ txt

trim txt trimmer = flip rtrim trimmer $ ltrim txt trimmer

atMost' :: (Alternative m, Monad m) => Int -> m a -> m [a]
atMost' n p
    | n <= 0 = return []
    | otherwise = do
        res <- Just <$> p <|> return Nothing
                
        case res of
            Nothing -> return []
            Just x  -> fmap (x:) (atMost' (n-1) p)

atMost :: Int -> Parser a -> Parser [a]
atMost n p
        | n <= 0    = return []
        | otherwise = do
                res <- Just <$> p <|> return Nothing
                
                case res of
                    Nothing -> return []
                    Just x  -> fmap (x:) (atMost (n-1) p)
