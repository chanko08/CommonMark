{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
module Test.SetextHeader(test) where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.List as List

import Test.Hspec
import Test.QuickCheck


import qualified Data.Either as Either (either)
import Control.Applicative
import Data.Monoid

import qualified Data.Attoparsec.Text as Attoparsec (parseOnly, endOfInput, isEndOfLine)

import SetextHeader
import Test.Utils



lineOfTextNoSpaceInFront = lineOfText `suchThat` (\(c:cs) -> c /= ' ')


setextHeaderTestUnit = do
    beginningSpace <- flip Text.replicate " " <$> elements [0, 1, 2, 3]
    text           <- Text.pack <$> lineOfTextNoSpaceInFront

    headerType     <- elements [1,2]

    let toHeader 1 = "="
        toHeader 2 = "-"
        toHeader _ = error "invalid header level given for setext header"

    headerLine  <- flip Text.replicate (toHeader headerType)  <$> positiveNum

    let newline = "\n"
        testText = beginningSpace <> text <> newline <> headerLine <> newline
        testAnswer = SetextHeader headerType text

    return $ TestUnit testText testAnswer

test =  describe "Setext Header" $
    it "parses any arbitrary markdown Setext header" $ property $
        forAll setextHeaderTestUnit $ givesResult setextHeader
