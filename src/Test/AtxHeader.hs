{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
module Test.AtxHeader(test) where
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.List as List

import Test.Hspec
import Test.QuickCheck


import qualified Data.Either as Either
import Control.Applicative
import Data.Monoid

import qualified Data.Attoparsec.Text as Attoparsec

import AtxHeader
import Test.Utils

data AtxHeaderTest = AtxHeaderTest Text AtxHeader deriving Show

{-
Creates arbitrary valid ATX headers in markdown syntax
-}
atxHeaderTestUnit = do
        -- anywhere from 0 to 3 spaces in the beginning
        beginningSpace <- mkText " " <$> elements [0, 1, 2, 3]
        --atx headers have 1-6 '#' signs in the front
        headerLevel    <- elements [1,2,3,4,5,6]
        let leadingSigns = mkText "#" headerLevel
        --atx headers require 1 space after signs, then any amount after
        afterSignSpace <- mkText " " <$> positiveNum
        --should be able to handle arbitrary space before ending signs
        trailingSpace  <- mkText " " <$> nonNegativeNum
        --atx headers can have any number of trailing '#' signs
        trailingSigns  <- mkText "#" <$> nonNegativeNum

        trailingSignsPreSpace <- mkText " " <$> positiveNum

        text           <- Text.strip . Text.pack <$> lineOfText
        let newline = "\n"
            headerText = Text.concat
                [ beginningSpace
                , leadingSigns
                , afterSignSpace
                , text
                , trailingSignsPreSpace
                , trailingSigns
                , trailingSpace
                , newline
                ]
            headerAnswer = AtxHeader headerLevel text
        return $ TestUnit headerText headerAnswer

        where
            mkText = flip Text.replicate


test = describe "Atx Header" $
    it "parses any arbitrary markdown Atx Header" $ property $
        forAll atxHeaderTestUnit $ givesResult atxHeader