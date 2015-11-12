{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
module Test.HorizontalRule (test) where
import Test.Hspec
import Test.QuickCheck
import Control.Applicative

import Data.Text                                    ( Text )
import qualified Data.Text            as Text       ( append, concat, replicate, intercalate )
import qualified Data.List            as List       ( intersperse )
import qualified Data.Either          as Either     ( isRight, either )
import qualified Data.Attoparsec.Text as Attoparsec ( parseOnly, endOfInput )

import HorizontalRule
import Test.Utils

horizontalRuleTestUnit = do
    let makeSpace n = Text.replicate n " "
        
    -- anywhere from 0 to 3 spaces in the beginning
    beginningSpace <- makeSpace <$> elements [0, 1, 2, 3]

    -- any number of symbols greater than 3
    numSymbols <- (+2) <$> positiveNum

    -- any nonnegative number of spaces after each of those symbols (+1 to numSymbols to include the very end of line)
    trailingSpaceList <- map makeSpace <$> vectorOf (numSymbols + 1) nonNegativeNum

    let spaceList = beginningSpace:trailingSpaceList

    -- horizontal rules are composed of either '*', '-' or '_'
    ruleType <- elements ["*", "-", "_"]
    rule <- flip Text.intercalate spaceList <$> elements ["*", "-", "_"]
            
    return $ TestUnit (Text.append rule "\n") HorizontalRule

test = describe "Horizontal Rule" $
    it "parses any arbitrary markdown horizontal rule" $ property $
        forAll horizontalRuleTestUnit $ givesResult horizontalRule