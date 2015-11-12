{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
module Main where
import Test.Hspec
import Test.QuickCheck
import Data.Text (Text)

import qualified Test.HorizontalRule    as HorizontalRule
import qualified Test.AtxHeader         as AtxHeader
import qualified Test.SetextHeader      as SetextHeader
import qualified Test.IndentedCodeBlock as IndentedCodeBlock
import qualified Test.FencedCodeBlock   as FencedCodeBlock


import SetextHeader as S
import AtxHeader as A
import IndentedCodeBlock as I

tests = hspec $ do
    HorizontalRule.test
    AtxHeader.test
    SetextHeader.test
    IndentedCodeBlock.test
    --FencedCodeBlock.test


main :: IO ()
main = tests
--main = print A.test2