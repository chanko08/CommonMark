{-# LANGUAGE OverloadedStrings #-}
module Test.IndentedCodeBlock where
import Test.Hspec
import Test.QuickCheck
import Control.Applicative

import IndentedCodeBlock
import Data.Text                                    ( Text )
import qualified Data.Text as Text
import Test.Utils

indentedCodeBlockTestUnit = do
    lines <- map (flip Text.append "\n" . Text.pack) <$> listOf1 lineOfText

    let validTextInput = Text.concat . map (Text.append "    ") $ lines
        expectedOutput = IndentedCodeBlock $ Text.concat lines

    return $ TestUnit validTextInput expectedOutput

test =  describe "Indented Code Block" $
    it "parses any arbitrary markdown indented code block" $ property $
        forAll indentedCodeBlockTestUnit $ givesResult indentedCodeBlock
