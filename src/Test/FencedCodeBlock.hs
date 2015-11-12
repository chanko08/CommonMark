{-# LANGUAGE OverloadedStrings #-}
module Test.FencedCodeBlock where
import Test.Hspec
import Test.QuickCheck
import Control.Applicative

import IndentedCodeBlock
import FencedCodeBlock (fencedCodeBlock, FencedCodeBlock(..))
import Data.Text ( Text )
import qualified Data.Text as Text
import Test.Utils
import Utils

fence fenceType num = Text.pack <$> vectorOf num (pure fenceType)

formText indentation numLines = return ()

-- any length string that doesn't contain a newline or a backtick
infoString = Text.strip . Text.pack <$> listOf (arbitrary `suchThat` (\c -> c /= '`'))

atLeast n g = (++) <$> vectorOf n g <*> listOf g

fenceBlock = do
    fence <- (elements ["~", "`"]) :: Gen Text
    startIndent <- elements ["", " ", "  ", "   "]

    startFence <- atLeast 3 (pure fence)
    endFence   <- atLeast (length startFence) (pure fence)

    let start = Text.append startIndent (Text.pack startFence)

    return ()

fencedCodeBlockTestUnit = do
    fenceType <- elements "~`"
    numberOfFences <- positiveNum
    extraFences <- nonNegativeNum
    indentationLevel <- elements ["", " ", "  ", "   "]
    
    
    startFence <- Text.append indentationLevel <$> fence fenceType numberOfFences
    endFence   <- Text.append indentationLevel <$> fence fenceType (numberOfFences + extraFences)

    info <- infoString

    let validTextInput = Text.concat [startFence, info, endFence]

    return $ TestUnit startFence (FencedCodeBlock info "")

test = describe "Fenced Code Block" $
    it "parses any arbitrary markdown Fenced Code Block" $ property $
        forAll fencedCodeBlockTestUnit $ givesResult fencedCodeBlock