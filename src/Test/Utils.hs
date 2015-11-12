module Test.Utils (givesResult, positiveNum, nonNegativeNum, TestUnit(..), lineOfText) where
import Test.QuickCheck
import Control.Applicative

import Data.Text (Text)
import qualified Data.Either as Either
import qualified Data.Attoparsec.Text as Attoparsec

data TestUnit a = TestUnit { testText :: Text, expectedOutput :: a } deriving (Show, Eq)

positiveNum :: Gen Int
positiveNum = unwrap <$> arbitrary
    where
        unwrap (Positive x) = x


nonNegativeNum :: Gen Int
nonNegativeNum = subtract 1 <$> positiveNum

lineOfText = listOf1 (arbitrary :: Gen Char) `suchThat` all notEndOflineOrTab
    where notEndOflineOrTab x = not (Attoparsec.isEndOfLine x) && not (x == '\t')

givesResult :: (Eq a) => Attoparsec.Parser a -> TestUnit a -> Bool
givesResult p unit = Either.either (const False) (== expectedOutput unit) result
    where
        result = Attoparsec.parseOnly (p <* Attoparsec.endOfInput) $ testText unit

