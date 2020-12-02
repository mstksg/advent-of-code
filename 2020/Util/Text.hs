module Util.Text where

import           Data.Text (Text)
import qualified Data.Text as T

infixl 9 !
(!) :: Text -> Int -> Char
(!) = T.index
