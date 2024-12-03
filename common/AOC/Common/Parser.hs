module AOC.Common.Parser (
  -- * Parsers
  TokStream (..),
  parseTokStream,
  parseTokStream_,
  parseTokStreamT,
  parseTokStreamT_,
  TokParser,
  parseWords,
  nextMatch,
  parseMaybeLenient,
  parseMaybe',
  parseOrFail,
  CharParser,
  pWord,
  pHWord,
  pAlphaWord,
  pAlphaNumWord,
  pDecimal,
  pTok,
  pTokMany,
  pSpace,
  parseLines,
  sepBy',
  sepByLines,
  sepBy1',
  sequenceSepBy,
  manyTillWithout,
  someTillWithout,
  pDropUntil,
  between',
  tokenMap,
  tokenAssoc,
  fullLine,
  optionalEnd,
) where

import AOC.Util
import Control.Applicative
import Control.DeepSeq (NFData)
import Control.Monad
import qualified Control.Monad.Combinators as P
import qualified Control.Monad.Combinators.NonEmpty as PNE
import Data.Bifunctor
import Data.Char
import Data.Coerce
import Data.Foldable
import Data.Functor.Identity
import Data.Hashable (Hashable)
import qualified Data.List as L
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified Data.Set as S
import Data.Void
import GHC.Generics (Generic)
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import Data.Traversable
import qualified Text.Megaparsec.Char.Lexer as PL

-- | Use a stream of tokens @a@ as the underlying parser stream.  Note that
-- error messages for parser errors are going necessarily to be wonky.
newtype TokStream a = TokStream {getTokStream :: [a]}
  deriving stock (Ord, Eq, Show, Generic, Functor)

instance Hashable a => Hashable (TokStream a)

instance NFData a => NFData (TokStream a)

instance (Ord a, Show a) => P.Stream (TokStream a) where
  type Token (TokStream a) = a
  type Tokens (TokStream a) = Seq a

  tokensToChunk _ = Seq.fromList
  chunkToTokens _ = toList
  chunkLength _ = Seq.length
  take1_ = coerce . L.uncons . getTokStream
  takeN_ n (TokStream xs) =
    bimap Seq.fromList TokStream (splitAt n xs)
      <$ guard (not (null xs))
  takeWhile_ p = bimap Seq.fromList TokStream . span p . getTokStream

-- | Parse a stream of tokens @s@ purely, returning 'Either'
parseTokStream ::
  Foldable t =>
  P.Parsec e (TokStream s) a ->
  t s ->
  Either (P.ParseErrorBundle (TokStream s) e) a
parseTokStream p = runIdentity . parseTokStreamT p

-- | Parse a stream of tokens @s@ purely
parseTokStream_ ::
  (Alternative m, Foldable t) =>
  P.Parsec e (TokStream s) a ->
  t s ->
  m a
parseTokStream_ p = runIdentity . parseTokStreamT_ p

-- | Parse a stream of tokens @s@ over an underlying monad, returning 'Either'
parseTokStreamT ::
  (Foldable t, Monad m) =>
  P.ParsecT e (TokStream s) m a ->
  t s ->
  m (Either (P.ParseErrorBundle (TokStream s) e) a)
parseTokStreamT p = P.runParserT p "" . TokStream . toList

-- | Parse a stream of tokens @s@ over an underlying monad
parseTokStreamT_ ::
  (Alternative f, Foldable t, Monad m) =>
  P.ParsecT e (TokStream s) m a ->
  t s ->
  m (f a)
parseTokStreamT_ p = fmap eitherToMaybe . parseTokStreamT p

type CharParser = P.Parsec Void String

pWord :: (P.Stream s, P.Token s ~ Char, Ord e) => P.Parsec e s String
pWord = pTok $ P.many (P.satisfy (not . isSpace))

pAlphaWord :: (P.Stream s, P.Token s ~ Char, Ord e) => P.Parsec e s String
pAlphaWord = pTok $ P.many P.letterChar

pAlphaNumWord :: (P.Stream s, P.Token s ~ Char, Ord e) => P.Parsec e s String
pAlphaNumWord = pTok $ P.many P.alphaNumChar

pHWord :: (P.Stream s, P.Token s ~ Char, Ord e) => P.Parsec e s String
pHWord = P.many (P.satisfy (not . isSpace)) <* P.many (P.satisfy (== ' '))

pDecimal :: forall a e s. (P.Stream s, P.Token s ~ Char, Ord e, Num a) => P.Parsec e s a
pDecimal = pTok $ PL.signed P.space PL.decimal

pTok :: (P.Stream s, P.Token s ~ Char, Ord e) => P.Parsec e s a -> P.Parsec e s a
pTok p = pSpace *> p <* pSpace

pTokMany :: (P.Stream s, P.Token s ~ Char, Ord e) => P.Parsec e s a -> P.Parsec e s [a]
pTokMany = pTok . P.many

pSpace :: (P.Stream s, P.Token s ~ Char, Ord e) => P.Parsec e s ()
pSpace = P.skipMany (P.char ' ')

parseMaybeLenient :: P.Parsec Void s a -> s -> Maybe a
parseMaybeLenient p = eitherToMaybe . P.parse p "parseMaybeLenient"

pDropUntil :: (P.Stream s, Ord e) => P.Parsec e s end -> P.Parsec e s end
pDropUntil = P.try . P.skipManyTill P.anySingle . P.try

-- | Alias for 'parseMaybeLenient'
parseMaybe' :: P.Parsec Void s a -> s -> Maybe a
parseMaybe' = parseMaybeLenient

parseOrFail ::
  (P.ShowErrorComponent e, P.VisualStream s, P.TraversableStream s) => P.Parsec e s a -> s -> a
parseOrFail p = either (error . P.errorBundlePretty) id . P.parse p "parseMaybeLenient"

parseLines :: P.Parsec Void String a -> String -> Maybe [a]
parseLines p = Just . mapMaybe (parseMaybeLenient p) . lines

parseWords :: P.Parsec Void (TokStream String) a -> String -> Maybe a
parseWords p = parseMaybeLenient p . TokStream . words

-- | 'sepBy' but automatically exclude the separator from the internal parser
sepBy' :: (P.Stream s, Ord e) => P.Parsec e s a -> P.Parsec e s sep -> P.Parsec e s [a]
sepBy' x sep = P.sepBy (P.notFollowedBy sep *> P.try x) sep

sepByLines :: (P.Stream s, Ord e, P.Token s ~ Char) => P.Parsec e s a -> P.Parsec e s [a]
sepByLines = flip sepBy' (P.char '\n')

-- | 'PNE.sepBy1' but automatically exclude the separator from the internal parser
sepBy1' :: (P.Stream s, Ord e) => P.Parsec e s a -> P.Parsec e s sep -> P.Parsec e s (NonEmpty a)
sepBy1' x sep = PNE.sepBy1 (P.notFollowedBy sep *> P.try x) sep

sequenceSepBy :: (Traversable t, P.Stream s, Ord e) => t (P.Parsec e s a) -> P.Parsec e s sep -> P.Parsec e s (t a)
sequenceSepBy xs sep = sequenceA . snd $ mapAccumR go False xs
  where
    go addSep x = (True, if addSep then x' <* sep else x')
      where
        x' = P.notFollowedBy sep *> P.try x

optionalEnd :: (P.Stream s, Ord e) => P.Parsec e s a -> P.Parsec e s end -> P.Parsec e s a
optionalEnd x end = P.try x <* P.optional (P.try end)

-- | 'manyTill' but do not parse the end token
manyTillWithout :: (P.Stream s, Ord e) => P.Parsec e s a -> P.Parsec e s end -> P.Parsec e s [a]
manyTillWithout x end = P.many (P.notFollowedBy end *> P.try x)

-- | 'someTill' but do not parse the end token
someTillWithout :: (P.Stream s, Ord e) => P.Parsec e s a -> P.Parsec e s end -> P.Parsec e s (NonEmpty a)
someTillWithout x end = PNE.some (P.notFollowedBy end *> P.try x)

-- | 'between' but automatically exclude the separator from the internal parser. does this make sense?
between' ::
  (P.Stream s, Ord e) => P.Parsec e s open -> P.Parsec e s close -> P.Parsec e s a -> P.Parsec e s a
between' open close x = P.between open close (P.notFollowedBy close *> P.try x)

tokenMap :: (P.Stream s, Ord e) => Map (P.Token s) a -> P.Parsec e s a
tokenMap mp = P.token (`M.lookup` mp) (S.map (P.Tokens . (:| [])) $ M.keysSet mp)

tokenAssoc :: (P.Stream s, Ord e) => [(P.Token s, a)] -> P.Parsec e s a
tokenAssoc = tokenMap . M.fromList

fullLine :: (P.Stream s, P.Token s ~ Char, Ord e) => P.Parsec e s a -> P.Parsec e s a
fullLine p = optionalEnd p P.newline

type TokParser s = P.Parsec Void (TokStream s)

-- | Skip every result until this token matches
nextMatch :: P.MonadParsec e s m => m a -> m a
nextMatch = P.try . fmap snd . P.manyTill_ (P.try P.anySingle)
