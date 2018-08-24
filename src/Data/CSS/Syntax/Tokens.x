-- -*- mode: haskell -*-
{

{-# LANGUAGE OverloadedStrings, BangPatterns #-}

module Data.CSS.Syntax.Tokens
    ( Token(..)
    , NumericValue(..)
    , HashFlag(..)
    , Unit

    , tokenize
    , serialize
    ) where


import           Control.Applicative
import           Control.Monad

import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B
import           Data.Monoid
import           Data.Char
import           Data.Scientific
import           Numeric
import           Data.Word
import           Foreign.Ptr
import           Foreign.ForeignPtr
import           Foreign.Storable
import           System.IO.Unsafe

import           Prelude

}

-- %wrapper "posn-bytestring"

$digit = 0-9
$letter = [a-zA-Z]
$whitespace = [\ \n\t]
$hex = [0-9a-fA-F]
$any = [\x00-\x10ffff]

$nameStartCodePoint = [$letter _ \x80-\x10ffff]
$nameCodePoint = [$nameStartCodePoint $digit \-]
$sign = [\+\-]

@escapedCodePoint = $hex{1,6} $whitespace ?
@escapedCodePoint' = \\ @escapedCodePoint
@name = \- ? ( $nameCodePoint | @escapedCodePoint' ) +

@numericValue = $sign? (($digit+ (\. $digit+)?) | (\. $digit+)) ([eE] $sign? $digit+)?


tokens :-

  "/*" $any* "*/"                       ; -- comment
  $whitespace+				{ \ _ -> Whitespace }
  "<!--"                                { \ _ -> CDO }
  "-->"                                 { \ _ -> CDC }
  "-->"                                 { \ _ -> CDC }
  ","                                   { \ _ -> Comma }
  ":"                                   { \ _ -> Colon }
  ";"                                   { \ _ -> Semicolon }
  "("                                   { \ _ -> LeftParen }
  ")"                                   { \ _ -> RightParen }
  "["                                   { \ _ -> LeftSquareBracket }
  "]"                                   { \ _ -> RightSquareBracket }
  "{"                                   { \ _ -> LeftCurlyBracket }
  "}"                                   { \ _ -> RightCurlyBracket }
  "$="                                  { \ _ -> SuffixMatch }
  "*="                                  { \ _ -> SubstringMatch }
  "^="                                  { \ _ -> PrefixMatch }
  "|="                                  { \ _ -> DashMatch }
  "~="                                  { \ _ -> IncludeMatch }
  "||"                                  { \ _ -> Column }

--     , parseNumeric

--     , parseEscapedIdentLike
--     , parseIdentLike
  "#" @name                             { \ n -> Hash HId (T.tail n) }

--     , parseString '"'
--     , parseString '\''

  "@" @name                             { \ n -> AtKeyword (T.tail n) }

  $any                                  { \ t -> Delim (T.head t) }

{

data Token
    = Whitespace

    | CDO -- CommentDelimiterOpen
    | CDC -- CommentDelimiterClose

    | Comma
    | Colon
    | Semicolon

    | LeftParen
    | RightParen
    | LeftSquareBracket
    | RightSquareBracket
    | LeftCurlyBracket
    | RightCurlyBracket

    | SuffixMatch
    | SubstringMatch
    | PrefixMatch
    | DashMatch
    | IncludeMatch

    | Column

    | String !Char !Text
    | BadString !Char !Text

    | Number !Text !NumericValue
    | Percentage !Text !NumericValue
    | Dimension !Text !NumericValue !Unit

    | Url !Text
    | BadUrl !Text

    | Ident !Text

    | AtKeyword !Text

    | Function !Text

    | Hash !HashFlag !Text

    | Delim !Char

    deriving (Show, Eq)


data NumericValue
    = NVInteger !Scientific
    | NVNumber !Scientific
    deriving (Show, Eq)

data HashFlag = HId | HUnrestricted
    deriving (Show, Eq)

type Unit = Text

-- Alex wrapper
-------------------------------------------------------------------------------

type Byte = Word8

data AlexInput =
    AlexInput
        !Char        -- previous char
        !(Ptr Word8) -- current input string
        !Int         -- bytes left

ignorePendingBytes :: AlexInput -> AlexInput
ignorePendingBytes i = i   -- no pending bytes when lexing bytestrings

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar (AlexInput c _ _) = c

type P = Ptr Word8
r :: P -> Word8
r ptr = unsafePerformIO (peek ptr)
{-# INLINE r #-}

pp :: Ptr Word8 -> Ptr Word8
pp !x = x `plusPtr` 1
{-# INLINE pp #-}
m1 :: Int -> Int
m1 !x = pred x
{-# INLINE m1 #-}

alexGetByte :: AlexInput -> Maybe (Byte,AlexInput)
alexGetByte (AlexInput _ p 0) = Nothing
alexGetByte (AlexInput _ p n) =
    let b   = r p
        c   = B.w2c b
        i'  = AlexInput c (pp p) (m1 n)
    in i' `seq` Just (b, i')

alexScanTokens :: B.ByteString -> Either String [Token]
alexScanTokens str =
  unsafePerformIO $ withForeignPtr fp $ \ p ->
    return $ go [] (AlexInput '\n' p len)
  where (fp, offset, len) = B.toForeignPtr str
        go acc inp@(AlexInput _ _ n) =
          case alexScan inp 0 of
                AlexEOF -> Right $ reverse acc
                AlexError (AlexInput _ _ n') ->
                    Left $ error $ "lexical error at byte " ++ show (len - n')
                AlexSkip  inp' _len       -> go acc inp'
                AlexToken inp'@(AlexInput _ _ n') _ act ->
                    let !t = act (T.decodeUtf8 $
                                  B.take (n - n') $ B.drop (len - n) str) in
                    go (t:acc) inp'

-- Tokenization
-------------------------------------------------------------------------------


-- | Parse a 'Text' into a list of 'Token's.
--
-- https://drafts.csswg.org/css-syntax/#tokenization

tokenize :: Text -> Either String [Token]
tokenize = alexScanTokens . preprocessInputStream



-- | Before sending the input stream to the tokenizer, implementations must
-- make the following code point substitutions: (see spec)
--
-- https://drafts.csswg.org/css-syntax/#input-preprocessing

preprocessInputStream :: Text -> B.ByteString
preprocessInputStream = B.pack . f . B.unpack . T.encodeUtf8
  where
    -- TODO: move to alexGetByte
    f []            = []

    f (0x0D:0x0A:r) = 0x0A : f r

    f (0x0D:r)      = 0x0A : f r
    f (0x0C:r)      = 0x0A : f r
    f (0x00:r)      = 0xFF : 0xFD : f r

    f (x:r)         = x : f r



-- Serialization
-------------------------------------------------------------------------------


-- | Serialize a list of 'Token's back into 'Text'. Round-tripping is not
-- guaranteed to be identity. The tokenization step drops some information
-- from the source.
--
-- https://drafts.csswg.org/css-syntax/#serialization

serialize :: [Token] -> Text
serialize = mconcat . map renderToken


renderToken :: Token -> Text
renderToken (Whitespace)         = " "

renderToken (CDO)                = "<!--"
renderToken (CDC)                = "-->"

renderToken (Comma)              = ","
renderToken (Colon)              = ":"
renderToken (Semicolon)          = ";"

renderToken (LeftParen)          = "("
renderToken (RightParen)         = ")"
renderToken (LeftSquareBracket)  = "["
renderToken (RightSquareBracket) = "]"
renderToken (LeftCurlyBracket)   = "{"
renderToken (RightCurlyBracket)  = "}"

renderToken (SuffixMatch)        = "$="
renderToken (SubstringMatch)     = "*="
renderToken (PrefixMatch)        = "^="
renderToken (DashMatch)          = "|="
renderToken (IncludeMatch)       = "~="

renderToken (Column)             = "||"

renderToken (String d x)         = T.singleton d <> renderString x <> T.singleton d
renderToken (BadString d x)      = T.singleton d <> renderString x <> T.singleton d

renderToken (Number x _)         = x
renderToken (Percentage x _)     = x <> "%"
renderToken (Dimension x _ u)    = x <> u

renderToken (Url x)              = "url(" <> x <> ")"
renderToken (BadUrl x)           = "url(" <> x <> ")"

renderToken (Ident x)            = x

renderToken (AtKeyword x)        = "@" <> x

renderToken (Function x)         = x <> "("

renderToken (Hash _ x)           = "#" <> x

renderToken (Delim x)            = T.singleton x



renderString :: Text -> Text
renderString = T.pack . concatMap f . T.unpack
  where
    nonPrintableCodePoint c
        | c >= '\x0000' && c <= '\x0008' = True -- NULL through BACKSPACE
        | c == '\x000B'                  = True -- LINE TABULATION
        | c >= '\x000E' && c <= '\x001F' = True -- SHIFT OUT through INFORMATION SEPARATOR ONE
        | c == '\x007F'                  = True -- DELETE
        | otherwise                      = False

    nonASCIICodePoint c = c >= '\x0080' -- control

    f c = if nonPrintableCodePoint c || nonASCIICodePoint c
        then "\\" <> showHex (ord c) ""
        else [c]


-- escapedCodePoint :: Parser Char
-- escapedCodePoint = do
--     mbChar <- AP.peekChar
--     case mbChar of
--         Nothing -> return $ '\xFFFD'
--         Just ch -> do
--          if isHexChar ch
--           then do
--             (t, _) <- AP.runScanner 0 f
--             case unhex (T.unpack t) of
--                 Nothing -> fail $ "escapedCodePoint: unable to parse hex " ++ (T.unpack t)
--                 Just cp -> do
--                     AP.peekChar >>= \c -> case c of
--                         Just nc -> if isWhitespace nc then void AP.anyChar else return ()
--                         _ -> return ()
--                     return $ if cp == 0 || cp > 0x10FFFF
--                       then chr 0xFFFD
--                       else chr cp
--           else do
--             if ch == '\n'
--                 then fail "A newline"
--                 else AP.anyChar >> return ch

--   where
--     f :: Int -> Char -> Maybe Int
--     f n c =
--         if n < 6 && isHexChar c
--             then Just (n + 1)
--             else Nothing


-- whenNext :: Char -> a -> Parser a
-- whenNext c a = do
--     mbChar <- AP.peekChar
--     if mbChar == Just c
--         then return a
--         else fail "whenNext"

-- -- 4.3.4. Consume a string token
-- parseString :: Char -> Parser Token
-- parseString endingCodePoint = do
--     skipChar endingCodePoint
--     go []

--   where
--     go acc = do
--         s <- AP.takeWhile (\ c -> c /= endingCodePoint && c /= '\\' && c /= '\n')
--         go' (s:acc)
--     go' acc = choice
--         [ (skipChar endingCodePoint <|> AP.endOfInput) *> return (String endingCodePoint $ fromAcc acc)
--         , AP.string "\\\n" *> go acc -- why?
--         , whenNext '\n' (BadString endingCodePoint $ fromAcc acc)
--         , escapedCodePoint' >>= \ch -> go (T.singleton ch:acc)
--         ]

-- fromAcc :: [T.Text] -> Text
-- fromAcc = T.concat . reverse

-- parseNumeric :: Parser Token
-- parseNumeric = do
--     (repr, nv) <- parseNumericValue
--     dimNum repr nv <|> pctNum repr nv <|> return (Number repr nv)
--   where
--     dimNum repr nv = do
--         unit <- parseName
--         return $ Dimension repr nv unit
--     pctNum repr nv = do
--         skipChar '%'
--         return $ Percentage repr nv


-- parseSign :: Parser (Text, Int)
-- parseSign = do
--     mbChar <- AP.peekChar
--     case mbChar of
--         Just '+' -> AP.anyChar >> return ("+", 1)
--         Just '-' -> AP.anyChar >> return ("-", (-1))
--         _        -> return ("", 1)

-- parseNumericValue :: Parser (Text, NumericValue)
-- parseNumericValue = do
--     -- Sign
--     (sS, s) <- parseSign

--     -- Digits before the decimal dot. They are optional (".1em").
--     (iS, i) <- do
--         digits <- AP.takeWhile isDigit
--         return $ if (T.null digits)
--             then ("", 0)
--             else (digits, read $ T.unpack digits)

--     -- Decimal dot and digits after it. If the decimal dot is there then it
--     -- MUST be followed by one or more digits. This is not allowed: "1.".
--     (fS, f, fB) <- option ("", 0, False) $ do
--         skipChar '.'
--         digits <- AP.takeWhile1 isDigit
--         return ("." <> digits, read $ T.unpack digits, True)

--     -- Exponent (with optional sign).
--     (tS, t, eS, e, eB) <- option ("", 1, "", 0, False) $ do
--         e <- AP.char 'E' <|> AP.char 'e'
--         (tS, t) <- parseSign
--         eS <- AP.takeWhile1 isDigit

--         return (T.singleton e <> tS, t, eS, read $ T.unpack eS, True)

--     let repr = sS<>iS<>fS<>tS<>eS
--     if T.null repr || repr == "-" || repr == "+" || T.head repr == 'e' || T.head repr == 'E'
--         then fail "parseNumericValue: no parse"
--         else do
--             let v = fromIntegral s * (i + f*10^^(-(T.length fS - 1))) * 10^^(t*e)
--             return $ if fB || eB
--                 then (repr, NVNumber v)
--                 else (repr, NVInteger v)


-- parseUrl :: Parser Token
-- parseUrl = do
--     AP.skipWhile isWhitespace
--     go []

--   where
--     endOfUrl acc =
--         (skipChar ')' <|> AP.endOfInput) *> return (Url $ fromAcc acc)

--     go acc = do
--         u <- AP.takeWhile (\ c -> c /= ')' && c /= '\\' && not (bad c || isWhitespace c))
--         go' (u:acc)
--     go' acc = choice
--         [ endOfUrl acc
--         , AP.satisfy bad >>= \ch -> badUrl (T.singleton ch:acc)
--         , AP.string "\\\n" *> badUrl ("\\\n":acc)
--         , AP.takeWhile1 isWhitespace >>= \c -> (endOfUrl acc <|> badUrl (c:acc))
--         , escapedCodePoint' >>= \ch -> go (T.singleton ch:acc)
--         ]
--     bad c = c == '"' || c == '\'' || c == '('

--     badUrl acc = do
--         u <- AP.takeWhile (\ c -> c /= ')' && c /= '\\')
--         badUrl' (u:acc)
--     badUrl' acc = choice
--         [ (skipChar ')' <|> AP.endOfInput) *> return (BadUrl $ fromAcc acc)
--         , escapedCodePoint' >>= \ch -> badUrl (T.singleton ch:acc)
--         ]


-- parseIdentLike :: Parser Token
-- parseIdentLike = do
--     name <- parseName
--     choice
--         [ do
--             -- Special handling of url() functions (they are not really
--             -- functions, they have their own Token type).
--             guard $ T.isPrefixOf "url" (T.map toLower name)

--             skipChar '('
--             AP.skipWhile isWhitespace

--             whenNext '"' (Function name) <|> whenNext '\'' (Function name) <|> parseUrl

--         , skipChar '(' *> return (Function name)
--         , return (Ident name)
--         ]


-- parseEscapedIdentLike :: Parser Token
-- parseEscapedIdentLike = do
--     mbChar <- AP.peekChar
--     case mbChar of
--         Just '\\' -> parseIdentLike <|> (AP.anyChar >> return (Delim '\\'))
--         _         -> fail "parseEscapedIdentLike: Does not start with an escape code"



-- unhex :: (Functor m, Monad m) => String -> m Int
-- unhex = fmap toInt . go []
--   where

--     go :: Monad m => [Int] -> String -> m [Int]
--     go acc []    = return acc
--     go acc (a:r) = do
--         x <- c a
--         go (x:acc) r

--     toInt = sum . map (\(e, x) -> 16 ^ e * x) . zip [(0::Int)..]

--     c :: Monad m => Char -> m Int
--     c '0' = return 0
--     c '1' = return 1
--     c '2' = return 2
--     c '3' = return 3
--     c '4' = return 4
--     c '5' = return 5
--     c '6' = return 6
--     c '7' = return 7
--     c '8' = return 8
--     c '9' = return 9
--     c 'A' = return 10
--     c 'B' = return 11
--     c 'C' = return 12
--     c 'D' = return 13
--     c 'E' = return 14
--     c 'F' = return 15
--     c 'a' = return 10
--     c 'b' = return 11
--     c 'c' = return 12
--     c 'd' = return 13
--     c 'e' = return 14
--     c 'f' = return 15
--     c _   = fail "Invalid hex digit!"

}
