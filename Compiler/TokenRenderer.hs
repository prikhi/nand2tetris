{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Render token streams as a list XML tags
module Compiler.TokenRenderer (tokensToXml) where

import Compiler.Tokens
import Compiler.XmlWriter
import Data.Text (Text, pack)


tokensToXml :: [Token] -> Text
tokensToXml = withRootNode "tokens" . mapM_ renderToken


renderToken :: Token -> Xml
renderToken = \case
    CommentTok {} -> return ()
    IntegerTok i -> textNode "integerConstant" (pack $ show i)
    StringTok s -> textNode "stringConstant" s
    IdentifierTok ident -> textNode "identifier" ident
    KeywordTok kw -> textNode "keyword" (renderKeyword kw)
    SymbolTok sym -> textNode "symbol" (pack . return $ renderSymbol sym)
