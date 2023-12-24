{-# LANGUAGE OverloadedStrings #-}

-- | A monadic interface for our Analyzer to write XML files
module Compiler.XmlWriter
    ( Xml
    , withRootNode
    , withParentNode
    , textNode
    , escapeXmlVal
    ) where

import Data.Text (Text)
import Data.Text qualified as T


-- | XmlWriter is essentially a ReaderT for the current indentation level
-- & WriterT of the built XML.
--
-- But we implement all that ourselves because it is fun :)
newtype XmlWriter a = XmlWriter (Int -> ([Text], a))


instance Functor XmlWriter where
    fmap f (XmlWriter x) = XmlWriter $ \i ->
        let (xml, a) = x i
         in (xml, f a)


-- | Pure ignores indent, apply prepends the left xml with the right xml
instance Applicative XmlWriter where
    pure a = XmlWriter $ const ([], a)
    XmlWriter x1 <*> XmlWriter x2 = XmlWriter $ \i ->
        let (xml1, f) = x1 i
            (xml2, a) = x2 i
         in (xml2 <> xml1, f a)


-- | Monadic actions prepend the new list to the old
instance Monad XmlWriter where
    return = pure
    XmlWriter x >>= f = XmlWriter $ \i ->
        let (s1, a) = x i
            (XmlWriter y) = f a
            (s2, b) = y i
         in (s2 <> s1, b)


-- | Most functions won't care about returning anything useful, we'll just
-- be building up the XML.
type Xml = XmlWriter ()


-- | Begin XML generation with the given root node, returning the final XML
-- text.
withRootNode :: Text -> Xml -> Text
withRootNode node (XmlWriter action) =
    let
        -- indent nested blocks
        (nodes, ()) = action 1
        -- add our close block
        withClose = writeClose node : nodes
        -- flip and add out open block
        withOpen = writeOpen node : reverse withClose
     in
        T.unlines withOpen


-- | Create a parent node with the given XML inner content.
withParentNode :: Text -> Xml -> Xml
withParentNode node (XmlWriter inner) = XmlWriter $ \i ->
    let
        -- Generate inner nodes with increased indentation
        (innerNodes, ()) = inner (i + 1)
        -- Make our open/close nodes
        open = T.replicate (i * 4) " " <> writeOpen node
        close = T.replicate (i * 4) " " <> writeClose node
     in
        (close : innerNodes <> [open], ())


-- | Generate a new node containing some escaped text.
textNode :: Text -> Text -> Xml
textNode tag val = XmlWriter $ \i ->
    let indent = T.replicate (i * 4) " "
        node = T.concat [indent, writeOpen tag, " ", escapeXmlVal val, " ", writeClose tag]
     in ([node], ())


-- | Perform any necessary escape sequence replacements for characters in
-- an XML node's inner value.
escapeXmlVal :: Text -> Text
escapeXmlVal t =
    foldr
        (uncurry T.replace)
        t
        [ ("<", "&lt;")
        , (">", "&gt;")
        , ("\"", "&quot;")
        , ("&", "&amp;")
        ]


writeOpen :: Text -> Text
writeOpen n = T.concat ["<", n, ">"]


writeClose :: Text -> Text
writeClose n = T.concat ["</", n, ">"]
