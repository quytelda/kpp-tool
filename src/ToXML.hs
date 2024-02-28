{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module ToXML (ToXML(..)) where

import           Text.XML

class ToXML a where
  toElement :: a -> Element

  toNode :: a -> Node
  toNode = NodeElement . toElement

  toDocument :: a -> Document
  toDocument v =
    let documentPrologue = Prologue [] Nothing []
        documentEpilogue = []
        documentRoot     = toElement v
    in Document{..}

