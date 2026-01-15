module Demo.UI.Common
  ( -- * Common Attribute Names
    headerAttr
  , footerAttr
  , activeAttr
  , inactiveAttr

    -- * Common Attribute Map
  , defaultAttrMap

    -- * Common UI Patterns
  , connectionStatus
  , slideProgress
  ) where

import Brick (AttrMap, AttrName, attrMap, attrName, fg, on)
import Data.Text (Text)
import Data.Text qualified as T
import Graphics.Vty qualified as V

-- | Header attribute name
headerAttr :: AttrName
headerAttr = attrName "header"

-- | Footer attribute name
footerAttr :: AttrName
footerAttr = attrName "footer"

-- | Active/selected item attribute
activeAttr :: AttrName
activeAttr = attrName "active"

-- | Inactive item attribute
inactiveAttr :: AttrName
inactiveAttr = attrName "inactive"

-- | Default attribute map shared across UI modules
defaultAttrMap :: AttrMap
defaultAttrMap =
  attrMap
    V.defAttr
    [ (headerAttr, V.white `on` V.blue)
    , (footerAttr, V.black `on` V.white)
    , (activeAttr, V.black `on` V.yellow)
    , (inactiveAttr, fg V.white)
    ]

-- | Format connection status text
connectionStatus :: Bool -> Text
connectionStatus True = " (connected)"
connectionStatus False = " (standalone)"

-- | Format slide progress text
slideProgress :: Int -> Int -> Text
slideProgress current total =
  "[" <> T.pack (show (current + 1)) <> "/" <> T.pack (show total) <> "]"
