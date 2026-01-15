module Demo.Core.Variable
  ( -- * Variable Substitution
    substituteVars
  , substituteVarsEither

    -- * Parsing
  , findVarReferences
  , VarRef (..)
  ) where

import Data.Aeson (Value, encode)
import Data.ByteString.Lazy qualified as BL
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Demo.Core.Types (VarStore, lookupVar)
import Text.Regex.TDFA ((=~))

-- | A variable reference found in text
data VarRef = VarRef
  { vrIndex :: Int
  , vrOriginal :: Text
  }
  deriving stock (Show, Eq)

-- | Find all variable references ($v1, $v2, etc.) in text
findVarReferences :: Text -> [VarRef]
findVarReferences txt =
  [ VarRef (read (T.unpack num)) match
  | (match, num) <- findMatches txt
  ]
 where
  findMatches :: Text -> [(Text, Text)]
  findMatches t =
    let matches = T.unpack t =~ ("\\$v([0-9]+)" :: String) :: [[String]]
     in [ (T.pack m0, T.pack m1)
        | m <- matches
        , (m0, m1) <- case m of
            (x : y : _) -> [(x, y)]
            _ -> []
        ]

-- | Substitute all variable references with their values
-- Returns Left with missing variable index if any variable is not found
substituteVarsEither :: VarStore -> Text -> Either Int Text
substituteVarsEither store txt =
  let refs = findVarReferences txt
   in foldl (substituteOne store) (Right txt) refs

-- | Substitute variables, throwing error on missing variables
substituteVars :: VarStore -> Text -> Text
substituteVars store = either missingVar id . substituteVarsEither store
 where missingVar i = error $ "Variable $v" <> show i <> " not defined"

-- | Substitute a single variable reference
substituteOne :: VarStore -> Either Int Text -> VarRef -> Either Int Text
substituteOne _ (Left e) _ = Left e
substituteOne store (Right txt) VarRef {..} =
  case lookupVar vrIndex store of
    Nothing -> Left vrIndex
    Just val -> Right $ T.replace vrOriginal (valueToText val) txt

-- | Convert a JSON Value to Text for substitution
valueToText :: Value -> Text
valueToText = either (const "<encoding error>") stripQuotes
            . TE.decodeUtf8' . BL.toStrict . encode

-- | Strip surrounding quotes from JSON strings
stripQuotes :: Text -> Text
stripQuotes t | T.length t >= 2, T.head t == '"', T.last t == '"' = T.init (T.tail t)
              | otherwise = t
