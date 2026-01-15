module Demo.Interpreter.Ghci
  ( -- * Evaluation
    evalGhciExpr
  , evalGhciExprTyped
  , GhciResult (..)
  , ghciResultValue
  , ghciResultDisplay
  , ghciResultVarIndex
  , ghciResultError

    -- * Interpreter State
  , GhciInterpreter
  , newInterpreter
  , closeInterpreter
  ) where

import Data.Aeson (Value, toJSON)
import Data.Text (Text)
import Data.Text qualified as T
import Demo.Core.Types (VarStore, insertVar, nextVarIndex)
import Language.Haskell.Interpreter
  ( Extension (OverloadedStrings)
  , GhcError (..)
  , InterpreterError (..)
  , OptionVal ((:=))
  , as
  , interpret
  , languageExtensions
  , runInterpreter
  , set
  , setImportsQ
  )

-- | Result of GHCI evaluation
data GhciResult
  = GhciSuccess Value Text Int  -- ^ Value, Display text, Var index
  | GhciError Text              -- ^ Error message
  deriving stock (Show, Eq)

-- | Get the value from a successful result
ghciResultValue :: GhciResult -> Maybe Value
ghciResultValue (GhciSuccess v _ _) = Just v
ghciResultValue _ = Nothing

-- | Get the display text from a result
ghciResultDisplay :: GhciResult -> Maybe Text
ghciResultDisplay (GhciSuccess _ d _) = Just d
ghciResultDisplay _ = Nothing

-- | Get the var index from a successful result
ghciResultVarIndex :: GhciResult -> Maybe Int
ghciResultVarIndex (GhciSuccess _ _ i) = Just i
ghciResultVarIndex _ = Nothing

-- | Get the error message from a failed result
ghciResultError :: GhciResult -> Maybe Text
ghciResultError (GhciError e) = Just e
ghciResultError _ = Nothing

-- | GHCI interpreter (placeholder for future stateful interpreter)
data GhciInterpreter = GhciInterpreter
  deriving stock (Show, Eq)

-- | Create a new GHCI interpreter
newInterpreter :: IO GhciInterpreter
newInterpreter = pure GhciInterpreter

-- | Close the GHCI interpreter
closeInterpreter :: GhciInterpreter -> IO ()
closeInterpreter _ = pure ()

-- | Evaluate a GHCI expression
evalGhciExpr :: GhciInterpreter -> Text -> VarStore -> IO (GhciResult, VarStore)
evalGhciExpr _ expr store = do
  let varIdx = nextVarIndex store
  result <- tryEvalAsJson expr
  case result of
    Left err ->
      pure (GhciError (formatError err), store)
    Right (val, display) ->
      let newStore = insertVar varIdx val store
       in pure (GhciSuccess val display varIdx, newStore)

-- | Evaluate with an expected type annotation
evalGhciExprTyped ::
  GhciInterpreter ->
  Text ->
  Text ->
  VarStore ->
  IO (GhciResult, VarStore)
evalGhciExprTyped interp expr _expectedType store =
  -- For now, ignore expected type and just evaluate
  evalGhciExpr interp expr store

-- | Try to evaluate an expression as JSON
tryEvalAsJson :: Text -> IO (Either InterpreterError (Value, Text))
tryEvalAsJson expr = do
  result <- runInterpreter $ do
    set [languageExtensions := [OverloadedStrings]]
    setImportsQ
      [ ("Prelude", Nothing)
      , ("Data.Aeson", Nothing)
      , ("Data.Text", Just "T")
      ]
    -- Get the Show representation for display
    showResult <- interpret (T.unpack $ "show (" <> expr <> ")") (as :: String)
    -- Try to convert to JSON Value (wrap as string if not ToJSON)
    let jsonVal = toJSON showResult
    pure (jsonVal, T.pack showResult)

  case result of
    Left err -> pure (Left err)
    Right val -> pure (Right val)

-- | Format interpreter error for display
formatError :: InterpreterError -> Text
formatError = \case
  UnknownError msg -> T.pack msg
  WontCompile errs -> T.unlines $ map (T.pack . errMsg) errs
  NotAllowed msg -> "Not allowed: " <> T.pack msg
  GhcException msg -> "GHC Exception: " <> T.pack msg
