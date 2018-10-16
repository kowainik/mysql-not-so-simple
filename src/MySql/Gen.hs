{- | The purpose of this module is to have function that can generate 'Row'
instacnces for tuples of different size. It's quite cumbersome to write and
refactor all instances manually. Instead they can be generated.
-}

module MySql.Gen
       ( genInstances
       , genSingleInstance
       ) where

import qualified Data.Text as T

-- | Generate 'Row' instances for tuples between @2@ and given argument @n@.
genInstances
    :: Int  -- ^ @n@: maximum size of tuple; usually 15
    -> Text
genInstances n = unlines $ map genSingleInstance [2 .. n]

-- | Generates single 'Row' instance for tuple of given size.
genSingleInstance :: Int -> Text
genSingleInstance n = unlines
    [ "instance (" <> context <> ") => Row (" <> vars <> ") where"
    , "    toRow (" <> vars <> ") = [" <> toFieldVars <> "]"
    , "    fromRow [" <> vars <> "] = (" <> commas <> ") <$> " <> fromFieldVars
    , "    fromRow _ = Nothing"
    ]
  where
    -- generates list with variable names like: ["a1", "a2", "a3", ...]
    names :: [Text]
    names = map (\i -> "a" <> show i) [1 .. n]

    vars, commas, context, toFieldVars, fromFieldVars :: Text
    vars = T.intercalate "," names
    commas = toText $ replicate (n - 1) ','
    context       = prefixIntercalate "Field "     ", "    names
    toFieldVars   = prefixIntercalate "toField "   ", "    names
    fromFieldVars = prefixIntercalate "fromField " " <*> " names

prefixIntercalate :: Text -> Text -> [Text] -> Text
prefixIntercalate pref between = T.intercalate between . map (pref <>)
