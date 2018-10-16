{-# LANGUAGE TemplateHaskell #-}

-- | The 'sql' quasiquoter, for writing large @SQL@ statements.

module MySql.QQ
     ( sql
     ) where

import Data.String (fromString)
import Database.MySQL.Base (Query)
import Language.Haskell.TH (Exp, Q, appE, stringE)
import Language.Haskell.TH.Quote (QuasiQuoter (..))

{- | A quasiquoter for writing big @SQL@ queries.
One should consider turning on the @-XQuasiQuotes@ pragma in that module:
@
{-# LANGUAGE QuasiQuoter #-}

myQuery = query conn [sql|
    SELECT
      *
    FROM
      users
    WHERE jobTitle = ?
    |] jobTitle
@
-}
sql :: QuasiQuoter
sql = QuasiQuoter
    { quotePat  = error "MySql.QQ.sql: quasiquoter used in pattern context"
    , quoteType = error "MySql.QQ.sql: quasiquoter used in type context"
    , quoteDec  = error "MySql.QQ.sql: quasiquoter used in declaration context"
    , quoteExp  = sqlExp
    }

sqlExp :: String -> Q Exp
sqlExp = appE [| fromString :: String -> Query |] . stringE
