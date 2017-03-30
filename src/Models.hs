{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Models where

import Data.Text (Text)
import Data.Typeable (Typeable)
import Database.Persist.TH
       (mkMigrate, mkPersist, mpsGenerateLenses, persistLowerCase, share,
        sqlSettings)

-- This Template Haskell just generates a 'Comment' datatype and some helper
-- functions for working with it.  For more information, check out Persistent's
-- <http://www.yesodweb.com/book/persistent documentation>.

share
  [mkPersist sqlSettings {mpsGenerateLenses = False}, mkMigrate "migrateAll"]
    [persistLowerCase|
      Comment json
        author   Text
        text     Text

        deriving Eq
        deriving Show
        deriving Typeable
    |]

