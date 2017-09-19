# drifter-project-m36

[![Haskell Programming Language](https://img.shields.io/badge/language-Haskell-blue.svg)](http://www.haskell.org)
[![BSD3 License](http://img.shields.io/badge/license-BSD3-brightgreen.svg)](https://tldrlegal.com/license/bsd-3-clause-license-%28revised%29)
[![Hackage](https://img.shields.io/hackage/v/drifter-project-m36.svg)](http://hackage.haskell.org/package/drifter-project-m36)
[![Build status](https://travis-ci.org/3noch/drifter-project-m36.svg?branch=master)](https://travis-ci.org/3noch/drifter-project-m36)
[![Hackage-Deps](https://img.shields.io/hackage-deps/v/drifter-project-m36.svg)](http://packdeps.haskellers.com/feed?needle=drifter-project-m36)

Drifter migration backend for the amazing Project: M36 database.

This package is maintained by [Grafted-In](https://www.graftedin.io/).


## Example

```haskell
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Drifter.ProjectM36
import Drifter.ProjectM36.RelSchema
import ProjectM36.Atomable (toAtomType)
import ProjectM36.Client.Simple

main :: IO ()
main = do
  dbConn <- ... -- use ProjectM36.Client.Simple
  runMigrations dbConn schemaChanges

schemaChanges :: [Change PM36Migration]
schemaChanges = [
    Change{
      changeName         = ChangeName "make-relvar-purchases",
      changeDescription  = Nothing,
      changeDependencies = [],
      changeMethod       = MigrationStep (
        defineRelSchema $ RelSchema "purchases" $ Map.fromList [
            ("purchaseUuid",        (TextAtomType,                   Unique)),
            ("purchasePersonName",  (TextAtomType,                   NonUnique)),
            ("purchasePersonEmail", (TextAtomType,                   NonUnique)),
            ("purchaseStates",      (toAtomType $ undefined @[Text], NonUnique)),
            ("purchasePriceCents",  (IntAtomType,                    NonUnique)),
            ("purchaseDownloads",   (IntAtomType,                    NonUnique)),
            ("purchaseTimestamp",   (DateTimeAtomType,               NonUnique))
          ]
      )
    }
  ]
```
