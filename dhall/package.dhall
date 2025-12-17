let libName : Text = "todo"

let version : Text = "2.0.0"

let githubRepo : Text = "Choi-Suhyeon/todoCli"

let author : Text = "SUHYEON CHOI"

let maintainer : Text = "SUHYEON CHOI (1AzyF0x) <1AzyF0x@protonmail.ch>"

let copyright : Text = "2025 SUHYEON CHOI (1AzyF0x)"

let extraSourceFiles : List Text =
    [ "README.md"
    , "CHANGELOG.md"
    , "ROADMAP.md"
    , "docs/long-help.md"
    ]

let dataFiles : List Text =
    [ "docs/long-help.txt"
    , "docs/todo.1"
    ]

let commonGhcOptions : List Text =
    [ "-Wall"
    , "-Wcompat"
    , "-Widentities"
    , "-Wincomplete-record-updates"
    , "-Wincomplete-uni-patterns"
    , "-Wmissing-export-lists"
    , "-Wmissing-home-modules"
    , "-Wredundant-constraints"
    ]

let ghcOptions : List Text =
    [ "-threaded"
    , "-rtsopts"
    , "-with-rtsopts=-N"
    ]

let dependencies : List Text =
    [ "base"
    , "boxes"
    , "bytestring"
    , "cereal"
    , "containers"
    , "directory"
    , "file-embed"
    , "filepath"
    , "hashable"
    , "mtl"
    , "optparse-applicative"
    , "regex-tdfa"
    , "template-haskell"
    , "text"
    , "time"
    , "transformers"
    , "unordered-containers"
    , "witch"
    ]

let extTypeSystem : List Text =
    [ "FlexibleContexts"
    , "FlexibleInstances"
    , "MultiParamTypeClasses"
    , "FunctionalDependencies"
    , "ConstraintKinds"
    , "ScopedTypeVariables"
    , "DataKinds"
    , "TypeApplications"
    ]

let extDeriving : List Text =
    [ "DeriveGeneric"
    , "DeriveAnyClass"
    , "GeneralizedNewtypeDeriving"
    , "DerivingStrategies"
    , "StandaloneDeriving"
    , "DerivingVia"
    ]

let extOverloading : List Text =
    [ "OverloadedStrings"
    , "OverloadedLists"
    ]

let extRecords : List Text =
    [ "DuplicateRecordFields"
    , "OverloadedRecordDot"
    , "OverloadedLabels"
    , "NoFieldSelectors"
    , "RecordWildCards"
    , "NamedFieldPuns"
    ]

let extGeneralSyntax : List Text =
    [ "ImportQualifiedPost"
    , "BlockArguments"
    , "TupleSections"
    , "LambdaCase"
    , "MultiWayIf"
    , "BangPatterns"
    , "NumericUnderscores"
    ]

let defaultExtensions : List Text
    = extTypeSystem
    # extDeriving
    # extOverloading
    # extRecords
    # extGeneralSyntax

in
    { name = libName
    , version = version
    , github = githubRepo
    , license = "BSD-3-Clause"
    , author = author
    , maintainer = maintainer
    , copyright = copyright
    , extra-source-files = extraSourceFiles
    , data-files = dataFiles
    , description = "Please see the README on GitHub at <https://github.com/${githubRepo}#readme>"
    , dependencies = dependencies
    , ghc-options = commonGhcOptions
    , library =
        { source-dirs = "src"
        , default-extensions = defaultExtensions
        }
    , executables =
        { todo =
            { main = "Main.hs"
            , source-dirs = "app"
            , ghc-options = ghcOptions
            , dependencies = [libName]
            , default-extensions = defaultExtensions
            }
        }
    {--
    , tests =
        { main = "todo-test"
        , source-dirs = "test"
        , ghc-options = ghcOptions
        , dependencies = [libName]
        }
    --}
    }
