let Stack = < Str : Text | Lst : List Text >

in
    { resolver = Stack.Str "nightly-2025-04-22"
    , packages = Stack.Lst ["."]
    , extra-deps = Stack.Lst
        [ "fourmolu-0.17.0.0"
        , "table-layout-1.0.0.2@sha256:d1d7fb2afb618bed9c749c0873011f746f2448c3cd45b704380f8768b449af29,7880"
        ]
    }
