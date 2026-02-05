let Stack = < Str : Text | Lst : List Text >

in
    { resolver = Stack.Str "nightly-2025-04-22"
    , packages = Stack.Lst ["."]
    , extra-deps = Stack.Lst
        [ "fourmolu-0.17.0.0"
        , "table-layout-1.0.0.2@sha256:d1d7fb2afb618bed9c749c0873011f746f2448c3cd45b704380f8768b449af29,7880"
        , "number-length-0.2.1.0@sha256:cf2c851ab375e2b151973d1525833bb4318b959369301fc79764d3c7c755d3a6,5912"
        ]
    }
