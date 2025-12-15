let Stack = < Str : Text | Lst : List Text >

in
    { resolver = Stack.Str "nightly-2025-04-22"
    , packages = Stack.Lst ["."]
    , extra-deps = Stack.Lst ["fourmolu-0.17.0.0"]
    }