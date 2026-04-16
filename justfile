fmt:
    fourmolu --mode inplace $(find src -name "*.hs")

dbg n="": fmt
    stack build --fast --ghc-options="-O0"
    stack exec sonnet-haskell-exe -- {{n}}

build: fmt
    stack build --ghc-options="-O2"

run n="": build
    stack exec sonnet-haskell-exe -- {{n}}