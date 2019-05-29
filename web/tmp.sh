executable reflex-starter
  hs-source-dirs:      app
  main-is:             Main.hs
  ghc-options:         -threaded -rtsopts -with-rtsopts=-N
  if impl(ghcjs >= 0.2.1)
    ghcjs-options:     -dedupe

  build-depends:       base
                     , reflex >= 0.4.0
                     , reflex-dom >= 0.3
  default-language:    Haskell2010

source-repository head
  type:     git
  location: https://github.com/yamafaktory/reflex-starter
