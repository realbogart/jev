packages: .
allow-newer: template-haskell,
             base,
             text,
             ghc-prim,
             ghc-lib-parser,
             ormolu,
             hls-graph,
             containers,
             time,
             fourmolu,
             random,
             Diff,
             lens,
             transformers,
             filepath,
             ghc-exactprint,
             ghc-boot-th,
             ghc,
             ghcide,
             lsp,
             lsp-types,
             cabal-install-parsers:ormolu,
             cabal-install-parsers:Cabal-syntax,
             cabal-install,
             Cabal-syntax,
             data-default,
             *:Cabal,
             *:Cabal-syntax
constraints: unix == 2.8.5.1

source-repository-package
  type: git
  location: https://github.com/haskell/hackage-security.git
  tag: 514482d7a5a7df3b010b9137f50fdc928e659e93
  subdir: hackage-security
  --sha256: 2fgoWMyLlcYtk2NUu6LEs0gKO6M6qX2uh3TItDLyQWI=

source-repository-package
  type: git
  location: https://github.com/tweag/ormolu.git
  tag: 7a334ac3a05f3c3bf9b4516b31b7ad4e74e5aacf
  --sha256: 92JJolZ7+CSrV39SnOGpNp7bibUnNannzS8mBg7jf3U=

source-repository-package
  type: git
  location: https://github.com/haskell/file-io.git
  tag: e2b5ebccca652bf101270ab98221f03bd12b092c
  --sha256: o50OBMddfRL5BAAQ8e+skZzb3Xg8IBWQDXDLrzgGfUU=

source-repository-package
  type: git
  location: https://github.com/haskell/haskell-language-server.git
  tag: 90319a7e62ab93ab65a95f8f2bcf537e34dae76a
  --sha256: wy348++MiMm/xwtI9M3vVpqj2qfGgnDcZIGXw8sF1sA=

source-repository-package
  type: git
  location: https://github.com/haskell/haskell-language-server.git
  subdir: hls-graph
  tag: 90319a7e62ab93ab65a95f8f2bcf537e34dae76a
  --sha256: wy348++MiMm/xwtI9M3vVpqj2qfGgnDcZIGXw8sF1sA=

source-repository-package
  type: git
  location: https://github.com/haskell/haskell-language-server.git
  subdir: hls-plugin-api
  tag: 90319a7e62ab93ab65a95f8f2bcf537e34dae76a
  --sha256: wy348++MiMm/xwtI9M3vVpqj2qfGgnDcZIGXw8sF1sA=

source-repository-package
  type: git
  location: https://github.com/haskell/haskell-language-server.git
  subdir: hls-test-utils
  tag: 90319a7e62ab93ab65a95f8f2bcf537e34dae76a
  --sha256: wy348++MiMm/xwtI9M3vVpqj2qfGgnDcZIGXw8sF1sA=

source-repository-package
  type: git
  location: https://github.com/haskell/haskell-language-server.git
  subdir: ghcide
  tag: 90319a7e62ab93ab65a95f8f2bcf537e34dae76a
  --sha256: wy348++MiMm/xwtI9M3vVpqj2qfGgnDcZIGXw8sF1sA=

source-repository-package
  type: git
  location: https://github.com/haskell-CI/haskell-ci.git
  tag: 90e8ffe71ffaf883e2bedae6112c59e7eb9f0b2c
  subdir: cabal-install-parsers
  --sha256: 5FYqXIF6TZskW4q7tvl6KkXNdR3FOwI8LmXbSMAEwi4=

source-repository-package
  type: git
  location: https://github.com/haskell/cabal.git
  tag: 260ecdc3d848782d4df49e629cb0a5dc9e96ca9e
  subdir: Cabal
  --sha256: /TECd3cdqGDdlEur88H0KbV7jcE/czc0jecj9921T5c=

source-repository-package
  type: git
  location: https://github.com/haskell/cabal.git
  tag: 260ecdc3d848782d4df49e629cb0a5dc9e96ca9e
  subdir: Cabal-syntax
  --sha256: /TECd3cdqGDdlEur88H0KbV7jcE/czc0jecj9921T5c=

source-repository-package
  type: git
  location: https://github.com/haskell/cabal.git
  tag: 260ecdc3d848782d4df49e629cb0a5dc9e96ca9e
  subdir: cabal-install
  --sha256: /TECd3cdqGDdlEur88H0KbV7jcE/czc0jecj9921T5c=

source-repository-package
  type: git
  location: https://github.com/haskell/cabal.git
  tag: 260ecdc3d848782d4df49e629cb0a5dc9e96ca9e
  subdir: cabal-install-solver
  --sha256: /TECd3cdqGDdlEur88H0KbV7jcE/czc0jecj9921T5c=

source-repository-package
  type: git
  location: https://github.com/haskell/lsp.git
  tag: bd0217ce75c38070311d1fd61ee3961553f197bc
  subdir: lsp
  --sha256: DZN7aJEaoTyJMs/8EMlXwj7JQ7VYNyuQLyN0mD68c5U=

source-repository-package
  type: git
  location: https://github.com/haskell/lsp.git
  tag: bd0217ce75c38070311d1fd61ee3961553f197bc
  subdir: lsp-types
  --sha256: DZN7aJEaoTyJMs/8EMlXwj7JQ7VYNyuQLyN0mD68c5U=

source-repository-package
  type: git
  location: https://github.com/haskell/hie-bios.git
  tag: 65df091a4464b43043661d70c1dcfed1ed1fa9a9
  --sha256: esThG6NstiEWznITQbTm6zN224rGVLPyIGYjampRQXQ=
