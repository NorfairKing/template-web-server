{ mkDerivation, autodocodec, autodocodec-yaml, base, data-default
, envparse, genvalidity-hspec, hspec, hspec-discover, lib
, monad-logger, optparse-applicative, path, path-io, pretty-show
, shakespeare, template-haskell, text, yaml, yesod, yesod-static
, yesod-test
}:
mkDerivation {
  pname = "foo-bar-web-server";
  version = "0.0.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    autodocodec autodocodec-yaml base data-default envparse
    monad-logger optparse-applicative path path-io pretty-show
    shakespeare template-haskell text yaml yesod yesod-static
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [
    base genvalidity-hspec hspec monad-logger text yesod yesod-test
  ];
  testToolDepends = [ hspec-discover ];
  license = lib.licenses.unfree;
  hydraPlatforms = lib.platforms.none;
  mainProgram = "foo-bar-web-server";
}
