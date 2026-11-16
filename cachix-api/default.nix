{ mkDerivation, aeson, async, base, bytestring, conduit, crypton
, deriving-aeson, exceptions, hspec, hspec-discover, http-media
, jose, lib, nix-narinfo, openapi3, protolude, ram, resourcet
, safe-exceptions, servant, servant-auth, stm-chans, swagger2, text
, time, unordered-containers, uuid, websockets
}:
mkDerivation {
  pname = "cachix-api";
  version = "1.15.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson async base bytestring conduit crypton deriving-aeson
    exceptions http-media jose nix-narinfo openapi3 protolude ram
    resourcet safe-exceptions servant servant-auth stm-chans swagger2
    text time unordered-containers uuid websockets
  ];
  testHaskellDepends = [
    aeson base bytestring hspec protolude websockets
  ];
  testToolDepends = [ hspec-discover ];
  homepage = "https://github.com/cachix/cachix#readme";
  description = "Servant HTTP API specification for https://cachix.org";
  license = lib.meta.getLicenseFromSpdxId "Apache-2.0";
}
