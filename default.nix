{ mkDerivation, base, bytestring, cassava, containers, filepath
, foldl, hpack, lib, mtl, protolude, streaming, streaming-cassava
, streaming-commons, streaming-nonempty, streaming-with, time
, turtle, vector
}:
mkDerivation {
  pname = "giacenza";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring cassava containers filepath foldl mtl protolude
    streaming streaming-cassava streaming-commons streaming-nonempty
    streaming-with time turtle vector
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    base bytestring cassava containers filepath foldl mtl protolude
    streaming streaming-cassava streaming-commons streaming-nonempty
    streaming-with time turtle vector
  ];
  testHaskellDepends = [
    base bytestring cassava containers filepath foldl mtl protolude
    streaming streaming-cassava streaming-commons streaming-nonempty
    streaming-with time turtle vector
  ];
  prePatch = "hpack";
  homepage = "https://github.com/githubuser/giacenza#readme";
  license = lib.licenses.bsd3;
}
