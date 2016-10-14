{ mkDerivation, ansi-wl-pprint, array, base, binary, bytestring
, conduit, containers, cryptohash, directory, exceptions, filepath
, gitrev, hex, mtl, optparse-applicative, path, stdenv, text, time
, transformers, unix
}:
mkDerivation {
  pname = "hascar";
  version = "0.2.1.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    array base binary bytestring conduit containers exceptions mtl path
    text time
  ];
  executableHaskellDepends = [
    ansi-wl-pprint base binary bytestring directory filepath gitrev
    optparse-applicative path text transformers unix
  ];
  testHaskellDepends = [
    base bytestring conduit cryptohash hex path
  ];
  homepage = "https://github.com/VirtualForgeGmbH/hascar";
  description = "Decompress SAPCAR archives";
  license = stdenv.lib.licenses.gpl2;
}
