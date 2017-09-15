{ mkDerivation, base, bytestring, containers, cryptonite, lens, mtl, time
, stdenv, text, reactive-banana, errors
}:
mkDerivation {
  pname = "hs-naivechain";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base bytestring containers cryptonite lens mtl text time
    reactive-banana errors
  ];
  description = "Haskell hack @ NaiveChain implementation";
  license = stdenv.lib.licenses.bsd3;
}
