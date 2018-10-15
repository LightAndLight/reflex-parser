{ mkDerivation, base, reflex, reflex-basic-host, stdenv, stm }:
mkDerivation {
  pname = "reflex-parser";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base reflex ];
  executableHaskellDepends = [ base reflex reflex-basic-host stm ];
  license = stdenv.lib.licenses.bsd3;
}
