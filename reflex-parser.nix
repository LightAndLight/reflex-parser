{ mkDerivation, base, reflex, stdenv }:
mkDerivation {
  pname = "reflex-parser";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base reflex ];
  executableHaskellDepends = [ base ];
  license = stdenv.lib.licenses.bsd3;
}
