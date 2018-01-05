{ mkDerivation, base, colour, diagrams, stdenv }:
mkDerivation {
  pname = "scratch";
  version = "0";
  src = ./.;
  libraryHaskellDepends = [ base colour diagrams ];
  license = stdenv.lib.licenses.unfree;
}
