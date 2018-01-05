{ mkDerivation, base, colour, diagrams-lib, diagrams-svg, stdenv }:
mkDerivation {
  pname = "scratch";
  version = "0";
  src = ./.;
  libraryHaskellDepends = [ base colour diagrams-lib diagrams-svg ];
  license = stdenv.lib.licenses.unfree;
}
