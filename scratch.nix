{ mkDerivation, base, Chart, Chart-diagrams, colour, diagrams-lib
, diagrams-svg, stdenv
}:
mkDerivation {
  pname = "scratch";
  version = "0";
  src = ./.;
  libraryHaskellDepends = [
    base Chart Chart-diagrams colour diagrams-lib diagrams-svg
  ];
  license = stdenv.lib.licenses.unfree;
}
