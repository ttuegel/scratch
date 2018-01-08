{ mkDerivation, base, Chart, Chart-diagrams, colour, diagrams-lib
, diagrams-svg, lens, stdenv
}:
mkDerivation {
  pname = "scratch";
  version = "0";
  src = ./.;
  libraryHaskellDepends = [
    base Chart Chart-diagrams colour diagrams-lib diagrams-svg lens
  ];
  license = stdenv.lib.licenses.unfree;
}
