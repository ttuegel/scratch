{ mkDerivation, base, Chart, Chart-diagrams, colour, comonad
, containers, diagrams-lib, diagrams-svg, free, hpack, lens
, recursion-schemes, stdenv
}:
mkDerivation {
  pname = "scratch";
  version = "0";
  src = ./.;
  libraryHaskellDepends = [
    base Chart Chart-diagrams colour comonad containers diagrams-lib
    diagrams-svg free lens recursion-schemes
  ];
  libraryToolDepends = [ hpack ];
  prePatch = "hpack";
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
