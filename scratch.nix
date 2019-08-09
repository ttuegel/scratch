{ mkDerivation, base, Chart, Chart-diagrams, colour, containers
, diagrams-lib, diagrams-svg, hpack, lens, recursion-schemes
, stdenv
}:
mkDerivation {
  pname = "scratch";
  version = "0";
  src = ./.;
  libraryHaskellDepends = [
    base Chart Chart-diagrams colour containers diagrams-lib
    diagrams-svg lens recursion-schemes
  ];
  libraryToolDepends = [ hpack ];
  prePatch = "hpack";
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
