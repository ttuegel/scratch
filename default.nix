let

  nixpkgs =
    let
      inherit ((import <nixpkgs> {}).pkgs) fetchFromGitHub lib;
      lock = builtins.fromJSON (builtins.readFile ./nixpkgs.lock.json);
      bootstrap =
        fetchFromGitHub {
          owner = "NixOS";
          repo = "nixpkgs-channels";
          inherit (lock) rev sha256;
        };
      defaultOverrides =
        let file = ./default.overrides.nix; in
        lib.optional
        (lib.inNixShell && builtins.pathExists file)
        (import file);
      shellOverrides =
        let file = ./shell.overrides.nix; in
        lib.optional
        (lib.inNixShell && builtins.pathExists file)
        (import file);
      userShellOverrides =
        let
          file =
            builtins.getEnv "HOME" + "/.config/nixpkgs/shell.overrides.nix";
        in
          lib.optional
          (lib.inNixShell && builtins.pathExists file)
          (import file);
    in
      import bootstrap
      {
        config.allowUnfree = true;
        overlays = defaultOverrides ++ shellOverrides ++ userShellOverrides;
      };

in

let

  inherit (nixpkgs) pkgs;
  inherit (pkgs) haskellPackages lib;


  blacklistDirs = [ ".git" "dist" "dist-newstyle" ];
  whitelistExts = [ ".cabal" ".hs" ];
  whitelistNames = [ "LICENSE" ];

  filterSrc =
    let
      overrideSrc = drv: f:
        let inherit (pkgs.haskell.lib) overrideCabal; in
        overrideCabal drv (args: args // { src = f args.src; });
      predicate = path: type:
        let inherit (lib) any elem hasSuffix; in
        let baseName = baseNameOf path; in
        if type == "directory"
          then !(elem baseName blacklistDirs)
          else any (suf: hasSuffix suf baseName) whitelistExts
            || any (name: baseName == name) whitelistNames;
    in
      drv: overrideSrc drv (src: builtins.filterSource predicate src);

  drv = filterSrc (haskellPackages.callPackage ./scratch.nix {});

in

  if lib.inNixShell then drv.env else drv
