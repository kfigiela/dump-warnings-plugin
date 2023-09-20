let sources = import ./nix/sources.nix;
in { }:
let
  ghc = "ghc944";
  pkgs = import sources.nixpkgs { };
  inherit (pkgs) lib;
  inherit (pkgs.haskell.lib) disableLibraryProfiling disableExecutableProfiling;
  packageOverlay = _hfinal: _hprev: rec {
    dump-warnings-plugin = (disableLibraryProfiling (disableExecutableProfiling
      (haskellPkgs.callCabal2nix "dump-warning-plugin" ./. { })));
  };
  haskellPkgs = pkgs.haskell.packages.${ghc}.override (old: {
    overrides = pkgs.lib.composeExtensions old.overrides packageOverlay;
  });
in {
  inherit haskellPkgs;
  inherit pkgs;
  inherit sources;
}
