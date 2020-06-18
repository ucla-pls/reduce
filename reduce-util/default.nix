{ pkgs ? import ./nix/nixpkgs.nix {}
, compiler ? "default"
, dirtree ? import ./nix/dirtree.nix
, hspec-hedgehog ? import ./nix/hspec-hedgehog.nix
, reduce ? ../reduce
}:
let
  haskellPackages =
    if compiler == "default"
    then pkgs.haskellPackages
    else pkgs.haskell.packages."${compiler}";
in
  haskellPackages.developPackage {
    root = builtins.filterSource
      (path: type: baseNameOf path != ".nix")
      ./.;
    name = "reduce-util";
    source-overrides = { inherit dirtree reduce hspec-hedgehog; };
    overrides = hsuper: hself: { };
    modifier = drv:
      with pkgs.haskell.lib;
      addBuildTools drv (with haskellPackages; [ cabal-install ghcid ])
    ;
  }
