let nixpkgs_source = (fetchTarball https://github.com/NixOS/nixpkgs-channels/archive/nixos-19.09.tar.gz);
in {pkgs ? import nixpkgs_source {}}:

let
  hp = pkgs.haskellPackages.override{
      overrides = self: super: {
      };};
  ghc = hp.ghcWithPackages (ps: with ps; (
    [ cabal-install split ]));

in pkgs.stdenv.mkDerivation {
  name = "my-env-0";
  buildInputs = [ ghc ];
  shellHook = ''
    export LANG=en_US.UTF-8
    eval $(egrep ^export ${ghc}/bin/ghc)
  '';
}

