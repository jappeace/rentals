{
  hpkgs ? import ./nix/hpkgs.nix {},
  pkgs ? import ./nix/pkgs.nix {},
}:
hpkgs.shellFor {
  packages = ps: [ ps."rentals" ];
  withHoogle = false;
  DEVELOPMENT = "true";

  buildInputs = [
    hpkgs.haskell-language-server
    pkgs.ghcid
    pkgs.cabal-install
    pkgs.haskellPackages.postgresql-libpq
  ];
}
