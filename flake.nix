# I used chatgpt to generate this template and then just
# modified to how I normally use these things.
{
  description = "Rentals";
  nixConfig = {
    extra-substituters = [
      "https://jappie.cachix.org"
    ];
    extra-trusted-public-keys = [
      "jappie.cachix.org-1:+5Liddfns0ytUSBtVQPUr/Wo6r855oNLgD4R8tm1AE4="
    ];
  };


  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, flake-compat }:
    let
      lib = pkgs.haskell.lib;
      pkgs = nixpkgs.legacyPackages.x86_64-linux;
      hpkgs = pkgs.haskellPackages.override {
        overrides = hnew: hold: {
          rentals = hnew.callCabal2nix "rentals" ./. { };
          slugify = lib.dontCheck (lib.markUnbroken hold.slugify);
          iCalendar = lib.dontCheck (lib.doJailbreak (hnew.callCabal2nix "iCalendar"
                  (builtins.fetchGit {
                     url = "https://github.com/jappeace/iCalendar";
                     ref = "bump-bounds";
                     rev = "b1e73678dca75513f322061f91dc333788516e39";
                  }) {}));
        };
      };
    in
    {
      defaultPackage.x86_64-linux =  hpkgs.rentals;
      inherit pkgs;
      devShell.x86_64-linux = hpkgs.shellFor {
        packages = ps : [ ps."rentals" ];
        withHoogle = false;

        buildInputs = [
          hpkgs.haskell-language-server
          pkgs.ghcid
          pkgs.cabal-install
          pkgs.haskellPackages.postgresql-libpq
        ];
      };
    };
}
