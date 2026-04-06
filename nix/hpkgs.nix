{ pkgs ? import ./pkgs.nix { }
,
}:
let
  lib = pkgs.haskell.lib;
in
pkgs.haskellPackages.override {
  overrides = hnew: hold: {
    rentals = hnew.callCabal2nix "rentals" ../. { };
    slugify = lib.dontCheck (lib.markUnbroken hold.slugify);
    groupBy = lib.dontCheck (lib.markUnbroken hold.groupBy);
    mail-pool = hnew.callHackageDirect {
        pkg = "mail-pool";
        ver = "2.3.1";
        sha256 = "sha256-AL1VYMAmgoGTd50IElKRJJbC2eUUiNJX38G19hbv9DQ=";
      } {};
    iCalendar = lib.dontCheck (lib.doJailbreak (hnew.callCabal2nix "iCalendar"
            (builtins.fetchGit {
               url = "https://github.com/jappeace/iCalendar";
               ref = "bump-bounds";
               rev = "b1e73678dca75513f322061f91dc333788516e39";
            }) {}));
  };
}
