with import (builtins.fetchTarball {
  sha256 = "09d4f6h98rmxnxzm1x07jxgrc81k6mz7fjigq375fkmb41j2kdsi";
  url = let
    rev = "b6bca3d80619f1565ba0ea635b0d38234e41c6bd";
    in "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
}) {};
with pkgs.haskellPackages;
(developPackage {
  root = ./.;

  overrides = self: super:
    {
      #weigh = super.callHackageDirect {
      #  pkg = "weigh";
      #  ver = "0.0.16";
      #  sha256 = "0icdyvxxi7493ch8xlpwn024plspbsdssxmcy5984yar298z8hcw";
      #} {};

      PyF = haskell.lib.dontCheck super.PyF;
      besout = haskell.lib.doJailbreak super.besout;
    };
  }).overrideAttrs(old: {
    buildInputs = [cabal-install];
  })
