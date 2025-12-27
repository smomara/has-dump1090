{
  description = "has-dump1090";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs {
          inherit system;
          config.allowUnsupportedSystem = true; # 0_o
        };
        has-dump-1090 = pkgs.haskellPackages.callCabal2nix "has-dump-1090" ./. { };
      in
      {
        packages.default = has-dump-1090;
        devShells.default = pkgs.haskellPackages.shellFor {
          packages = p: [ has-dump-1090 ];

          buildInputs = with pkgs.haskellPackages; [
            cabal-install
            haskell-language-server
            hlint
            fourmolu
          ];

          nativeBuildInputs = with pkgs; [
            pkg-config
            rtl-sdr
          ];
        };
      }
    );
}
