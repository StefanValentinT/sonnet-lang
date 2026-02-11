{
  description = "haiku language";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };

        cargo = builtins.fromTOML (builtins.readFile ./Cargo.toml);
        pkgName = cargo.package.name;
        pkgVersion = cargo.package.version;
      in
      {
        packages.default = pkgs.rustPlatform.buildRustPackage {
          pname = pkgName;
          version = pkgVersion;
          src = self;

          cargoLock = {
            lockFile = ./Cargo.lock;
          };

                 };

        apps.default = flake-utils.lib.mkApp {
          drv = self.packages.${system}.default;
        };
      });
}
