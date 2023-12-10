{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    roc.url = "github:roc-lang/roc";
    roc2nix = {
      url = "github:JRMurr/roc2nix";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.roc.follows = "roc";
      inputs.flake-utils.follows = "flake-utils";
    };
  };

  outputs = { self, nixpkgs, flake-utils, roc, roc2nix, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
        rocPkgs = roc.packages.${system};
        rocFull = rocPkgs.full;

        rocLib = (roc2nix.lib.${system}).overrideToolchain rocPkgs.cli;

        compiledApp =
          let
            fs = pkgs.lib.fileset;
            # only grab roc files
            sourceFiles = fs.fileFilter (file: pkgs.lib.hasSuffix ".roc" file.name) ./roc;
          in
          rocLib.buildRocApp {
            name = "aoc";
            version = "0.1.0";

            src = fs.toSource {
              root = ./roc;
              fileset = sourceFiles;
            };

            rocDeps = {
              "https://github.com/roc-lang/basic-cli/releases/download/0.7.0/bkGby8jb0tmZYsy2hg1E_B2QrCgcSTxdUlHtETwm5m4.tar.br" = "sha256-71jf2j6/0wsAuEKVJU+VJ+XwBNWvFvX6IkR3gjfbkX8=";
            };
          };


      in
      {
        formatter = pkgs.nixpkgs-fmt;
        devShells = {
          default = pkgs.mkShell {
            buildInputs = with pkgs;
              [
                rocFull
                aoc-cli
                just
              ];

            shellHook = ''
              export ROC_LSP_PATH=${rocFull}/bin/roc_ls
            '';
          };
        };

        packages = { default = compiledApp; };
      });
}
