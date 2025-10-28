{
  description = "Flake for a R environment";
  inputs = {
    nixpkgs.url = "nixpkgs/nixos-25.05";
    flake-utils.url = "github:numtide/flake-utils";

    nix2container.url = "github:nlewo/nix2container";
    nix2container.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
      nix2container,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs { inherit system; };
        nix2containerPkgs = nix2container.packages.${system};
        imageBuilder = (import ./nix_package/image.nix { inherit nix2containerPkgs pkgs; }).builder;

        Rpkgs = pkgs;
        R-packages = with Rpkgs.rPackages; [
          shiny
          shinydashboard
          shinycssloaders
          shinyjs
          shinyvalidate
          RSQLite
          MASS
          digest
          plotly
          DT
          igraph
          lubridate
          vistime
          scrm
          GenomicRanges
          bsicons
          uuid
          shinyTree
          prettyunits
          reactable
          shinyWidgets
          logger
          callr

          (pkgs.rPackages.buildRPackage {
            name = "rutilstimflutre";
            src = pkgs.fetchFromGitHub {
              owner = "timflutre";
              repo = "rutilstimflutre";
              rev = "42660be440687c50169acae592cc32e1a11e4255";
              sha256 = "sha256-Au1Izpuht83S4oEhq+1fq4cIrOt+48DbsCoxNvdndi4=";
            };
            propagatedBuildInputs = with pkgs.rPackages; [
              data_table
              lme4
              Matrix
              Rcpp
            ];
          })
        ];
        R-test-packages = with Rpkgs.rPackages; [ testthat ];
        R-dev-packages = with Rpkgs.rPackages; [
          devtools
          languageserver
          styler
          profvis
          microbenchmark
        ];
      in
      rec {
        devShells.default = pkgs.mkShell {
          LOCALE_ARCHIVE =
            if "${system}" == "x86_64-linux" then "${pkgs.glibcLocalesUtf8}/lib/locale/locale-archive" else "";
          LANG = "en_US.UTF-8";
          LC_ALL = "en_US.UTF-8";
          R_LIBS_USER = "''"; # to not use users' installed R packages
          R_ZIPCMD = "${pkgs.zip}/bin/zip";
          TEST_PORT = 3000;
          PLAYWRIGHT_BROWSERS_PATH = pkgs.playwright.browsers;
          PLAYWRIGHT_SKIP_VALIDATE_HOST_REQUIREMENTS = true;
          nativeBuildInputs = [ pkgs.bashInteractive ];
          shellHook = ''
            npm install --ignore-scripts
            export PLANTBREEDGAME_DATA_ROOT=$(realpath ./data);
          '';

          buildInputs = [
            (Rpkgs.rWrapper.override { packages = R-packages ++ R-test-packages ++ R-dev-packages; })
            (Rpkgs.rstudioWrapper.override { packages = R-packages ++ R-test-packages ++ R-dev-packages; })
            pkgs.pandoc
            pkgs.zip
            pkgs.nodejs
            pkgs.playwright
            pkgs.skopeo
            pkgs.chromium
          ];
        };

        apps = {
          default = {
            type = "app";
            program = nixpkgs.lib.getExe (
              pkgs.writeShellApplication {
                name = "PlantBreedGame";
                text = ''
                  ${nixpkgs.lib.getExe (packages.plantBreedGame-dev)} --port 3000
                '';
              }
            );
          };

          ui_tests = {
            type = "app";
            program = nixpkgs.lib.getExe (
              pkgs.writeShellApplication {
                name = "PlantBreedGame-test_ui";
                text = ''
                  # npx playwright test "$@"
                  make ui_tests
                '';
              }
            );
          };

          unit_tests = {
            type = "app";
            program = nixpkgs.lib.getExe (
              pkgs.writeShellApplication {
                name = "PlantBreedGame-unit_tests";
                text = ''
                  make unit_tests
                '';
              }
            );
          };

          initialise-data = {
            type = "app";
            program = nixpkgs.lib.getExe (
              pkgs.writeShellApplication {
                name = "PlantBreedGame-unit_tests";
                text = ''
                  make data
                '';
              }
            );
          };
        };

        packages.plantBreedGame = pkgs.callPackage ./nix_package/default.nix {
          inherit pkgs;
          R_deps = R-packages;
          tests_R_deps = R-test-packages;
          src = pkgs.lib.sources.cleanSource ./.;
        };
        packages.default = packages.plantBreedGame;

        packages.plantBreedGame-dev = (
          packages.plantBreedGame.overrideAttrs (oldAttrs: {
            doCheck = false;
          })
        );

        images = {
          latest =
            let
            in
            (imageBuilder {
              imageName = "plantBreedGame";
              plantBreedGame = packages.plantBreedGame;
              tag = "latest";
            });
        };
      }
    );
}
