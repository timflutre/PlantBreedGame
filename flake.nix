{
  description = "Flake for a R environment";
  inputs = {
    nixpkgs.url = "nixpkgs/nixos-23.11";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = {
    self,
    nixpkgs,
    flake-utils,
  }:
    flake-utils.lib.eachDefaultSystem (
      system: let
        pkgs = import nixpkgs {inherit system;};
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

          (pkgs.rPackages.buildRPackage {
            name = "rutilstimflutre";
            src = pkgs.fetchFromGitHub {
              owner = "timflutre";
              repo = "rutilstimflutre";
              rev = "42660be440687c50169acae592cc32e1a11e4255";
              sha256 = "sha256-Au1Izpuht83S4oEhq+1fq4cIrOt+48DbsCoxNvdndi4=";
            };
            propagatedBuildInputs = with pkgs.rPackages; [data_table lme4 Matrix Rcpp];
          })

          /*
          developement packages
          */
          languageserver
          styler
        ];
      in {
        devShells.default = pkgs.mkShell {
          LOCALE_ARCHIVE =
            if "${system}" == "x86_64-linux"
            then "${pkgs.glibcLocalesUtf8}/lib/locale/locale-archive"
            else "";
          LANG = "en_US.UTF-8";
          LC_ALL = "en_US.UTF-8";
          R_LIBS_USER = "''"; # to not use users' installed R packages
          R_ZIPCMD = "${pkgs.zip}/bin/zip";
          TEST_PORT = 3000;
          PLAYWRIGHT_BROWSERS_PATH = pkgs.playwright-driver.browsers;
          PLAYWRIGHT_SKIP_VALIDATE_HOST_REQUIREMENTS = true;
          nativeBuildInputs = [pkgs.bashInteractive];
          shellHook = ''
            npm install --ignore-scripts
          '';
          buildInputs = [
            (Rpkgs.rWrapper.override {packages = R-packages;})
            (Rpkgs.rstudioWrapper.override {packages = R-packages;})
            pkgs.zip
            pkgs.nodejs_20
            (pkgs.playwright-driver.override {nodejs = pkgs.nodejs_20;})
          ];
        };

        apps = {
          default = let
            PlantBreedGame = pkgs.writeShellApplication {
              name = "PlantBreedGame";
              text = ''
                Rscript --vanilla -e "shiny::runApp(port = as.numeric(Sys.getenv('TEST_PORT')))"
              '';
            };
          in {
            type = "app";
            program = "${PlantBreedGame}/bin/PlantBreedGame";
          };

          test_ui = let
            test_ui = pkgs.writeShellApplication {
              name = "test_ui";
              text = ''
                npx playwright test "$@"
              '';
            };
          in {
            type = "app";
            program = "${test_ui}/bin/test_ui";
          };

          unit_tests = let
            unit_tests = pkgs.writeShellApplication {
              name = "unit_tests";
              text = ''
                Rscript --vanilla ${./tests/testthat.R}
              '';
            };
          in {
            type = "app";
            program = "${unit_tests}/bin/unit_tests";
          };

          initialise-data = let
            initialise-data = pkgs.writeShellApplication {
              name = "initialise-data";
              text = ''
                make data
              '';
            };
          in {
            type = "app";
            program = "${initialise-data}/bin/initialise-data";
          };
        };
      }
    );
}
