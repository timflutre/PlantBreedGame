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
    flake-utils.lib.eachDefaultSystem (system:
        let
          pkgs = import nixpkgs {inherit system;};
          Rpkgs = pkgs;
          R-packages = with Rpkgs.rPackages; [
              shiny
              shinydashboard
              shinycssloaders
              shinyjs
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

          devShells.default =
            pkgs.mkShell {
              LOCALE_ARCHIVE = if "${system}" == "x86_64-linux" then "${pkgs.glibcLocalesUtf8}/lib/locale/locale-archive" else "";
              R_LIBS_USER = "''"; # to not use users' installed R packages
              R_ZIPCMD = "${pkgs.zip}/bin/zip";
              nativeBuildInputs = [pkgs.bashInteractive];
              buildInputs = [
                (Rpkgs.rWrapper.override { packages = R-packages; })
                (Rpkgs.rstudioWrapper.override{ packages = R-packages; })
                pkgs.zip
              ];
            };

          apps = {
            default = let
              PlantBreedGame = pkgs.writeShellApplication {
                name = "PlantBreedGame";
                text = ''
                  Rscript --vanilla -e "shiny::runApp()"
                '';
              };
            in {
              type = "app";
              program = "${PlantBreedGame}/bin/PlantBreedGame";
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

