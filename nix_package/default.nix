{
  lib,
  pkgs,
  stdenv,
  runCommand,
  src,
  R_deps,
  tests_R_deps,
  ...
}:
let
  version = "v" + builtins.readFile ../VERSION;
  R_with_packages = pkgs.rWrapper.override {
    packages = R_deps;
  };
  R_with_packages_test = pkgs.rWrapper.override {
    packages = R_deps ++ tests_R_deps;
  };
in
pkgs.stdenv.mkDerivation (finalAttrs: rec {
  pname = "plantBreedGame";
  inherit version src;

  meta = with lib; {
    description = "Serious game to teach selective breeding via a web application.";
    mainProgram = "plantBreedGame";
    longDescription = ''
      Software implementing a serious game to teach selective breeding via the example of a fictitious annual plant species to students at the master level. It takes the form of a R-Shiny application (ie. web application), benefiting from the R programming language and software environment for statistical computing.
    '';
    homepage = "https://github.com/timflutre/PlantBreedGame";
    changelog = "https://github.com/timflutre/PlantBreedGame/commits/${version}/";
    license = licenses.agpl3Plus;
    maintainers = [
      "breedinggame@julien-diot.com"
      "timothee.flutre@inrae.fr"
    ];
    platforms = platforms.all;
  };

  doCheck = true;

  buildInput = [
    R_with_packages
    pkgs.pandoc
    pkgs.coreutils
    pkgs.zip
  ];

  nativeBuildInputs = [
    R_with_packages_test
    pkgs.makeWrapper
  ];

  buildPhase = ''
    mkdir -p build/app
    mkdir -p build/app/src
    cp -r ./src/* build/app/src/.

    mkdir -p build/app
    mkdir -p build/app/www
    cp -r ./www/* build/app/www/.

    mkdir -p build/app/tests
    cp -r ./tests/* build/app/tests/.

    cp ./global.R build/app/.
    cp ./server.R build/app/.
    cp ./ui.R build/app/.

    cp ./README.md build/app/.
    cp ./AUTHORS build/app/.
    cp ./COPYING build/app/.
    cp ./VERSION build/app/.
  '';

  checkPhase = ''
    export LC_ALL=C.UTF-8
    (cd build/app && ${R_with_packages_test}/bin/Rscript --vanilla ./tests/testthat.R)
  '';

  installPhase = ''
    runHook preInstall

    cp -r ./build $out
    mkdir -p $out/bin
    rm -rf $out/app/tests

    cat <<EOF > $out/bin/plantBreedGame
    #!${pkgs.bash}/bin/bash


    help() {
        echo "Usage: plantBreedGame [options]"
        echo ""
        echo "Options:"
        echo "  --host <hostname>   The IPv4 address that the application should listen on."
        echo "  --port <port>       TCP port that the application should listen on."
        echo "  --help              Display this help message."
    }

    HOST="0.0.0.0"
    PORT="3838"

    while [ "\$1" != "" ]; do
      case "\$1" in
        --host)
            shift
            HOST="\$1"
            ;;
        --port)
            shift
            if ! [[ "\$1" =~ ^[0-9]+\$ ]] || [ "\$1" -lt 1 ] || [ "\$1" -gt 65535 ]; then
              echo "Error: --port must be a number between 1 and 65535"
              exit 1
            fi
            PORT="\$1"
            ;;
        --help)
            help
            exit 0
            ;;
        --)
            shift
            break
            ;;
        *)
            echo "Error: Unkown argument \"\$1\""
            help
            exit 1
            ;;
      esac
      shift
    done

    if [ -z "\''$PLANTBREEDGAME_DATA_ROOT" ]; then
      if [ -z "\''$XDG_DATA_HOME" ]; then
          XDG_DATA_HOME="\''$HOME/.local/share"
      fi
      PLANTBREEDGAME_DATA_ROOT="\''$XDG_DATA_HOME/plantBreedGame"
    fi

    export PLANTBREEDGAME_DATA_ROOT
    export R_LIBS_USER="'''"
    export R_ZIPCMD="zip";
    export LANG="C.UTF-8"
    export LC_ALL="C.UTF-8"

    if [[ ! -d \''${PLANTBREEDGAME_DATA_ROOT} ]];then
      echo "plantBreedGame data folder does not exist, create one at \''${PLANTBREEDGAME_DATA_ROOT}"
      mkdir -p "\''${PLANTBREEDGAME_DATA_ROOT}"
    fi

    echo "VERSION: \''$(cat $out/app/VERSION)"
    echo "data: \''$PLANTBREEDGAME_DATA_ROOT"
    echo "Host: \''$HOST"
    echo "Port: \''$PORT"

    Rscript --vanilla -e 'setwd("$out/app"); shiny::runApp(appDir = "$out/app", port = as.numeric('"\''${PORT}"'), host = "'"\''${HOST}"'")'
    EOF

    chmod +x $out/bin/plantBreedGame

    runHook postInstall
  '';

  postFixup = ''
    wrapProgram $out/bin/plantBreedGame \
      --set PATH ${lib.makeBinPath buildInput}
  '';

  passthru.tests =
    runCommand "plantBreedGame-test"
      {
        nativeBuildInputs = [ finalAttrs.finalPackage ];
      }
      ''
        # Necessary for the derivation to be successful
        mkdir $out
        echo "This currently do nothing"
        echo "Test: DONE"
      '';
})
