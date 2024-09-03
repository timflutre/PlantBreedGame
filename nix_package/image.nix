{
  nix2containerPkgs,
  pkgs,
}: {
  builder = {
    imageName ? null,
    plantBreedGame ? null,
    tag ? "latest",
  }: let
    setTimezone = pkgs.runCommand "setTimezone" {} ''
      mkdir -p $out/etc
      ln -s ${pkgs.tzdata}/share/zoneinfo/Asia/Tokyo $out/etc/localtime
    '';
  in
    nix2containerPkgs.nix2container.buildImage {
      name = imageName;
      tag = tag;
      maxLayers = 100;

      # force plantBreedGame and its dependencies to be in different layers
      layers = [
        # dependencies layer
        (nix2containerPkgs.nix2container.buildLayer {
          deps = plantBreedGame.buildInput;
          maxLayers = 90;
        })
        # plantBreedGame layer
        (nix2containerPkgs.nix2container.buildLayer {
          deps = plantBreedGame.buildInput;
        })
      ];

      copyToRoot = [
        (pkgs.buildEnv {
          name = "root";
          paths = with pkgs;
          with pkgs.dockerTools; [
            plantBreedGame
            setTimezone

            fakeNss
            tzdata

            bashInteractive
            coreutils
            usrBinEnv
            binSh
          ];
          ignoreCollisions = false;
          pathsToLink = ["/bin" "/var" "/run" "/tmp" "/etc" "/share"];
        })
      ];

      config = {
        WorkingDir = "/";
        Run = ["touch" "/toto.txt"];
        Volume = "/var/lib/plantBreedGame";
        Cmd = ["${plantBreedGame}/bin/plantBreedGame" "--host" "0.0.0.0" "--port" "3838"];
        Expose = 3838;
      };
    };
}
