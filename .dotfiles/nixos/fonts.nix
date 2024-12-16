{
  pkgs,
  config,
  lib,
  ...
}: {
  fonts = {
    packages = with pkgs; [
      (nerdfonts.override {
        fonts = [
          "Iosevka"
          "RobotoMono"
          "EnvyCodeR"
          "JetBrainsMono"
          "SpaceMono"
          "VictorMono"
          "ZedMono"
          "GeistMono"
        ];
      })
      # input-fonts
      roboto
      roboto-serif
      ibm-plex
      nerdfonts
      material-design-icons
      material-icons
      fira-code
      fira-code-symbols
      iosevka-comfy.comfy
    ];

    fontconfig = let
      fonts = config.themes.fonts;
    in {
      enable = lib.mkForce true;
      defaultFonts = {
        monospace = ["${fonts.mono.family} ${toString fonts.mono.size}"];
        sansSerif = ["${fonts.main.family} ${toString fonts.main.size}"];
        serif = ["${fonts.serif.family} ${toString fonts.serif.size}"];
      };
    };
    enableDefaultPackages = true;
  };

  themes.fonts = {
    main = {
      family = "IBM Plex Sans";
      size = lib.mkDefault 13;
    };
    serif = {
      family = "IBM Plex Serif";
      size = lib.mkDefault 13;
    };
    mono = {
      family = "Iosevka Nerd Font Mono";
      size = lib.mkDefault 16;
    };
    propo = {
      family = "Iosevka Nerd Font Propo";
      size = lib.mkDefault 14;
    };
  };
}
