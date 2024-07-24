{
  config,
  pkgs,
  osConfig,
  ...
}: let
  fonts = osConfig.themes.fonts;
in {
  programs.alacritty = {
    enable = true;
    settings = {
      general.live_config_reload = true;
      window.decorations = "none";
      window.startup_mode = "Windowed";
      scrolling.history = 16384;
      cursor.style = {
        shape = "Beam";
        blinking = "On";
      };
      cursor.thickness = 0.25;
    };
  };
}
