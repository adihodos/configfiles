{ config, pkgs, setupOptions, ... }:

{
  programs.urxvt = {
    enable = true;
    
    fonts = [
      "xft:${setupOptions.user.font.mono}:style=Regular:size=14"
      "xft:${setupOptions.user.font.mono}:style=Bold:size=14"
      "xft:${setupOptions.user.font.mono}:style=Italic:size=14"
      "xft:${setupOptions.user.font.mono}:style=Bold Italic:size=14"
    ];
    
    extraConfig = {
      "foreground" = "#93a1a1";
      "background" = "#141c21";
      "cursorColor" = "#afbfbf";

      "color0" = "#263640";
      "color8" = "#4a697d";

      "color1" = "#d12f2c";
      "color9" = "#fa3935";

      "color2" = "#819400";
      "color10" = "#a4bd00";

      "color3" = "#b08500";
      "color11" = "#d9a400";

      "color4" = "#2587cc";
      "color12" = "#2ca2f5";

      "color5" = "#696ebf";
      "color13" = "#8086e8";

      "color6" = "#289c93";
      "color14" = "#33c5ba";

      "color7" = "#bfbaac";
      "color15" = "#fdf6e3";

      "letterSpace" = "0";
      "lineSpace" = "0";
      "geometry" = "92x24";
      "internalBorder" = "24";
      "cursorBlink" = "true";
      "cursorUnderline" = "false";
      "saveline" = "2048";
      "scrollBar" = "false";
      "scrollBar_right" = "false";
      "urgentOnBell" = "true";
      "depth" = "24";
    };
  };
}
