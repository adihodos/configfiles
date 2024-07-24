{
  config,
  lib,
  pkgs,
  ...
}: {
  # 3d/painting
  home.packages = with pkgs; [
    gimp
    blender
    feh
    imagemagick
    krita
    inkscape-with-extensions
    flameshot
    pastel
    texturepacker
    #rucksack
    awesomebump
    # pdfstudioviewer
    libsForQt5.okular
  ];
}
