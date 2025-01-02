{
  config,
  lib,
  pkgs,
  ...
}: {
  programs.eza = {
    enable = true;
    extraOptions = [
      "--oneline"
      "--group-directories-first"
      "--header"
      "--long"
      "--color=always"
      "--icons=always"
    ];
  };

  programs.bat = {
    enable = true;
  };

  home.packages = with pkgs; [
    btop
    bitwise
    base16-shell-preview
    #neofetch
    fastfetch
    du-dust
    tlrc
    ripgrep
    zenith
    dfc
    bandwhich
    procs
    dua
    fzf
    skim
    fd
    far2l
    libsForQt5.kate
    gamepad-tool
    mpv
    vlc
    chafa
    viu
    pdfgrep
    pdf4qt
    sioyek
    nvimpager
    hwinfo
    pkgtop
    uroboros
  ];
}
