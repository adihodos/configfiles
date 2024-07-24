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
    neofetch
    du-dust
    tlrc
    ripgrep
    zenith
    dfc
    bandwhich
    procs
    most
    dua
    fzf
    bitwise
    skim
    gf
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
    zathura
    sioyek
    nvimpager
  ];

  home.sessionVariables = {
    PAGER = "nvimpager";
  };
}
