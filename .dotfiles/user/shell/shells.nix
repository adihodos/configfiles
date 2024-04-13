{ config, lib, pkgs, ... }:
{
  programs.bash = {
    enable = true;
    shellAliases = {
	    ll = "eza";
    };
  };

  programs.starship = {
    enable = true;
    settings = lib.importTOML ./starship.pastel.toml;
  };

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
    pastel
    mc
    bitwise
    neofetch
    du-dust
    tlrc
  ];
}
