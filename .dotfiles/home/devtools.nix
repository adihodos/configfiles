{ config, pkgs, setupOptions, ... }:
{
  programs.direnv = {
    enable = true;
    nix-direnv = {
      enable = true;
    };
  };
  
  home.packages = with pkgs; [
  ];
}
