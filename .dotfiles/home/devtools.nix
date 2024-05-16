{ config, pkgs, setupOptions, ... }:
{
  programs.direnv = {
    enable = true;
    nix-direnv = {
      enable = true;
    };
  };
  
  home.packages = with pkgs; [
    clang-tools_17
    man-pages
    man-pages-posix
    stdman
    kdiff3
    helix
    neovim
    lunarvim
    neovide
    gnvim
  ];
}
