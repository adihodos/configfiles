# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, setupOptions, ... }:

{
  services.xserver.desktopManager.xterm.enable = false;
  services.xserver.displayManager = {
    lightdm.enable = true;
    defaultSession = "i3";
  };
  
  services.xserver.windowManager.i3 = {
    enable = true;
    extraPackages = with pkgs; [
      i3status-rust
      i3lock
      i3blocks 
      clipmenu
      rlauncher
    ];
    # configFile = ./i3.config;
  };

  home.file.".i3/config".source = ./i3.config;
  
}
