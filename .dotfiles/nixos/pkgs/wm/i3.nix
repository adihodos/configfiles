# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, lib, pkgs-unstable, setupOptions, ... }:

{
  services.xserver.desktopManager.xterm.enable = false;
  
  services.xserver.displayManager = {
    # lightdm.enable = true;
    # defaultSession = "none+i3";
    sddm.enable = true;
    sddm.theme = "${import ./sddm-theme.nix { inherit pkgs; inherit lib; }}";
  };
  
  services.xserver.windowManager.i3 = {
    enable = true;
    extraPackages = with pkgs; [
      i3status-rust
      i3lock
      i3blocks 
      clipmenu
      rlaunch
      nitrogen
    ];
  };

  environment.systemPackages = with pkgs-unstable; [
    (bumblebee-status.override{plugins = p:[p.cpu p.nic p.load p.dunstctl];})
  ] ++ [pkgs.iw];
}
