{ config, pkgs, setupOptions, ... }:

{
  xsession.windowManager.i3 = {
    enable = true;
    extraConfig = builtins.ReadFile ./../../dotfiles/.config/i3/config;
  };
}
