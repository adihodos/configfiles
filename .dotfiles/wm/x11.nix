{ config, pkgs, setupOptions, ... }:

{
  services.dbus = {
    enable = true;
    packages = [ pkgs.dconf ];
  };

  programs.dconf = {
    enable = true;
  };

  services.gnome = {
    gnome-keyring.enable = true;
  };

  services.xserver = {
    enable = true;
    videoDrivers = ["nvidia"];
    xkb = {
      layout = "us";
      variant = "";
      options = "caps:escape";
    };
  };

  # xdg portal is required for screenshare
  xdg.portal = {
    enable = true;
    configPackages = [pkgs.xdg-desktop-portal-gtk];
  };

  environment.systemPackages = with pkgs; [ rxvt-unicode ];
}
