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
    libinput = {
      enable = true;
      mouse.naturalScrolling = false;
    };
  };

  environment.systemPackages = with pkgs; [
    xorg.xmodmap
    xclip
    xsel
  ];

  # xdg portal is required for screenshare
  xdg.portal = {
    enable = true;
    configPackages = [pkgs.xdg-desktop-portal-gtk];
  };
}
