{ config, pkgs, setupOptions, ... }:

{
  # Enable the X11 windowing system.
  services.xserver.enable = true;

  # Enable the XFCE Desktop Environment.
  #services.xserver.displayManager.lightdm.enable = true;
  # services.xserver.desktopManager.xfce.enable = true;

  services.xserver.desktopManager.mate = {
    enable = true;
    #extraPanelApplets = with pkgs.mate; [ mate-applets ];
    #extraCajaExtensions = with pkgs.mate; [ caja-extensions ];
  };

#  home.packages = with pkgs; [
#    material-design-icons
#    marwaita
#  ];

  # Configure keymap in X11
  services.xserver = {
    layout = "us";
    xkbVariant = "";
  };

  # Enable CUPS to print documents.
  services.printing.enable = true;

}
