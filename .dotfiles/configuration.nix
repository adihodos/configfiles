# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, setupOptions, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
      ./wm/x11.nix
      ./sys/sys.nix
    ];

  nix.package = pkgs.nixFlakes;
  nix.extraOptions = ''
                   experimental-features = nix-command flakes
             '';

  # Bootloader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.hostName = setupOptions.system.hostname; # Define your hostname.
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Enable networking
  networking.networkmanager.enable = true;

  # Set your time zone.
  time.timeZone = setupOptions.system.timezone;

  # Select internationalisation properties.
  i18n.defaultLocale = setupOptions.system.locale;

  i18n.extraLocaleSettings = {
    LC_ADDRESS = setupOptions.system.extraLocale;
    LC_IDENTIFICATION = setupOptions.system.extraLocale;
    LC_MEASUREMENT = setupOptions.system.extraLocale;
    LC_MONETARY = setupOptions.system.extraLocale;
    LC_NAME = setupOptions.system.extraLocale;
    LC_NUMERIC = setupOptions.system.extraLocale;
    LC_PAPER = setupOptions.system.extraLocale;
    LC_TELEPHONE = setupOptions.system.extraLocale;
    LC_TIME = setupOptions.system.extraLocale;
  };


  # Enable sound with pipewire.
  sound.enable = true;
  hardware.pulseaudio.enable = false;
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
    # If you want to use JACK applications, uncomment this
    #jack.enable = true;

    # use the example session manager (no others are packaged yet so this is enabled by default,
    # no need to redefine it in your config for now)
    #media-session.enable = true;
  };

  # Enable touchpad support (enabled default in most desktopManager).
  # services.xserver.libinput.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.${setupOptions.user.username} = {
    isNormalUser = true;
    description = setupOptions.user.name;
    extraGroups = [ "networkmanager" "wheel" "input" ];
    packages = with pkgs; [];
    uid = 1024;
  };

  # nixpkgs.config.input-fonts.acceptLicense = true;

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    #  vim # Do not forget to add an editor to edit configuration.nix! The Nano editor is also installed by default.
    wget
    p7zip
	  emacs29-gtk3
    unzip
    wget
    curl
    git
    mc
  ];

  fonts.fontDir.enable = true;
  fonts.packages = with pkgs; [
 	  (nerdfonts.override { fonts = [ "Iosevka" "RobotoMono" "EnvyCodeR" "JetBrainsMono" "SpaceMono" "VictorMono" ]; })
    # input-fonts
	  roboto
	  roboto-serif
  ];

  fonts.fontconfig = {
	  defaultFonts.monospace = [ "Iosevka Nerd Font Mono Medium" ];
  };
  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = {
  #   enable = true;
  #   enableSSHSupport = true;
  # };

  # List services that you want to enable:

  # Enable the OpenSSH daemon.
  # services.openssh.enable = true;

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "23.11"; # Did you read the comment?

  nix.settings.experimental-features = [ "nix-command" "flakes" ];
  virtualisation.virtualbox.guest = {
	  enable = true;
	  x11 = true;
  };

}
