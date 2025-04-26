{
  config,
  pkgs,
  setupOptions,
  ...
}: {
  # Bootloader.
  #boot.kernelPackages = pkgs.linuxPackages_latest;
  boot.kernelPackages = pkgs.linuxPackages_xanmod_latest;
  #pkgs.linuxPackages_zen;
  # boot.loader.systemd-boot.enable = true;
  # boot.loader.efi.canTouchEfiVariables = true;

  # boot.loader.systemd-boot = {
  #   enable = true;
  #   editor = false;
  # };
  boot.loader.efi = {
    canTouchEfiVariables = false;
  };
  boot.loader.grub = {
    enable = true;
    copyKernels = true;
    # efiInstallAsRemovable = true;
    # efiSupport = true;
    fsIdentifier = "uuid";
    splashMode = "stretch";
    # version = 2;
    device = "nodev";
    extraEntries = ''
      menuentry "Reboot" {
        reboot
      }
      menuentry "Poweroff" {
        halt
      }
    '';
  };

  boot.supportedFilesystems = ["ntfs"];
  boot.kernel.sysctl."kernel.yama.ptrace_scope" = 0;

  fileSystems."/mnt/windows/d_drive" = {
    device = "/dev/disk/by-uuid/8EDC7875DC785A03";
    fsType = "ntfs-3g";
    options = ["r" "uid=1000"];
  };

  # boot.kernelModules = ["uinput"];
  boot.kernel.sysctl = {
    "kernel.sysrq" = 1; # SysRQ for is rebooting their machine properly if it freezes: SOURCE: https://oglo.dev/tutorials/sysrq/index.html
    "net.core.rmem_default" = 16777216; # Default socket receive buffer size, improve network performance & applications that use sockets
    "net.core.rmem_max" = 16777216; # Maximum socket receive buffer size, determin the amount of data that can be buffered in memory for network operations
    "net.core.wmem_default" = 16777216; # Default socket send buffer size, improve network performance & applications that use sockets
    "net.core.wmem_max" = 16777216; # Maximum socket send buffer size, determin the amount of data that can be buffered in memory for network operations
    "net.ipv4.tcp_keepalive_intvl" = 30; # TCP keepalive interval between probes, TCP keepalive probes, which are used to detect if a connection is still alive.
    "net.ipv4.tcp_keepalive_probes" = 5; # TCP keepalive probes, TCP keepalive probes, which are used to detect if a connection is still alive.
    "net.ipv4.tcp_keepalive_time" = 300; # TCP keepalive interval (seconds), TCP keepalive probes, which are used to detect if a connection is still alive.
    "vm.dirty_background_bytes" = 268435456; # 256 MB in bytes, data that has been modified in memory and needs to be written to disk
    "vm.dirty_bytes" = 1073741824; # 1 GB in bytes, data that has been modified in memory and needs to be written to disk
    "vm.min_free_kbytes" = 65536; # Minimum free memory for safety (in KB), can help prevent memory exhaustion situations
    "vm.swappiness" = 1; # how aggressively the kernel swaps data from RAM to disk. Lower values prioritize keeping data in RAM,
    "vm.vfs_cache_pressure" = 50; # Adjust vfs_cache_pressure (0-1000), how the kernel reclaims memory used for caching filesystem objects
  };

  #boot.extraModprobeConfig = ''
  #  options snd slots=snd-hda-intel
  #'';
  boot.blacklistedKernelModules = ["snd_pcsp"];

  hardware.nvidia = {
    # works
    # package = config.boot.kernelPackages.nvidiaPackages.mkDriver {
    #   version = "560.35.03";
    #   sha256_64bit = "sha256-8pMskvrdQ8WyNBvkU/xPc/CtcYXCa7ekP73oGuKfH+M=";
    #   sha256_aarch64 = "";
    #   openSha256 = "";
    #   settingsSha256 = "sha256-kQsvDgnxis9ANFmwIwB7HX5MkIAcpEEAHc8IBOLdXvk=";
    #   persistencedSha256 = "";
    # };
    package = config.boot.kernelPackages.nvidiaPackages.mkDriver {
      version = "570.86.16"; # use new 570 drivers
      sha256_64bit = "sha256-RWPqS7ZUJH9JEAWlfHLGdqrNlavhaR1xMyzs8lJhy9U=";
      openSha256 = "sha256-DuVNA63+pJ8IB7Tw2gM4HbwlOh1bcDg2AN2mbEU9VPE=";
      settingsSha256 = "sha256-9rtqh64TyhDF5fFAYiWl3oDHzKJqyOW3abpcf2iNRT8=";
      usePersistenced = false;
    };
    # package = config.boot.kernelPackages.nvidiaPackages.latest;

    # Modesetting is required.
    modesetting.enable = true;
    powerManagement.enable = false;
    #powerManagement.finegrained = false;
    open = false;
    nvidiaSettings = false;
  };
  #
  hardware = {
    graphics = {
      enable = true;
      extraPackages = with pkgs; [
        vaapiVdpau
        libvdpau-va-gl
        nvidia-vaapi-driver
        #amdvlk
        #rocmPackages.clr.icd
      ];
    };
    #pulseaudio.support32Bit = true;
  };

  # Enable CUPS to print documents.
  services.printing.enable = true;
  services.udev.packages = [pkgs.game-devices-udev-rules];
  hardware.uinput.enable = true;

  # Enable sound with pipewire.
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
    # If you want to use JACK applications, uncomment this
    # jack.enable = true;

    # use the example session manager (no others are packaged yet so this is enabled by default,
    # no need to redefine it in your config for now)
    #media-session.enable = true;
  };

  systemd.tmpfiles.rules = [
    "L+    /opt/rocm/hip   -    -    -     -    ${pkgs.rocmPackages.clr}"
  ];

  environment.variables = {
    ROC_ENABLE_PRE_VEGA = "1";
  };
}
