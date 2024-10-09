{
  config,
  pkgs,
  setupOptions,
  ...
}: {
  swapDevices = [
    {
      label = "nixos_swap";
    }
  ];
  # Bootloader.
  #boot.kernelPackages = pkgs.linuxPackages_latest;
  boot.kernelPackages = pkgs.linuxPackages_xanmod_latest;
  #pkgs.linuxPackages_zen;
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
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
    package = config.boot.kernelPackages.nvidiaPackages.mkDriver {
      version = "560.35.03";
      sha256_64bit = "sha256-8pMskvrdQ8WyNBvkU/xPc/CtcYXCa7ekP73oGuKfH+M=";
      sha256_aarch64 = "";
      openSha256 = "";
      settingsSha256 = "sha256-kQsvDgnxis9ANFmwIwB7HX5MkIAcpEEAHc8IBOLdXvk=";
      persistencedSha256 = "";
    };
    #package = config.boot.kernelPackages.nvidiaPackages.latest;

    #package = config.boot.kernelPackages.nvidiaPackages.mkDriver {
    #  version = "555.58.02";
    #  sha256_64bit = "sha256-xctt4TPRlOJ6r5S54h5W6PT6/3Zy2R4ASNFPu8TSHKM=";
    #  sha256_aarch64 = "sha256-8hyRiGB+m2hL3c9MDA/Pon+Xl6E788MZ50WrrAGUVuY=";
    #  openSha256 = "sha256-8hyRiGB+m2hL3c9MDA/Pon+Xl6E788MZ50WrrAGUVuY=";
    #  settingsSha256 = "sha256-ZpuVZybW6CFN/gz9rx+UJvQ715FZnAOYfHn5jt5Z2C8=";
    #  persistencedSha256 = "sha256-xctt4TPRlOJ6r5S54h5W6PT6/3Zy2R4ASNFPu8TSHKM=";
    #};

    # Modesetting is required.
    modesetting.enable = true;
    powerManagement.enable = false;
    #powerManagement.finegrained = false;
    open = false;
    nvidiaSettings = false;
  };

  hardware = {
    #nvidia = {

    #  # Nvidia power management. Experimental, and can cause sleep/suspend to fail.
    #  # Enable this if you have graphical corruption issues or application crashes after waking
    #  # up from sleep. This fixes it by saving the entire VRAM memory to /tmp/ instead
    #  # of just the bare essentials.

    #  # Fine-grained power management. Turns off GPU when not in use.
    #  # Experimental and only works on modern Nvidia GPUs (Turing or newer).

    #  # Use the NVidia open source kernel module (not to be confused with the
    #  # independent third-party "nouveau" open source driver).
    #  # Support is limited to the Turing and later architectures. Full list of
    #  # supported GPUs is at:
    #  # https://github.com/NVIDIA/open-gpu-kernel-modules#compatible-gpus
    #  # Only available from driver 515.43.04+
    #  # Currently alpha-quality/buggy, so false is currently the recommended setting.

    #  # Enable the Nvidia settings menu,
    #  # accessible via `nvidia-settings`.

    #  # Optionally, you may need to select the appropriate driver version for your specific GPU.
    #  package = config.boot.kernelPackages.nvidiaPackages.stable;

    #  #open = true;
    #  #powerManagement.enable = true;
    #  #modesetting.enable = true;
    #};

    opengl = {
      enable = true;
      driSupport = true;
      driSupport32Bit = true;
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
  sound.enable = true;
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
