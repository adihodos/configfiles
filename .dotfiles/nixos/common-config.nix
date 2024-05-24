# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).
{
  config,
  pkgs,
  setupOptions,
  ...
}: {
  boot = {
    # See console messages during early boot.
    initrd.kernelModules = ["fbcon"];

    # Disable console blanking after being idle.
    kernelParams = ["consoleblank=0"];

    # Clean /tmp on boot
    cleanTmpDir = true;
  };

  # Increase the amount of inotify watchers
  # Note that inotify watches consume 1kB on 64-bit machines.
  boot.kernel.sysctl = {
    "fs.inotify.max_user_watches" = 1048576; # default:  8192
    "fs.inotify.max_user_instances" = 1024; # default:   128
    "fs.inotify.max_queued_events" = 32768; # default: 16384
  };

  i18n.defaultLocale = "en_US.UTF-8";
  console = {
    earlySetup = true;
    font = "${pkgs.terminus_font}/share/consolefonts/ter-132n.psf.gz";
    packages = with pkgs; [terminus_font];
    keyMap = "us";
  };
}
