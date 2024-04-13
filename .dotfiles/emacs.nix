{ config, pkgs, setupOptions, ... }:

{
  services.emacs = {
    enable = true;
    package = pkgs.emacs-unstable;
    defaultEditor = true;
  };
}
