{ config, lib, pkgs, ... }:
{
  programs.bash = {
    enable = true;
    shellAliases = {
	    ll = "eza";
      cat = "bat";
    };
  };

  home.sessionVariables = {
    PAGER = "most";
  };

  programs.starship = {
    enable = true;
    enableBashIntegration = true;
    settings = lib.importTOML ./dotfiles/starship/starship.pastel.toml;
  };
}
