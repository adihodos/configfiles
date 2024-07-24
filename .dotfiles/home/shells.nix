{
  config,
  lib,
  pkgs,
  ...
}: {
  programs.bash = {
    enable = true;
    shellAliases = {
      ll = "eza";
      cat = "bat";
    };
  };

  programs.starship = {
    enable = true;
    enableBashIntegration = true;
    settings = lib.importTOML ./dotfiles/starship/starship.pastel.toml;
  };
}
