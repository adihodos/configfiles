{
  config,
  pkgs,
  setupOptions,
  ...
}: {
  programs.kitty = {
    enable = true;
    extraConfig = builtins.readFile ./../dotfiles/kitty/kitty.conf;
    theme = "Gruvbox Material Dark Hard";
  };
}
