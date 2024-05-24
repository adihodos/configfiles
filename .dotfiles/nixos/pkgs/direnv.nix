{
  config,
  pkgs,
  setupOptions,
  ...
}: {
  programs.direnv = {
    enable = true;
    nix-direnv = {
      enable = true;
    };
  };
}
