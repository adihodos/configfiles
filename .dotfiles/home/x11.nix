{
  config,
  pkgs,
  osConfig,
  setupOptions,
  ...
}: {
  programs.urxvt = {
    enable = true;
  };
}
