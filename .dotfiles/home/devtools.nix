{
  config,
  pkgs,
  setupOptions,
  inputs,
  ...
}: {
  programs.direnv = {
    enable = true;
    nix-direnv = {
      enable = true;
    };
  };

  home.packages = with pkgs; [
    alejandra
    clang-tools_17
    man-pages
    man-pages-posix
    stdman
    kdiff3
    helix
    inputs.neovim-nightly-overlay.packages.${pkgs.system}.default
    neovide
    luaformatter
    stylua
    qemu_full
    quickemu
    virt-manager
    procps
  ];
}
