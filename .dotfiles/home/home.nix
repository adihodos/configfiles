{
  lib,
  config,
  pkgs,
  setupOptions,
  ...
}: {
  imports = [
    ./x11.nix
    ./wm/i3/i3.nix
    ./wm/i3/dunst.nix
    ./shells.nix
    ./terminals/alacritty.nix
    ./terminals/kitty.nix
    ./git.nix
    ./devtools.nix
    ./browser.nix
    ./utilities.nix
    ./graphics.nix
  ];
  # Home Manager needs a bit of information about you and the paths it should
  # manage.
  home.username = setupOptions.user.username;
  home.homeDirectory = "/home/" + setupOptions.user.username;

  # This value determines the Home Manager release that your configuration is
  # compatible with. This helps avoid breakage when a new Home Manager release
  # introduces backwards incompatible changes.
  #
  # You should not change this value, even if you update Home Manager. If you do
  # want to update the value, then make sure to first check the Home Manager
  # release notes.
  home.stateVersion = "24.11"; # Please read the comment before changing.

  # The home.packages option allows you to install Nix packages into your
  # environment.
  home.packages = [
    # # Adds the 'hello' command to your environment. It prints a friendly
    # # "Hello, world!" when run.
    # pkgs.hello

    # # It is sometimes useful to fine-tune packages, for example, by applying
    # # overrides. You can do that directly here, just don't forget the
    # # parentheses. Maybe you want to install Nerd Fonts with a limited number of
    # # fonts?
    # (pkgs.nerdfonts.override { fonts = [ "FantasqueSansMono" ]; })

    # # You can also create simple shell scripts directly inside your
    # # configuration. For example, this adds a command 'my-hello' to your
    # # environment:
    # (pkgs.writeShellScriptBin "my-hello" ''
    #		echo "Hello, ${config.home.username}!"
    # '')
  ];

  # Home Manager is pretty good at managing dotfiles. The primary way to manage
  # plain files is through 'home.file'.
  home.file = {
    # # Building this configuration will create a copy of 'dotfiles/screenrc' in
    # # the Nix store. Activating the configuration will then make '~/.screenrc' a
    # # symlink to the Nix store copy.
    # ".screenrc".source = dotfiles/screenrc;

    # # You can also set the file content immediately.
    # ".gradle/gradle.properties".text = ''
    #		org.gradle.console=verbose
    #		org.gradle.daemon.idletimeout=3600000
    # '';
  };

  # Home Manager can also manage your environment variables through
  # 'home.sessionVariables'. If you don't want to manage your shell through Home
  # Manager then you have to manually source 'hm-session-vars.sh' located at
  # either
  #
  #	 ~/.nix-profile/etc/profile.d/hm-session-vars.sh
  #
  # or
  #
  #	 /etc/profiles/per-user/adi/etc/profile.d/hm-session-vars.sh
  #
  home.sessionVariables = {
    EDITOR = setupOptions.system.editor;
    CPM_SOURCE_CACHE = "$HOME/.cache/CPM";
    PAGER = "nvimpager";
  };

  home.file."./.cache/CPM/.keep" = {
    force = true;
    recursive = true;
    text = "monkaS";
  };
  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  dconf.settings = {
    "org/virt-manager/virt-manager/connections" = {
      autoconnect = ["qemu:///system"];
      uris = ["qemu:///system"];
    };
  };
}
