{ config, pkgs, setupOptions, lib, ... }:
let
  mod = "Mod4";
in
{
  programs.feh.enable = true;

  xdg.mimeApps.defaultApplications = {
    "image/bmp" = lib.mkForce "feh.desktop";
    "image/gif" = lib.mkForce "feh.desktop";
    "image/jpeg" = lib.mkForce "feh.desktop";
    "image/jpg" = lib.mkForce "feh.desktop";
    "image/pjpeg" = lib.mkForce "feh.desktop";
    "image/png" = lib.mkForce "feh.desktop";
    "image/tiff" = lib.mkForce "feh.desktop";
    "image/webp" = lib.mkForce "feh.desktop";
    "image/x-bmp" = lib.mkForce "feh.desktop";
    "image/x-pcx" = lib.mkForce "feh.desktop";
    "image/x-png" = lib.mkForce "feh.desktop";
    "image/x-portable-anymap" = lib.mkForce "feh.desktop";
    "image/x-portable-bitmap" = lib.mkForce "feh.desktop";
    "image/x-portable-graymap" = lib.mkForce "feh.desktop";
    "image/x-portable-pixmap" = lib.mkForce "feh.desktop";
    "image/x-tga" = lib.mkForce "feh.desktop";
    "image/x-xbitmap" = lib.mkForce "feh.desktop";
  };

  services = {
    # caffeine.enable = true;
    flameshot.enable = true;
    # picom.enable = true;
  };

  systemd.user = {
    targets.i3-session = {
      Unit = {
        Description = "i3 session";
        Documentation = [ "man:systemd.special(7)" ];
        BindsTo = [ "graphical-session.target" ];
        Wants = [ "graphical-session-pre.target" ];
        After = [ "graphical-session-pre.target" ];
      };
    };

    services = {
      # caffeine.Install.WantedBy = lib.mkForce [ "i3-session.target" ];
      # feh = {
      #   Unit = {
      #     Description = "feh background";
      #     PartOf = [ "i3-session.target" ];
      #     # After = [ "xrandr.service" "picom.service" ];
      #   };
      #
      #   Service = {
      #     ExecStart = "${pkgs.feh}/bin/feh --bg-fill ${config.xdg.dataHome}/wall.png";
      #     RemainAfterExit = true;
      #     Type = "oneshot";
      #   };
      #   Install.WantedBy = [ "i3-session.target" ];
      # };

      flameshot.Install.WantedBy = lib.mkForce [ "i3-session.target" ];
    };
  };

  xsession.windowManager.i3 = {
    enable = true;

    config = {
      modifier = mod;

      fonts = {
        names = [ setupOptions.user.font.propo ];
        style = "Regular";
        size = 10.0;
      };

      startup = [
        # { command = "systemctl --user restart polybar"; always = true; notification = false; }
        { command = "dunst"; always = true; notification = false; }
        { command = "numlockx on"; always = true; notification = false; }
      ];

      keybindings = lib.mkOptionDefault {
        "${mod}+Shift+c" = "reload";
        "${mod}+Shift+r" = "restart";
        "${mod}+u" = "border none";
        "${mod}+Return" = "exec kitty";
        "${mod}+Shift+q" = "kill";
        # Hide the bar
        "${mod}+h" = "bar mode toggle";

        "${mod}+d" = "exec ${pkgs.rlaunch}/bin/rlaunch -f \"${setupOptions.user.font.propo}\" -h 32 -t kitty";

        # switch to workspace
        "${mod}+1" = "workspace $terms";
        "${mod}+2" = "workspace $web";
        "${mod}+3" = "workspace $multimedia";
        "${mod}+4" = "workspace $design";
        "${mod}+5" = "workspace $office";
        "${mod}+6" = "workspace $knowledge";
        "${mod}+7" = "workspace $games";
        "${mod}+8" = "workspace 8";
        "${mod}+9" = "workspace 9";
        "${mod}+0" = "workspace 10";

        # move focused container to workspace
        "${mod}+Shift+1" = "move container to workspace $terms";
        "${mod}+Shift+2" = "move container to workspace $web";
        "${mod}+Shift+3" = "move container to workspace $multimedia";
        "${mod}+Shift+4" = "move container to workspace $design";
        "${mod}+Shift+5" = "move container to workspace $office";
        "${mod}+Shift+6" = "move container to workspace $knowledge";
        "${mod}+Shift+7" = "move container to workspace $games";
        "${mod}+Shift+8" = "move container to workspace 8";
        "${mod}+Shift+9" = "move container to workspace 9";
        "${mod}+Shift+0" = "move container to workspace 10";

        # "${mod}+x" = "exec sh -c '${pkgs.maim}/bin/maim -s | xclip -selection clipboard -t image/png'";

        # "${mod}+Shift+x" = "exec sh -c '${pkgs.i3lock}/bin/i3lock -c 222222 & sleep 5 && xset dpms force of'";

        # # alternatively, you can use the cursor keys:
        # "${mod}+Left" = "focus left";
        # "${mod}+Down" = "focus down";
        # "${mod}+Up" = "focus up";
        # "${mod}+Right" = "focus right";

        # move focused window
        "${mod}+Shift+h" = "move left";
        "${mod}+Shift+j" = "move down";
        "${mod}+Shift+k" = "move up";
        "${mod}+Shift+l" = "move right";

        # alternatively, you can use the cursor keys:
        "${mod}+Shift+Left" = "move left";
        "${mod}+Shift+Down" = "move down";
        "${mod}+Shift+Up" = "move up";
        "${mod}+Shift+Right" = "move right";

        # split in horizontal orientation
        "${mod}+Alt+v" = "split v";

        # split in vertical orientation
        "${mod}+Alt+h" = "split h";

        # enter fullscreen mode for the focused container
        "${mod}+f" = "fullscreen toggle";

        # change container layout (stacked, tabbed, toggle split)
        "${mod}+s" = "layout stacking";
        "${mod}+w" = "layout tabbed";
        "${mod}+e" = "layout toggle split";

        # toggle tiling / floating
        "${mod}+Shift+space" = "floating toggle";
        "${mod}+r" = "mode \"resize\"";

        # focus the parent container
        "${mod}+p" = "focus parent";
        "${mod}+c" = "focus child";
      };

    #   bars = [
    #     {
    #       position = "bottom";
    #       statusCommand = "${pkgs.i3status-rust}/bin/i3status-rs ${./i3status-rust.toml}";
    #     }
    #   ];
      # };

      focus = {
        # Mouse doesn't jump from one screen to another
        mouseWarping = false;
      };

      workspaceAutoBackAndForth = true;
      workspaceLayout = "default";
    };

    extraConfig = builtins.readFile ./../../dotfiles/.config/i3/config;
  };
}
