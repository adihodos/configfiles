{
  config,
  pkgs,
  osConfig,
  pkgs-unstable,
  setupOptions,
  lib,
  ...
}: let
  mod = "Mod4";
in {
  programs.feh.enable = true;

  programs.rofi = {
    enable = true;

    plugins = with pkgs; [
      rofi-calc
      rofi-emoji
      rofi-power-menu
    ];

    location = "top";
    extraConfig = {
      modi = "calc,combi,drun,emoji,run,ssh,power-menu";
      show-icons = true;
      sort = true;
      kb-cancel = "Escape,Super+space";
    };
  };

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
        Documentation = ["man:systemd.special(7)"];
        BindsTo = ["graphical-session.target"];
        Wants = ["graphical-session-pre.target"];
        After = ["graphical-session-pre.target"];
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
      # };

      #
      #   Service = {
      #     ExecStart = "${pkgs.feh}/bin/feh --bg-fill ${config.xdg.dataHome}/wall.png";
      #     RemainAfterExit = true;
      #     Type = "oneshot";
      #   };
      #   Install.WantedBy = [ "i3-session.target" ];
      # };

      flameshot.Install.WantedBy = lib.mkForce ["i3-session.target"];
    };
  };

  xsession = {
    numlock.enable = true;
  };

  xsession.windowManager.i3 = {
    enable = true;

    config = {
      modifier = mod;

      startup = [
        # { command = "systemctl --user restart polybar"; always = true; notification = false; }
        {
          command = "dunst";
          always = true;
          notification = false;
        }
        {
          command = "numlockx on";
          always = true;
          notification = false;
        }
        {
          command = "redshift";
          always = true;
          notification = false;
        }
      ];

      keybindings = lib.mkOptionDefault {
        "${mod}+Shift+c" = "reload";
        "${mod}+Shift+r" = "restart";
        "${mod}+u" = "border none";
        "${mod}+Return" = "exec kitty";
        "${mod}+Shift+q" = "kill";
        # Hide the bar
        "${mod}+Shift+t" = "bar mode toggle";
        #launcher
        #"${mod}+d" = "exec ${pkgs.rlaunch}/bin/rlaunch -h 32 -t kitty";
        #flameshot
        "${mod}+d" = "exec ${pkgs.rofi}/bin/rofi -modi drun -show drun";
        #"${mod}+Shift+e" = "exec ${pkgs.rofi-power-menu}/bin/rofi-power-menu -show power-menu -modi power-menu:rofi-power-menu";
        "${mod}+Print" = "exec ${pkgs.flameshot}/bin/flameshot gui";
        "${mod}+Shift+Print" = "exec ${pkgs.flameshot}/bin/flameshot launcher";

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
        "${mod}+v" = "split v";
        # split in vertical orientation
        "${mod}+h" = "split h";

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

      focus = {
        # Mouse doesn't jump from one screen to another
        mouseWarping = false;
      };

      workspaceAutoBackAndForth = true;
      workspaceLayout = "default";

      bars = [
        {
          statusCommand = let
            cmd = "${pkgs.bumblebee-status}/bin/bumblebee-status";
            # theme = "gruvbox-powerline";
            theme = "iceberg-rainbow";
          in "${cmd} -m cpu memory disk:root nic date time \\
      -p root.path=/ time.format=\"%H:%M CW %V\" \\
      date.format=\"%a, %b %d %Y\" -t ${theme}";

          fonts = lib.mkForce {
            names = ["Iosevka Comfy"];
            style = "SemiBold";
            size = 14.0;
          };

          mode = "dock";
          position = "bottom";
          trayOutput = null;
        }
      ];
    };

    extraConfig = ''
      gaps inner 2
      gaps outer 2

      set $terms "1: terms"
      set $web "2: web"
      set $multimedia "3: multi"
      set $design "4: design"
      set $office "5: office"
      set $knowledge "6: knowledge"
      set $games "7: Games"

      assign [class="firefox" instance="Navigator"] → $web
      assign [class="Brave-browser" instance="brave-browser"] → $web
      assign [class="Min" instance="min"] → $web
      assign [class="Chromium" instance="chromium"] → $web
      assign [class="Slack" instance="slack"] → $web
      assign [class="mpv" instance="gl"] → $multimedia
      assign [class="Easytag" instance="easytag"] → $multimedia
      assign [class="Deadbeef" instance="deadbeef"] → $multimedia
      assign [class="Lutris" instance="lutris"] → $games
      assign [class="Steam" instance="Steam"] → $games
      assign [class="Thunderbird" instance="Mail"] → $office
      assign [class="Thunderbird" instance="Msgcompose"] → $office
      assign [class="Nextcloud" instance="nextcloud"] → $office
      assign [class="digikam" instance="digikam"] → $design
      assign [class="Darktable" instance="darktable"] → $design
      assign [class="Gimp" instance="gimp"] → $design
      assign [class="Pinta" instance="pinta"] → $design
      assign [class="scribus" instance="scribus"] → $design
      assign [class="Inkscape" instance="org.inkscape.Inkscape"] → $design
      assign [class="Joplin" instance="joplin"] → $office
      assign [class="calibre" instance="calibre-gui"] → $office
      assign [instance="FreeMind"] → $knowledge
      assign [instance="libreoffice-calc"] → $knowledge
      assign [class="VirtualBox Manager" instance="virtualBox Manager"] → $vm
      assign [class="VirtualBox Machine" instance="virtualBox Machine"] → $vm

      mode "resize" {

        # Pressing left will shrink the window’s width.
        # Pressing right will grow the window’s width.
        # Pressing up will shrink the window’s height.
        # Pressing down will grow the window’s height.
        bindsym h resize shrink width 4 px or 4 ppt
        bindsym j resize grow height 4 px or 4 ppt
        bindsym k resize shrink height 4 px or 4 ppt
        bindsym l resize grow width 4 px or 4 ppt

        # same bindings, but for the arrow keys
        bindsym Left resize shrink width 4 px or 4 ppt
        bindsym Down resize grow height 4 px or 4 ppt
        bindsym Up resize shrink height 4 px or 4 ppt
        bindsym Right resize grow width 4 px or 4 ppt

        # back to normal: Enter or Escape
        bindsym Return mode "default"
        bindsym Escape mode "default"
      }

    '';

    #extraConfig = builtins.readFile ./../../dotfiles/.config/i3/config;
  };
}
