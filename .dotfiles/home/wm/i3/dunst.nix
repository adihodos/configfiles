{
	pkgs,
  osConfig,
  lib,
	...
}:
let
  
  fonts = osConfig.themes.fonts;
  colors = osConfig.themes.colors;
in
{
	services.dunst = {
		enable = true;
		iconTheme = {
			package = pkgs.catppuccin-papirus-folders;
			name = "Papirus";
		};
		settings = with colors; {
			global = {
				frame_color = "#${pink}95";
				separator_color = "#${pink}";
				width = 220;
				height = 280;
				offset = "0x15";
				# font = "${fonts.main.family} ${fonts.main.size}";
        font = "${fonts.main.family} 10";
				corner_radius = 10;
				origin = "top-center";
				notification_limit = 3;
				idle_threshold = 120;
				ignore_newline = "no";
				mouse_left_click = "close_current";
				mouse_right_click = "close_all";
				sticky_history = "yes";
				history_length = 20;
				show_age_threshold = 60;
				ellipsize = "middle";
				padding = 10;
				always_run_script = true;
				frame_width = 2;
				transparency = 10;
				progress_bar = true;
				progress_bar_frame_width = 0;
				highlight = lib.strings.concatStrings ["#" pink];
			};
			fullscreen_delay_everything.fullscreen = "delay";
			urgency_low = {
				background = "#${base}83";
				foreground = "#${text}";
				timeout = 5;
			};
			urgency_normal = {
				background = "#${base}83";
				foreground = "#c6d0f5";
				timeout = 6;
			};
			urgency_critical = {
				background = "#${base}83";
				foreground = "#${text}";
				frame_color = "#${red}80";
				timeout = 0;
			};
		};
	};
}
