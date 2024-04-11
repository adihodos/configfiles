{ config, pkgs, ... }:
{
	programs.alacritty = {
		enable = true;
		settings = {
			general.live_config_reload = true;
			window.decorations = "Full";
			window.startup_mode = "Maximized";
			scrolling.history = 16384;
			font.normal = {
				family = "Iosevka Nerd Font Propo";
				style = "Regular";
			};
			font.size = 20.0;
			cursor.style = {
				shape = "Beam";
				blinking = "On";
			};
			cursor.thickness = 0.25;
		};
	};
}
