{ config, pkgs, setupOptions, ... }:
{
	programs.alacritty = {
		enable = true;
		settings = {
			general.live_config_reload = true;
			window.decorations = "none";
			window.startup_mode = "Windowed";
			scrolling.history = 16384;
			font.normal = {
				family = setupOptions.user.font.mono;
				style = "Regular";
			};
			font.size = 16.0;
			cursor.style = {
				shape = "Beam";
				blinking = "On";
			};
			cursor.thickness = 0.25;
		};
	};
}
