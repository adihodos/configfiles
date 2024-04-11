{ config, pkgs, ... }:
{

  programs.kitty = {
	enable = true;
	font.name = "Liberation Mono";
	font.size = 16.0;
	shellIntegration.enableBashIntegration = true;
	theme = "Doom One";
	settings = {
		scrollback_lines = 10000;
		cursor_shape = "block";
		cursor_blink_interval = 5;
	};
  };
}
