{ config, pkgs, setupOptions, ... }:
{
  programs.git = {
    enable = true;
    userName = setupOptions.user.name;
    userEmail = setupOptions.user.email;
    extraConfig = {
      init.defaultBranch = "main";
      safe.directory = "/home/" + setupOptions.user.username + "/.dotfiles";
      core.editor = setupOptions.user.spawnEditor;
      core.autocrlf = "input";
      alias.lg = "log --color --graph --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr)%C(bold blue)<%an>%Creset' --abbrev-commit";
      diftool.prompt = "false";
    };
  };

  home.packages = [
  	pkgs.lazygit
  ];
}
