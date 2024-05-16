{ config, pkgs, setupOptions, ... }:
{
  programs.git = {
    enable = true;
    userName = setupOptions.user.name;
    userEmail = setupOptions.user.email;
    ignores = [ ".envrc" ".direnv" ];
    extraConfig = {
      init.defaultBranch = "main";
      safe.directory = "/home/" + setupOptions.user.username + "/.dotfiles";
      core.editor = setupOptions.user.spawnEditor;
      core.autocrlf = "input";
      alias.lg = "log --color --graph --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr)%C(bold blue)<%an>%Creset' --abbrev-commit";
      diftool.prompt = "false";

      # core.pager = "diff-so-fancy | less --tabs=4 -RF";
      # interactive.diffFilter =  "diff-so-fancy --patch";

      color.ui = true;

      color.diff-highlight.oldNormal = "red bold";
      color.diff-highlight.oldHighlight = "red bold 52";
      color.diff-highlight.newNormal = "green bold";
      color.diff-highlight.newHighlight = "green bold 22";

      color.diff.meta = "11";
      color.diff.frag = "magenta bold";
      color.diff.func = "146 bold";
      color.diff.commit = "yellow bold";
      color.diff.old  = "red bold";
      color.diff.new = "green bold";
      color.diff.whitespace = "red reverse";

      diff.tool = "kdiff3";
      difftool = {
        prompt = false;
        keepBackup = false;
        trustExitCode = false;
      };
      
      merge.tool = "kdiff3";
      mergetool = {
        prompt = false;
        keepBackup = false;
        keepTemporaries = false;
      };
    };
  };

  programs.git.diff-so-fancy = {
    enable = true;
  };

  home.packages = [
  	pkgs.lazygit
  ];
}
