{ config
, pkgs
, lib
, ...
}:
with lib;
{
  options.jeyj0.fish = {
    enable = mkEnableOption "fish";
  };

  config = mkIf config.jeyj0.fish.enable {
    home.packages = with pkgs.unstable; [
      fish
    ];
    jeyj0.direnv.enable = true;
    programs.fish = {
      enable = true;
      package = pkgs.unstable.fish;

      shellAliases = {
        # general
        cp = "cp -i";
        mv = "mv -i";
        ls = "exa";
        la = "ls -l --all";
        tree = "exa --tree";
        op = "cd ~/projects/(ls ~/projects/ | fzf)";

        # nnn
        nnn = "nnn -H";
        fm = "nnn";

        # git
        gs = "git status";
        gss = "git status -s";
        gpush = "git push";
        gpull = "git pull";
        gc = "git commit";
        gco = "git_checkout_fzf";
        glog = "git log";
        # glog-graph = "git log --graph --abbrev-commit --color=always --decorate --format=format:\"\%C(blue)\%h\%C(reset) - \%C(white bold)\%s\%C(reset) \%C(dim white)- \%an\%C(reset)\%n""           \%C(cyan)\%aD\%C(reset) \%C(green)(\%ar)\%C(reset)\%C(auto)\%d\%C(reset)\"";
        ga = "git add";
        gd = "git diff";
        gdc = "git diff --cached";

        # make deletion of files on command line safer
        rm = "trash";

        # alias e to the default editor
        e = "$VISUAL";

        # (neo)vim
        vim = "nvim";
        vi = "vim";
        v = "vim";
        f = "~/.config/nvim/jeyj0-plugged/vim-floaterm/bin/floaterm";

        # utilities
        whatismyip = "curl ipinfo.io/ip";
      };

      interactiveShellInit = ''
        set FISH_PATH $HOME/.config/fish

        # install fundle if not present (package manager for fish)
        if not functions -q fundle
          eval (curl -sfL https://git.io/fundle-install)
        end

        fundle plugin 'edc/bass'
        fundle plugin 'jeyj0/fzf-fish-integration'

        fundle init

        set PATH $HOME/.local/bin $HOME/.emacs.d/bin $HOME/.cargo/bin $PATH

        # do not show a welcome message
        set fish_greeting

        # use starship prompt
        starship init fish | source

        # load rust
        if test -e $HOME/.cargo/env
          source $HOME/.cargo/env
        end

        # add direnv hook
        # eval (direnv hook fish)
        direnv hook fish | source

        # a function to load key=value environment variable files
        function posix-source
          for i in (cat $argv)
            set arr (echo $i |tr = \n)
            set -gx $arr[1] $arr[2]
          end
        end
      '';
    };
    home.file.fishConfigs = {
      source = ./xdg-config;
      target = ".config/fish";
      recursive = true;
    };
  };
}

