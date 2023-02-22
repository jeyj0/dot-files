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

        # TokyoNight Color Palette
        set -l foreground c8d3f5
        set -l selection 3654a7
        set -l comment 636da6
        set -l red ff757f
        set -l orange ff966c
        set -l yellow ffc777
        set -l green c3e88d
        set -l purple fca7ea
        set -l cyan 86e1fc
        set -l pink c099ff

        # Syntax Highlighting Colors
        set -g fish_color_normal $foreground
        set -g fish_color_command $cyan
        set -g fish_color_keyword $pink
        set -g fish_color_quote $yellow
        set -g fish_color_redirection $foreground
        set -g fish_color_end $orange
        set -g fish_color_error $red
        set -g fish_color_param $purple
        set -g fish_color_comment $comment
        set -g fish_color_selection --background=$selection
        set -g fish_color_search_match --background=$selection
        set -g fish_color_operator $green
        set -g fish_color_escape $pink
        set -g fish_color_autosuggestion $comment

        # Completion Pager Colors
        set -g fish_pager_color_progress $comment
        set -g fish_pager_color_prefix $cyan
        set -g fish_pager_color_completion $foreground
        set -g fish_pager_color_description $comment
        set -g fish_pager_color_selected_background --background=$selection
      '';
    };
    home.file.fishConfigs = {
      source = ./xdg-config;
      target = ".config/fish";
      recursive = true;
    };
  };
}

