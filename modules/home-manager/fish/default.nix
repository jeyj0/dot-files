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

      interactiveShellInit = ''
        set FISH_PATH $HOME/.config/fish

        # install fundle if not present (package manager for fish)
        if not functions -q fundle
          eval (curl -sfL https://git.io/fundle-install)
        end

        fundle plugin 'edc/bass'
        fundle plugin 'jeyj0/fzf-fish-integration'

        fundle init

        source $FISH_PATH/paths.fish
        source $FISH_PATH/aliases.fish

        # do not show a welcome message
        set fish_greeting

        # use starship prompt
        starship init fish | source

        if test -d $NVM_DIR
          source $FISH_PATH/nvm.fish
        end

        # load rust
        if test -e $HOME/.cargo/env
          source $HOME/.cargo/env
        end

        # add direnv hook
        # eval (direnv hook fish)
        direnv hook fish | source

        # source nnn config
        source $FISH_PATH/nnn.fish

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

