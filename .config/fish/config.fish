set FISH_PATH $HOME/.config/fish

# install fundle if not present (package manager for fish)
if not functions -q fundle
  eval (curl -sfL https://git.io/fundle-install)
end

fundle plugin 'edc/bass'

fundle init

source $FISH_PATH/paths.fish
source $FISH_PATH/aliases.fish

if test -d $NVM_DIR
  source $FISH_PATH/nvm.fish
end

# do not show a welcome message
set fish_greeting

# load rust
source $HOME/.cargo/env

# use starship prompt
starship init fish | source
