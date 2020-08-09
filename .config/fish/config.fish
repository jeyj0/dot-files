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
eval (direnv hook fish)
