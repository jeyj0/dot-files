set FISH_PATH $HOME/.config/fish

# install fundle if not present (package manager for fish)
if not functions -q fundle
  eval (curl -sfL https://git.io/fundle-install)
end

fundle plugin 'edc/bass'

fundle init

# TODO: load conditionally
source $FISH_PATH/nvm.fish
# nvm use default # TODO load automatically when needed by functions

source $FISH_PATH/paths.fish
source $FISH_PATH/aliases.fish

# do not show a welcome message
set fish_greeting

# load rust
source $HOME/.cargo/env

# use starship prompt
starship init fish | source

function fco -d "Fuzzy-find and checkout a branch"
  git branch --all | grep -v HEAD | string trim | fzf | read -l result; and git checkout "$result"
end
