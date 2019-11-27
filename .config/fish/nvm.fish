function nvm
  set NVM_DIR $HOME/.config/nvm
  bass source $NVM_DIR/nvm.sh --no-use ';' nvm $argv
end

