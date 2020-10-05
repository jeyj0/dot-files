# general
alias ls='exa'
alias la='ls -l --all'
alias tree='exa --tree'
alias op='cd ~/projects/(ls ~/projects/ | fzf)'

# git
alias gs='git status'
alias gss='git status -s'
alias gpush='zsh $HOME/scripts/git-push.sh'
alias gpull='git pull'
alias gc='git commit'
alias gco='git_checkout_fzf'
alias glog='git log'
alias glog-graph='git log --graph --abbrev-commit --color=always --decorate --format=format:"%C(blue)%h%C(reset) - %C(white bold)%s%C(reset) %C(dim white)- %an%C(reset)%n""           %C(cyan)%aD%C(reset) %C(green)(%ar)%C(reset)%C(auto)%d%C(reset)"'
alias ga='git add'
alias gd='git diff'
alias gdc='git diff --cached'

# make deletion of files on command line safer
alias rm='trash'

# alias e to the default editor
alias e='$VISUAL'

# (neo)vim
alias vim='nvim'
alias vi='vim'
alias v='vim'
alias f='~/.config/nvim/jeyj0-plugged/vim-floaterm/bin/floaterm'

# vscodium = vscode
alias code='codium'

# utilities
alias whatismyip='curl ipinfo.io/ip'
