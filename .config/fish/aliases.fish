# git
alias gs='git status'
alias gss='git status -s'
alias gpush='zsh $HOME/scripts/git-push.sh'
alias gpull='git pull'
alias gc='git commit'
alias glog='git log'
alias glog-graph='git log --graph --abbrev-commit --color=always --decorate --format=format:"%C(blue)%h%C(reset) - %C(white bold)%s%C(reset) %C(dim white)- %an%C(reset)%n""           %C(cyan)%aD%C(reset) %C(green)(%ar)%C(reset)%C(auto)%d%C(reset)"'
alias ga='git add'
alias gd='git diff'
alias gdc='git diff --cached'

# make deletion of files on command line safer
alias rm='trash'

# (neo)vim
alias vim='if test -n "(nvr --serverlist)"; ensure_node; end; zsh $HOME/scripts/neovim-remote.sh'
alias v='vim'
