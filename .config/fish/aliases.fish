# git
alias gs='git status'
alias gss='git status -s'
alias gpush='git_push'
alias gpull='git pull'
alias gc='git commit'
# alias gco='git_checkout'
alias gco='zsh $HOME/scripts/git-checkout.sh'
alias gco-b='gco -b'
alias glog='git log'
alias glog-graph='git log --graph --abbrev-commit --decorate --format=format:"%C(blue)%h%C(reset) - %C(white bold)%s%C(reset) %C(dim white)- %an%C(reset)%n""           %C(cyan)%aD%C(reset) %C(green)(%ar)%C(reset)%C(auto)%d%C(reset)"'
alias ga='git add'
alias gd='git diff'

# make deletion of files on command line safer
alias rm='trash'

# (neo)vim
alias vim='if test -n "(nvr --serverlist)"; ensure_node; end; zsh $HOME/scripts/neovim-remote.sh'
alias v='vim'
