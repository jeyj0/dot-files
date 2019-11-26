CURRENT_DIR=$(dirname $0 >&1)

# git
alias gs='git status'
alias gss='git status -s'
alias gpush="${CURRENT_DIR}/git-push.sh"
alias gpull='git pull'
alias gm-master='git merge master'
alias gc='git commit'
alias gc-m='git commit -m'
alias gco='git checkout'
alias gco-master='git checkout master'
alias gco-b='git checkout -b'
alias glog='git log'
alias glog-graph='git log --graph --abbrev-commit --decorate --format=format:"%C(blue)%h%C(reset) - %C(white bold)%s%C(reset) %C(dim white)- %an%C(reset)%n""           %C(cyan)%aD%C(reset) %C(green)(%ar)%C(reset)%C(auto)%d%C(reset)"'
alias ga='git add'
alias gd='git difftool'
alias gd-master='git difftool master'

# apt
alias update-and-upgrade='sudo apt-get update && sudo apt-get upgrade'
alias upgrade-kept-back='sudo apt-get --with-new-pkgs upgrade'

# docker
alias docker-mongo='docker run -d -p 27017:27017 -v ~/data:/data/db --name mongo mongo'

# make deletion of files on command line safer
alias rm='trash'

# (neo)vim
alias vim='~/scripts/neovim-remote.sh'
alias v='vim'
