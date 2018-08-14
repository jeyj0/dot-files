CURRENT_DIR=$(dirname $0 >&1)

# git
alias gs='git status'
alias gss='git status -s'
alias gpush="${CURRENT_DIR}/git-push.sh"
alias gpull='git pull'
alias gmm='git merge master'
alias gcm='git commit -m'
alias gco='git checkout'
alias gco-master='git checkout master'
alias gco-b='git checkout -b'
alias glog='git log'
alias ga='git add'
alias gd='git diff'
alias gd-master='git diff master'

# apt
alias update-and-upgrade='sudo apt-get update && sudo apt-get upgrade'
alias upgrade-kept-back='sudo apt-get --with-new-pkgs upgrade'

# docker
alias docker-mongo='docker run -d -p 27017:27017 -v ~/data:/data/db --name mongo mongo'