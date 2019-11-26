#!/usr/bin/env zsh

local numArgs=$#

function () {
  # function taken from: https://revelry.co/terminal-workflow-fzf/ on Nov 26, 2019
  git_checkout_branch_fzf() {
    result=$(git branch -a --color=always | grep -v '/HEAD\s' | sort |
      fzf --height 50% --border --ansi --tac --preview-window right:70% \
        --preview 'git log --oneline --graph --date=short --pretty="format:%C(auto)%cd %h%d %s" $(sed s/^..// <<< {} | cut -d" " -f1) | head -'$LINES |
      sed 's/^..//' | cut -d' ' -f1)
  
    if [[ $result != "" ]]; then
      if [[ $result == remotes/* ]]; then
        git checkout --track $(echo $result | sed 's#remotes/##')
      else
        git checkout "$result"
      fi
    fi
  }

  if [[ $numArgs -ge 1 ]] || ! which fzf &>/dev/null; then
    git checkout "$@"
  else
    git_checkout_branch_fzf
  fi
}
