function gco -d "Fuzzy-find and checkout a branch"
  git branch --all \
    | grep -v '/HEAD\s' \
    | string trim \
    | fzf --height 50% --preview 'glog-graph (echo {} | sed "s/^\* //")' --preview-window right:70% --ansi --tac --reverse \
    | read -l result\
    ; and git checkout "$result"
end
