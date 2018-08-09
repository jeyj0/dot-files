#!/bin/bash
GS_OUTPUT=$(git status >&1)

regex_branch="On branch ([a-zA-Z0-9_\-]+)"

regex_origin="'origin/.*"

if [[ $GS_OUTPUT =~ $regex_origin ]]
then
    git push
else
    if [[ $GS_OUTPUT =~ $regex_branch ]]
    then
        branch=${BASH_REMATCH[1]}
        git push --set-upstream origin $branch
    fi
fi
