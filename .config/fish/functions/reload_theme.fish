#!/usr/bin/env fish

function reload_theme
    xrdb $HOME/.Xresources
    i3-msg reload
    restart_polybars
end
