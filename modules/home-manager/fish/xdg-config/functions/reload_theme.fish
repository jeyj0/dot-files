function reload_theme
    xrdb $HOME/.Xresources
    i3-msg reload >/dev/null
    restart_polybars

    reload_kitty_colors
end
