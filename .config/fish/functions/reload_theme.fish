function reload_theme
    xrdb $HOME/.Xresources
    i3-msg reload >/dev/null
    restart_polybars
    kitty @ --to unix:/tmp/kittysocket set-colors $HOME/.config/kitty/kitty.conf
end
