#!/usr/bin/env fish

function reload_kitty_colors
    set sockets (ls /tmp/kittysocket_*)
    for socket in $sockets
        kitty @ --to unix:$socket set-colors {$HOME}/.config/kitty/kitty.conf
    end
end
