function start_terminal
    set socket 0

    while test -e /tmp/kittysocket_$socket
        set socket (math "$socket + 1")
    end

    kitty --listen-on unix:/tmp/kittysocket_$socket

    set -e socket
end
