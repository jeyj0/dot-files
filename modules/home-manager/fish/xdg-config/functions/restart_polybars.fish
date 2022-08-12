#!/usr/bin/env fish

function restart_polybars
    # Terminate already running bar instances
    pkill polybar

    # Launch "bar" on all monitors
    for monitor in (polybar --list-monitors)
    set mon echo (echo $monitor | sed -r 's/^([^:]+).*$/\1/')
    env MONITOR=$mon polybar bar >>/tmp/polybar1.log 2>&1 &
    end
end
