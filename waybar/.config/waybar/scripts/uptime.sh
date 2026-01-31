#!/bin/bash
exec 3< <(sleep infinity)
while :; do
    read -r u _ < /proc/uptime
    u=${u%.*}
    printf '%02d:%02d:%02d\n' $((u/3600)) $((u%3600/60)) $((u%60))
    read -t 1 -u 3 ||:
done
