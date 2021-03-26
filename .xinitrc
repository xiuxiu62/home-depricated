#!/bin/bash

nitrogen --restore &
compton --config ~/.config/compton.conf -b &
dunst &
urxvt &
xrandr --output DP-3 --mode 1920x1080 --rotate left --pos 0x0 \
       --output DP-1 --mode 1920x1080 --primary --pos 1080x575 \
       --output DP-2 --mode 1920x1080 --rotate left --pos 3000x0 &

exec xmonad
