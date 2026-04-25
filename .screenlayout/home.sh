#!/bin/sh

xrandr \
  --output DP-0 --primary --mode 3840x2160 --pos 0x0 --rotate normal \
  --output DP-2 --mode 3840x2160 --pos 3840x0 --rotate normal \
  --output HDMI-0 --mode 1920x1080 --rate 60 --same-as DP-0 --scale-from 3840x2160 --rotate normal \
  --output DP-1 --off \
  --output DP-3 --off \
  --output DP-4 --off \
  --output DP-5 --off

#i3-msg restart

