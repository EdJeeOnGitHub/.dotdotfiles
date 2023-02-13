#!/bin/sh
xrandr --output eDP1 \
		--primary \
		--mode 2880x1800 \
		--pos 0x0 \
		--rotate normal \
	--output DP1 \
		--mode 2880x864 \
		--pos 0x1800 \
		--rotate normal \
	--output DP2 \
		--mode 1920x1080 \
		--pos 2880x0 \
		--rotate normal \
		--scale 2x2 \
	--output DP3 \
		--off \
		--output VIRTUAL1 \
		--off
