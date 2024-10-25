#!/bin/sh
xrandr --verbose --output eDP1 \
		--primary \
		--mode 2880x1800 \
		--pos 0x1800 \
		--rotate normal \
	--output DP1 \
		--mode 2880x864 \
		--pos 0x3600 \
		--rotate normal \
	--output DP2-1 \
		--mode 1920x1080 \
		--pos 0x0 \
		--rotate normal \
		--transform 1.5,0,0,0,1.5,0,0,0,1 \
	--output DP3 \
		--off \
		--output VIRTUAL1 \
		--off

xrandr --verbose \
	--output DP2-2 \
	--pos 2880x0 \
	--rotate normal \
	--transform 1.5,0,0,0,1.5,0,0,0,1 



#!/bin/sh
#xrandr --verbose --output eDP1 \
#		--primary \
#		--mode 2880x1800 \
#		--pos 0x0 \
#		--rotate normal \
#	--output DP1 \
#		--mode 2880x864 \
#		--pos 0x1800 \
#		--rotate normal \
#	--output DP2-1 \
#		--mode 1920x1080 \
#		--pos 2880x0 \
#		--rotate normal \
#		--transform 1.5,0,0,0,1.5,0,0,0,1 \
#	--output DP3 \
#		--off \
#		--output VIRTUAL1 \
#		--off
#
#xrandr --verbose \
#	--output DP2-2 \
#	--pos 5760x0 \
#	--rotate normal \
#	--transform 1.5,0,0,0,1.5,0,0,0,1 
