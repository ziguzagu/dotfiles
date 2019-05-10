#!/bin/bash
#
# mute-on.sh - Mute on volumn of MacOS X Lion.
#
# Please add this script to logout hook as following:
#   % sudo defaults write com.apple.loginwindow LogoutHook /Users/ziguzagu/bin/mute-on.sh
#

osascript -e 'set volume with output muted'
