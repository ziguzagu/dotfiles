#!/bin/bash
#
# mute-off.sh - Mute off volumn of MacOS X Lion.
#
# Please add this script to login hook as following:
#   % sudo defaults write com.apple.loginwindow LoginHook /Users/ziguzagu/bin/mute-off.sh
#

osascript -e 'set volume without output muted'
