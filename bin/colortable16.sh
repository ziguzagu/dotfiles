#!/usr/bin/env bash
set -eu

echo "Background | Foreground colors"
echo "---------------------------------------------------------------------"
for((bg=40;bg<=47;bg++)); do
	for((bold=0;bold<=1;bold++)) do
		echo -en "\033[0m"" ESC[${bg}m   | "
		for((fg=30;fg<=37;fg++)); do
			if [ $bold == "0" ]; then
				echo -en "\033[${bg}m\033[${fg}m [${fg}m  "
			else
				echo -en "\033[${bg}m\033[1;${fg}m [1;${fg}m"
			fi
		done
		echo -e "\033[0m"
	done
	echo "---------------------------------------------------------------------"
done
