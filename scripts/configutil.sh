#!/usr/bin/env bash



check_in_path(){
	echo -n "Looking for $1	... "
	hash $1 2>&- || { echo "[false]"; echo >&2 "I need $1 in path but I can't find it... aborting"; exit 1; } && echo "[ok]"
}


