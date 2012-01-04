#!/usr/bin/env bash
# 
# Auxiliary functions for the KOW Framework main build system
#
# @author Marcelo C. de Freitas



# Check if a given command is in path;
# usage:
#	check_in_path COMMAND_NAME
check_in_path(){
	echo -n "Looking for $1	... "
	hash $1 2>&- || { echo "[false]"; echo >&2 "I need $1 in path but I can't find it... aborting"; exit 1; } && echo "[ok]"
}



# Copy updating a regular file or directory
# usage:
#	cpu origin destination
cpu(){
	origin="$1"
	fname=$(basename "$1")
	destination="$2/$fname"
	if [[ -f "$origin" ]]
	then
		if [[ "$origin" -nt "$destination" ]]
		then
			cp "$origin" "$destination"
		else
			echo "Skipping \"$origin\""
		fi
	else
		echo "Can't copy (not a regular file): \"$origin\""
		exit 1;
	fi
}



############################
# Configuration Management #
############################


# reset the configuration file
init_configuration(){
	echo -n "" > .configuration
}



# Set a configuration key/value:
# usage:
#	setconfiguration key value
set_configuration() {
	echo $1=\"$2\" >> .configuration
}


# Printout the configuration file
# usage:
#	cat_configuration
cat_configuration(){
	cat .configuration
}


# load the configuration file:
# usage:
#	load_configuration
load_configuration(){
	source .configuration
}
