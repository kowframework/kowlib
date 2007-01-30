#!/bin/sh
#
# This script will create source documentation from the project
# contained in this folder.

#################
# CONFIGURATION #
#################

# source folders
# list only folders with .ads files within
SOURCES="src"


##########
# SCRIPT #
##########

ARQUIVOS=""
INCLUDES=""

for i in ${SOURCES}
do
	for j in $i/*.ads
	do
		ARQUIVOS="${ARQUIVOS} `basename $j`"
	done

	INCLUDES="$INCLUDES -I../$i"
done


rm -rf doc/*.html

cd doc 

echo PROCESSING SOURCES

for i in $ARQUIVOS
do
	echo $i
done | adabrowse $INCLUDES -f- -w0

echo DONE
