# Makefile for the KOW Generic Library Framework
#
# @author Marcelo Coraça de Freitas <marcelo@kow.com.br> 
#
#
# Please, read Makefile.include for more information

PROJECT=kowlib
INCLUDE_FILES=src/*

ENABLE_DEBUG=false

GPRBUILD_PARAMS=-XKOWLIB_EXTERNALLY_BUILT=false


ifeq ($(OS), Windows_NT)
	INCLUDE_FILES=src/* src-windows/*
else
	INCLUDE_FILES=src/* src-posix/*
endif

include Makefile.include


pre_setup:
extra_gnatprep_setup:
pos_setup:
pre_build:
pos_build:

extra_clean:

