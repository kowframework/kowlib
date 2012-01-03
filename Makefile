# Makefile for the KOW Generic Library Framework
#
# @author Marcelo Coraça de Freitas <marcelo@kow.com.br> 

PROJECT=kowlib
INCLUDE_FILES=src/*

ENABLE_DEBUG=yes

export KOW_LIB_EXTERNALY_BUILT=false


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

