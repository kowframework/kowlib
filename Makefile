# Makefile for the KOW Generic Library Framework
#
# @author Marcelo Coraça de Freitas <marcelo@kow.com.br> 

PROJECT_FILES=kowlib.gpr
GPR_FILES=kowlib.gpr
INCLUDE_FILES=src/*


DOCS_DIRS=manual


ifeq ($(OS), Windows_NT)
	INCLUDE_FILES=src/* src-windows/*
else
	INCLUDE_FILES=src/* src-linux/*
endif

include Makefile.include


pre_libs:
pos_libs:

extra_clean:

