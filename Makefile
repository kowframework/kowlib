# Makefile for the KOW Generic Library Framework
#
# @author Marcelo Coraça de Freitas <marcelo@kow.com.br> 
#
#
# Please, read Makefile.include for more information


all:
	./scripts/build.sh

install:
	./install.sh

uninstall:
	./uninstall.sh
clean:
	./scripts/clean.sh
