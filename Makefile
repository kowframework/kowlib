# Makefile for the AW_Lib
#
# @author Marcelo Coraça de Freitas <marcelo.batera@gmail.com> 


libs:
	gnatmake -P awlib.gpr
tests:
	gnatmake -P awlib-tests.gpr

all: libs



clean-libs:
	gnatclean -P awlib.gpr
clean-tests:
	gnatclean -P awlib-tests.gpr
clean: clean-tests clean-libs
	@echo "All clean"
