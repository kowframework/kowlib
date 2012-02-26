# Makefile for the AW_Lib
#
# @author Marcelo Coraça de Freitas <marcelo.batera@gmail.com> 

projectFile="kowlib-samples.gpr"



all: samples
	@echo "======================================================="
	@echo "==                                                   =="
	@echo "==                    All done!                      =="
	@echo "==                                                   =="
	@echo "==    Now run the examples in the ./bin directory!   =="
	@echo "==                                                   =="
	@echo "======================================================="


samples:
	 gprbuild -P ${projectFile}



clean:
	gprclean -P ${projectFile}
	@echo "All clean"

