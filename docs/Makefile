
Reference_manual.pdf: 
	@command -v doxygen 1> /dev/null || ( echo "Doxygen is required but is not installed (or not found)" && exit 1 )
	@doxygen pdi.doxygen
	@(cd doxygen/latex && make && cp refman.pdf ../../Reference_manual.pdf)

clean: 
	@rm -r ../docs/doxygen
	@rm -r ../docs/Reference_manual.pdf
