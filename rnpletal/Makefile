.IGNORE:

PACKAGES=\
   rvs \
   cliser \
   rnpl \
   svs \
   vutil \
   utilmath \
   visutil \
   utilio \
   cvtestsdf \
   netlib_linpack \
   netlib_odepack \
   netlib_fftpack \
   netlib_lapack3.0

default:
	echo "Nothing to do by default."  
	echo "Type 'make clean' to clean"
	echo "Type 'make configure' to rebuild configure scripts"

configure: clean
	for d in $(PACKAGES); do \
		(cd $$d; test -f configure.in && autoconf && date && ls -lt configure;) \
	done

export: configure
	for d in $(PACKAGES); do \
		(test -d $(HOME)/autoconf/$$d && cd $(HOME)/autoconf/$$d; E;) \
	done

clean:
	for d in $(PACKAGES); do \
		(cd $$d; make clean;) \
	done
	/bin/rm Install.log
