handle_mpich_126() {

# set RSHCOMMAND environment variable to default 'rsh' if 
# it is not defined, presumably are supposed to be doing the same thing 
# with some of the other command-line parameters (lord knows why the env
# vbl is approach is favored over cmd line)
	RSHCOMMAND=${RSH-rsh}
	export RSHCOMMAND

	./configure -cc="$CC" -cflags="$CFLAGS" -fc="$F77" -fflags="$F77FLAGS" -f90="$F90" -f90flags="$F90FLAGS" \
	    -nodevdebug --prefix=$PREFIX 
	make 
	make install

}

handle_pamr() {

# Look for MPI installation
LIBMPICH="$PREFIX/lib/libmpich.a"
test -f $LIBMPICH || die "Could not find MPICH library '$LIBMPICH'"

test -f $PREFIX/bin/mpicc || die "Could not find mpicc"
CC="$PREFIX/bin/mpicc"
export CC

test -f $PREFIX/bin/mpif77 || die "Could not find mpif77"
F77="$PREFIX/bin/mpif77"
export F77

test -f $PREFIX/bin/mpif90 || die "Could not find mpif90"
F90="$PREFIX/bin/mpif90"
export F90

}
