####################################################################
#  LAPACK make include file.                                       #
#  LAPACK, Version 3.0                                             #
#  June 30, 1999                                                  #
####################################################################
#
SHELL = /bin/sh
#
#  The machine (platform) identifier to append to the library names
#
PLAT =
#  
#  Modify the FORTRAN and OPTS definitions to refer to the
#  compiler and desired compiler options for your machine.  NOOPT
#  refers to the compiler options desired when NO OPTIMIZATION is
#  selected.  Define LOADER and LOADOPTS to refer to the loader and 
#  desired load options for your machine.
#
FORTRAN  = gfortran
OPTS     = -O6 -fno-second-underscore
DRVOPTS  = $(OPTS)
NOOPT    = -fno-second-underscore
LOADER   = gfortran
LOADOPTS = -O6 -fno-second-underscore -L/usr/local/lib -L/usr/X11R6/lib -L/sw/lib -L/usr/local/gfortran/lib -L/opt/local/lib
#
#  The archiver and the flag(s) to use when building archive (library)
#  If you system has no ranlib, set RANLIB = echo.
#
ARCH     = ar
ARCHFLAGS= cr
RANLIB   = ranlib
#
#  The location of the libraries to which you will link.  (The 
#  machine-specific, optimized BLAS library should be used whenever
#  possible.)
#
BLASLIB      = ../../libblas.a
LAPACKLIB    = liblapack.a
TMGLIB       = libtmglib.a
EIGSRCLIB    = libeigsrc.a
LINSRCLIB    = liblinsrc.a
