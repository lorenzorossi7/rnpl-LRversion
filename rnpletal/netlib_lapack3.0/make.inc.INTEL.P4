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
FORTRAN  = ifort
OPTS     = -O3 -axW -tpp7 -w -w90 -w95 -cm -Vaxlib
DRVOPTS  = $(OPTS)
NOOPT    = -g -axW -tpp7 -w -w90 -w95 -cm -Vaxlib
LOADER   = ifort
LOADOPTS = -O3 -axW -tpp7 -w -w90 -w95 -cm -Vaxlib
#
#  The archiver and the flag(s) to use when building archive (library)
#  If you system has no ranlib, set RANLIB = echo.
#
ARCH     = ar
ARCHFLAGS= cr
RANLIB   = echo
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
