#!/bin/bash 
############################################################
# Install script for use with GNU compilers on OS X (ugh!)
# 
# Courtesy of David Wayne Neilsen, BYU
############################################################
PACKAGES="\
   rvs \
   cliser \
   sv \
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
   netlib_lapack3.0 \
"

P=`basename $0`

usage() {
cat<<END
usage: $P installation-prefix

Example: $P /usr/local

Note: See script source for user-modifiable environment variables that may 
need to be tailored to your desired compilation environment.
END
exit 1
}

warn() {
   echo "$P: $1"
}

header() {
cat<<END | tee -a Install.log
+++++++++++++++++++++++++++++++++++++++
LOG: rnpletal/Install 

date=`date`

hostname=`hostname`
uname -a=`uname -a`
USER=$USER

CC=<$CC>
CXX=<$CXX>
CFLAGS=<$CFLAGS>
CPPFLAGS=<$CPPFLAGS>
LDFLAGS=<$LDFLAGS>
F77=<$F77>
F77FLAGS=<$F77FLAGS>
F77LFLAGS=<$F77LFLAGS>
F90=<$F90>
F90FLAGS=<$F90FLAGS>
LIB_PATHS=<$LIB_PATHS>
INCLUDE_PATHS=<$INCLUDE_PATHS>
+++++++++++++++++++++++++++++++++++++++

END
}

case $# in
1) PREFIX=$1;;
*) usage;;
esac

# Name of C compiler
export CC="gcc"

# Name of C++ compiler
export CXX="g++"

# Generic C flags (optimization etc.)
export CFLAGS="-faltivec -O3"

# C pre-processor flags (specify non-standard include locations etc.)
export CPPFLAGS="-I${PREFIX}/include"

# Generic C++ flags (optimization etc.)
export CXXFLAGS="-O3"

# Loader flags (specify non-standard library locations etc.)
export LDFLAGS="-L${PREFIX}/lib -L/sw/lib"

# Name of Fortran 77 compiler
export F77="g77"

# Generic Fortran 77 flags (optimization etc.)
export F77FLAGS="-O3 -fno-second-underscore -faltivec"

# Fortran 77 load phase flags (specify non-standard library locations etc.)
export F77LFLAGS="-L${PREFIX}/lib -L/sw/lib"

# Name of Fortran 90 compiler
export F90="g77"

# Generic Fortran 90 flags (optimization etc.)
export F90FLAGS="-O3 -fno-second-underscore -faltivec"

# Additional paths to search for libraries (white space separated)
export LIB_PATHS="${PREFIX}/lib"

# Additional paths to search for include files (white space separated)
export INCLUDE_PATHS="${PREFIX}/include"

export DAGH_NO_MPI='on'
export BBH_CHECK_DEFAULTS='on'

unset  CCF77LIBS

test -f Install.log && rm Install.log
header
for pack in $PACKAGES; do
   if test -d $pack; then
      echo "+++++++++++++++++++++++++++++++++++++++" | tee -a Install.log
      echo "Configuring and installing in $pack ..." | tee -a Install.log
      echo "+++++++++++++++++++++++++++++++++++++++" | tee -a Install.log
      (cd $pack; ./configure --prefix=$PREFIX; make install) 2>&1 | tee -a Install.log
   else
      warn "Directory $pack not found in this distribution." 
   fi
done