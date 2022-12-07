.IGNORE:

prefix  = /usr/local

bindir = $(prefix)/bin
libdir = $(prefix)/lib

BBH_SYSTEM = DARWIN

CC     = gcc
AR     = ar
RANLIB = ranlib
PERL   = perl
RPCGEN = rpcgen
CTOF   = @CTOF@

LIBS       = -lsvs   -lrnpl  -lxvs -lm
LDFLAGS    = -L/usr/local/lib -L/usr/X11R6/lib -L/sw/lib -L/usr/local/gfortran/lib -L/opt/local/lib -L../src    -L/usr/local/lib 
CPPFLAGS   = -I/usr/local/include -I/opt/local/include -DDARWIN -D__L -DWant_c_files    -I/usr/local/include    -I/usr/local/include
CFLAGS     = -O6
DEFS       = -DPACKAGE_NAME=\"\" -DPACKAGE_TARNAME=\"\" -DPACKAGE_VERSION=\"\" -DPACKAGE_STRING=\"\" -DPACKAGE_BUGREPORT=\"\" -DPACKAGE_URL=\"\" -DSTDC_HEADERS=1 -DHAVE_STRDUP=1  -DF77_HAS_SYSTEM -DF77_HAS_CHDIR -DHAVE_LIBVS=1

F77      = gfortran
F77FLAGS = -O6 -fno-second-underscore
F77_TRANSFORM = touch

CC_COMP    = $(CC) $(CFLAGS) $(CPPFLAGS) -I../src
CC_LOAD    = $(CC) $(CFLAGS) $(LDFLAGS) 

F77_COMP   = $(F77) $(F77FLAGS) 
F77_LOAD   = $(F77) $(F77FLAGS) $(LDFLAGS)  

EXECUTABLES = f_thvs c_thvs

all: $(EXECUTABLES)

install:
	echo "+++ Example programs do not install"

.c.o:
	$(CC_COMP) -c $*.c

.f.o: 
	$(F77_TRANSFORM) $*.f
	$(F77_COMP) -c $*.f

f_thvs: f_thvs.o
	$(F77_LOAD) f_thvs.o $(LIBS) -o f_thvs

c_thvs: c_thvs.o
	$(CC_LOAD) c_thvs.o $(LIBS) -o c_thvs

########################################################################
# Clean-up
########################################################################
clean:
	@/bin/rm $(EXECUTABLES)
	@/bin/rm *.o
	@/bin/rm *.hdf
	@/bin/rm *.sdf
