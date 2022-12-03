.IGNORE:

prefix  = /home/matt/autoconf/svs

bindir = $(prefix)/bin
libdir = $(prefix)/lib

BBH_SYSTEM = LINUX_IA32

CC     = icc
AR     = ar
RANLIB = ranlib
PERL   = perl
RPCGEN = rpcgen
CTOF   = @CTOF@

LIBS       = -lsvs   -lrnpl  -lxvs -lm
LDFLAGS    = -L/usr/local/intel/lib -L/usr/X11R6/lib -L../src    -L/home/matt/autoconf/svs/lib  -L/usr/local/intel/lib 
CPPFLAGS   =  -wd177,266 -DINTEL_8_OR_LATER -DLINUX -DPORT_GROUP -DWant_c_files    -I/home/matt/autoconf/svs/include    -I/usr/local/include
CFLAGS     = -O 
DEFS       = -DPACKAGE_NAME=\"\" -DPACKAGE_TARNAME=\"\" -DPACKAGE_VERSION=\"\" -DPACKAGE_STRING=\"\" -DPACKAGE_BUGREPORT=\"\" -DPACKAGE_URL=\"\" -DSTDC_HEADERS=1 -DHAVE_STRDUP=1  -DF77_HAS_SYSTEM -DF77_HAS_CHDIR -DHAVE_LIBVS=1

F77      = ifort
F77FLAGS = -O3  -w -w90 -w95 -cm -Vaxlib
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
