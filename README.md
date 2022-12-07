# rnpl-LRversion
Upgraded versions of RNPL to compile on LR's Mac

Note of changes made

Hans Bantilan (HB) has modified sdfdump.c to enable dumping of rank 2 .sdf files into ascii. I, Lorenzo Rossi (LR) modified it further to enable dumping of rank 3 .sdf files into ascii.


LR

I, Lorenzo Rossi, have edited the following files in RNPL changing #include <malloc.h> into #include <malloc/malloc.h> to eliminate an error during the compilation on Mac Os High Sierra.
The modified lines have a comment to explain what I did also inside the files themselves.

rnpl/examples/get_param/t_get_param_v.c: #include <malloc.h>
rnpl/examples/get_param/t_get_param.c: #include <malloc.h>
rvs/src/Archive/rvs_xdr.c: #include <malloc.h>
rvs/src/Archive/rvs_svc.c: #include <malloc.h>
rvs/src/Archive/rvs_clnt.c: #include <malloc.h>

I have also added the following lines in certain files in order to, again, avoid errors from modern compilers.

rvs/src/rvs.x: %#include <string.h>
rvs/src/v1.c: #include <stdlib.h>
rvs/src/vn.c: #include <stdlib.h>
rnpl/src/librnpl.c: #include "svs.h"
rnpl/src/sdf.c: #include <ctype.h>

rnpl/src/gpar.y: 
%{
/* LR - this seems to be needed by modern compilers */
int yylex();
void yyerror(const char *s);
%}

rnpl/src/rnpl.y:
%{
/* LR - this seems to be needed by modern compilers */
int yylex();
void yyerror(const char *s);
%}

rnpl/src/rnpl.y:
#include <getopt.h> (UNCOMMENTED IN THE FILE as I don't know how to write a comment in a .y file without generating errors during compilation)

rnpl/src/rnpl_sup.c: #include "bbhutil.h"
rnpl/src/rnpl_sup.c: moved definition of coordsys_to_index function before it's called for the first time in the file
rnpl/src/rnpl_sup.c: moved "#include <math.h>" above "#ifdef __POWERPC__"

rnpl/src/genf77extern_sun.c: #include <ctype.h>
rnpl/etc/sdftosv.c: #include <getopt.h>
rnpl/etc/sdftosv.c:	#include <string.h>
rnpl/etc/sdftosv.c:	moved definition of dump_mser function before it's called for the first time in the file
rnpl/etc/sdffilter.c: #include <bbhutil.h>
rnpl/etc/sdftranspose.c: #include <bbhutil.h>
rnpl/etc/sdfslice_util.c: #include "d3lib.h"



In rnpl/aclocal.m4: I changed 
                 LDFLAGS="$LDFLAGS -L/usr/X11R6/lib -L/sw/lib -L/usr/local/gfortran/lib -L/opt/local/lib"
into 
                #LR changed this so that the library for -lgfortran is found on Monterey with gfortran 12.2.0
                #LDFLAGS="$LDFLAGS -L/usr/X11R6/lib -L/sw/lib -L/usr/local/gfortran/lib -L/opt/local/lib"
                 LDFLAGS="$LDFLAGS -L/usr/X11R6/lib -L/sw/lib -L/usr/local/opt/gcc/lib/gcc/current -L/opt/local/lib"

In rnpl/.configure: I changed 
                 LDFLAGS="$LDFLAGS -L/usr/X11R6/lib -L/sw/lib -L/usr/local/gfortran/lib -L/opt/local/lib"
into 
                #LR changed this so that the library for -lgfortran is found on Monterey with gfortran 12.2.0
                #LDFLAGS="$LDFLAGS -L/usr/X11R6/lib -L/sw/lib -L/usr/local/gfortran/lib -L/opt/local/lib"
                 LDFLAGS="$LDFLAGS -L/usr/X11R6/lib -L/sw/lib -L/usr/local/opt/gcc/lib/gcc/current -L/opt/local/lib"


In cvtestsdf/aclocal.m4: I changed 
                 LDFLAGS="$LDFLAGS -L/usr/X11R6/lib -L/sw/lib -L/usr/local/gfortran/lib -L/opt/local/lib"
into 
                #LR changed this so that the library for -lgfortran is found on Monterey with gfortran 12.2.0
                #LDFLAGS="$LDFLAGS -L/usr/X11R6/lib -L/sw/lib -L/usr/local/gfortran/lib -L/opt/local/lib"
                 LDFLAGS="$LDFLAGS -L/usr/X11R6/lib -L/sw/lib -L/usr/local/opt/gcc/lib/gcc/current -L/opt/local/lib"

In cvtestsdf/.configure: I changed 
                 LDFLAGS="$LDFLAGS -L/usr/X11R6/lib -L/sw/lib -L/usr/local/gfortran/lib -L/opt/local/lib"
into 
                #LR changed this so that the library for -lgfortran is found on Monterey with gfortran 12.2.0
                #LDFLAGS="$LDFLAGS -L/usr/X11R6/lib -L/sw/lib -L/usr/local/gfortran/lib -L/opt/local/lib"
                 LDFLAGS="$LDFLAGS -L/usr/X11R6/lib -L/sw/lib -L/usr/local/opt/gcc/lib/gcc/current -L/opt/local/lib"

In rnpl/examples/get_param/t_get_param.c: #include <string.h>
In rnpl/examples/get_param/t_get_str_param.c: #include <stdlib.h>
In rnpl/examples/get_param/t_regex.c: #include <stdlib.h>
In rnpl/examples/get_param/t_is_param_assigned.c: #include <stdlib.h>



In svs/examples/c_thvs.c: #include "svs.h"
In svs/examples/c_thvs.c: #include "bbhutil.h"

In vutil/aclocal.m4: I changed 
                 LDFLAGS="$LDFLAGS -L/usr/X11R6/lib -L/sw/lib -L/usr/local/gfortran/lib -L/opt/local/lib"
into 
                #LR changed this so that the library for -lgfortran is found on Monterey with gfortran 12.2.0
                #LDFLAGS="$LDFLAGS -L/usr/X11R6/lib -L/sw/lib -L/usr/local/gfortran/lib -L/opt/local/lib"
                 LDFLAGS="$LDFLAGS -L/usr/X11R6/lib -L/sw/lib -L/usr/local/opt/gcc/lib/gcc/current -L/opt/local/lib"

In vutil/.configure: I changed 
                 LDFLAGS="$LDFLAGS -L/usr/X11R6/lib -L/sw/lib -L/usr/local/gfortran/lib -L/opt/local/lib"
into 
                #LR changed this so that the library for -lgfortran is found on Monterey with gfortran 12.2.0
                #LDFLAGS="$LDFLAGS -L/usr/X11R6/lib -L/sw/lib -L/usr/local/gfortran/lib -L/opt/local/lib"
                 LDFLAGS="$LDFLAGS -L/usr/X11R6/lib -L/sw/lib -L/usr/local/opt/gcc/lib/gcc/current -L/opt/local/lib"

In visutil/aclocal.m4: I changed 
                 LDFLAGS="$LDFLAGS -L/usr/X11R6/lib -L/sw/lib -L/usr/local/gfortran/lib -L/opt/local/lib"
into 
                #LR changed this so that the library for -lgfortran is found on Monterey with gfortran 12.2.0
                #LDFLAGS="$LDFLAGS -L/usr/X11R6/lib -L/sw/lib -L/usr/local/gfortran/lib -L/opt/local/lib"
                 LDFLAGS="$LDFLAGS -L/usr/X11R6/lib -L/sw/lib -L/usr/local/opt/gcc/lib/gcc/current -L/opt/local/lib"

In visutil/.configure: I changed 
                 LDFLAGS="$LDFLAGS -L/usr/X11R6/lib -L/sw/lib -L/usr/local/gfortran/lib -L/opt/local/lib"
into 
                #LR changed this so that the library for -lgfortran is found on Monterey with gfortran 12.2.0
                #LDFLAGS="$LDFLAGS -L/usr/X11R6/lib -L/sw/lib -L/usr/local/gfortran/lib -L/opt/local/lib"
                 LDFLAGS="$LDFLAGS -L/usr/X11R6/lib -L/sw/lib -L/usr/local/opt/gcc/lib/gcc/current -L/opt/local/lib"

In utilio/string_c.c: #include <stdlib.h>

In utilio/filevs_c.h: int       vsxynt_(char *name,double t,DVEC x,DVEC y,int n);
In utilio/filevs_c.c: 
#include "v_types.h" 
#include "filevs_c.h"
In utilio/filevs_c.c: I changed the line
    return( vsxynt_(name,&t,x,y,&n,strlen(name)) ) ;
into
   //LR changed this as the original line gives an error in moder compilers
    //return( vsxynt_(name,&t,x,y,&n,strlen(name)) ) ;
   return( vsxynt_(name,t,x,y,&n) ) ;
   
In utilmath/utilmath.c: #include <math.h> 

