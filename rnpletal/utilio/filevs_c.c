#include <string.h>

/*  C front end to f77 file-based server calls with BSD interlanguage */
/*  convention ... */

#ifndef _CRAY
int vsxynt(char *name,double t,double  *x,double  *y,int n) {
	return( vsxynt_(name,&t,x,y,&n,strlen(name)) ) ;
}
#else

#include <fortran.h>

fortran int VSXYNT ( _fcd name, double *t, double *x, double *y, int *n);

int vsxynt(char *name,double t,double *x,double *y,int n) {
   _fcd    cftname;
   cftname = _cptofcd(name,strlen(name));
   return( VSXYNT (cftname,&t,x,y,&n) );
}

#endif
