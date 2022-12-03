/************************************************************************/
/*  Copyright 1996-2002 Matthew W Choptuik  UT Austin/UBC               */
/*  All rights reserved.                                                */
/************************************************************************/
/*  vsxynt() interface to gft_out_bbox() so that vsxynt() calls         */
/*  result in RNPL-styl .sdf (or .hdf) files.                           */
/*                                                                      */
/*  xvs() is now an alias for vsxynt.                                   */
/************************************************************************/

#include <bbhutil.h>
#include "svs.h"

int xvs(const char *name,double t,double *x,double *y,int n) {
	return vsxynt(name,t,x,y,n);
}

int vsxynt(const char *name,double t,double *x,double *y,int n) {
   int    shape[1];
   shape[0] = n;
   if( gft_out_full(name,t,shape,"r",1,x,y) ) {
      return n;
   } else {
      return -1;
   }
}
