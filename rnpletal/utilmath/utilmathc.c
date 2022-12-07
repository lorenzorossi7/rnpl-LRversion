#include "utilmathc.h"
#include <math.h> //LR: needed by modern compilers

double    drange_um(double dmin,double dmax,double d) {
   return( d < dmin ? dmin : (d > dmax ? dmax : d ) ) ;
}

int       irange_um(int imin,int imax,int i) {
   return( i < imin ? imin : (i > imax ? imax : i ) ) ;
}

double    dlintr(double frmin,double frmax,double fr,
                 double tomin,double tomax) {
   return( drange_um(tomin,tomax,
                     tomin + (fr - frmin) * (tomax - tomin) / (frmax - frmin)) 
         );
}

int       dr2ir(double dmin,double dmax,double d,int imin,int imax) {
   return( irange_um(imin,imax,
                     (int) (0.5 + 
                            dlintr(dmin,dmax,d,(double) imin,(double) imax))) );
}

double    ir2dr(int imin,int imax,int i,double dmin,double dmax) {
   return( drange_um(dmin,dmax,
                     dlintr((double) imin,(double) imax,(double) i,dmin,dmax)) 
         );
                         
}

double    udvint(DVEC v,int n,double x) {
   int     i;
   double  xi,  dx, dxm1, val;

   if(         n <= 0 ) {
      val = 0.0;
   } else if ( n == 1 ) {
      val = v[0];
   } else {
      dxm1 = n - 1;
      dx   = 1.0 / dxm1;
      x    = drange_um(0.0,1.0,x);
      i    = dxm1 * x; if( i == (n - 1) ) i--;
      xi   = i * dx;
      val = dxm1 * (x - xi) * v[i+1] + (1.0 - dxm1 * (x - xi)) * v[i];
   }
   return( val );
}

double    uivint(IVEC v,int n,double x) {
   int     i;
   double  xi,  dx, dxm1, val;

   if(         n <= 0 ) {
      val = 0;
   } else if ( n == 1 ) {
      val = v[0];
   } else {
      dxm1 = n - 1;
      dx   = 1.0 / dxm1;
      x    = drange_um(0.0,1.0,x);
      i    = dxm1 * x; if( i == (n - 1) ) i--;
      xi   = i * dx;
      val = dxm1 * (x - xi) * v[i+1] + (1.0 - dxm1 * (x - xi)) * v[i];
   }
   return( val );
}

int countnans(DVEC v,int n) {
   int     i,    rc = 0;

   for( i = 0; i < n; i++ ) {
      if( isnan(v[i]) )  rc++;
   }
   return rc;
}

void replacenans(DVEC v,int n,double value) {
   int     i;

   for( i = 0; i < n; i++ ) {
      if( isnan(v[i]) ) v[i] = value;
   }
}

double dvmin_nonan(DVEC v,int n) {
   double   rc = 0.0;
   int      i;

   for( i = 0; i < n; i++ ) {
      if( !isnan(v[i]) ) {
         rc = v[i]; 
goto Continue;
      }
   }
Continue:
   for( i = i+1; i < n; i++ ) {
      if( !isnan(v[i]) && v[i] < rc ) rc = v[i];
   }
   return rc;
}

double dvmax_nonan(DVEC v,int n) {
   double   rc = 0.0;
   int      i;

   for( i = 0; i < n; i++ ) {
      if( !isnan(v[i]) ) {
         rc = v[i]; 
goto Continue;
      }
   }
Continue:
   for( i = i+1; i < n; i++ ) {
      if( !isnan(v[i]) && v[i] > rc ) rc = v[i];
   }
   return rc;
}
