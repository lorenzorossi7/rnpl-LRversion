#include <math.h>
#include <ieeefp.h>
#include "utilmath_c.h"

double drand48_(void) {
   return drand48();
}

int isnand_(double *pnum) {
   return isnand(*pnum);
}

int countnans_(double *v,int *pn) {
   int     i,    rc = 0;

   for( i = 0; i < *pn; i++ ) {
      if( isnand(v[i]) )  rc++;
   }
   return rc;
}

void replacenans_(double *v,int *pn,double *pvalue) {
   int     i;

   for( i = 0; i < *pn; i++ ) {
      if( isnand(v[i]) ) v[i] = *pvalue;
   }
}
