#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <limits.h>
#include <time.h>
#include "ds.h"

double dmin(r_double x0,r_double x1) {
   return (x0 < x1) ? x0 : x1;
}

double dmax(r_double x0,r_double x1) {
   return (x0 > x1) ? x0 : x1;
}

DSEG make_DSEG(double x0,double x1) {
   DSEG      ds; 

   ds.x0 = x0;
   ds.x1 = x1;
   return ds;
}

int null_DSEG(DSEG ds) {
   return ds.x1 < ds.x0;
}

DSEG and_DSEG(DSEG ds,DSEG dsp) {
   return make_DSEG(dmax(ds.x0,dsp.x0),dmin(ds.x1,dsp.x1));
}

void pf_DSEG(DSEG ds) {
   printf("%g %g\n",ds.x0,ds.x1);
} 

void ppf_DSEG(DSEG ds,char *s) {
   if( ds.x0 <= ds.x1 ) {
      printf("%s:: (%g .. %g).\n",s,ds.x0,ds.x1);
   } else {
      printf("%s:: (Null).\n",s);
   }
}

void sf_DSEG(PDSEG pds) {
   if( scanf("%lf %lf",&pds->x0,&pds->x1) != 2 ) {
      pds->x0 = 1.0;
      pds->x1 = 0.0;
   }
}

/* Area under sawtooth (-1 .. 1) x h on ds. */

DSEG       saw_DSEG = {-1.0,1.0};

double A_saw_DSEG(DSEG ds,r_double h) {
#define       RA(a,b,h)   ((b - a) * h * (1.0 - 0.5 * (a + b)))
   r_double     A_r,  A_l;

   if( null_DSEG(and_DSEG(ds,saw_DSEG)) ) {
      return 0.0;
   } else {
      r_double     a,    b;
      a = ds.x0; b = ds.x1;
      A_r = b <=  0.0 ? 0.0 : RA(dmax(0.0,a),dmin(1.0,b),h);
      A_l = a >=  0.0 ? 0.0 : RA(fabs(dmin(0.0,b)),fabs(dmax(-1.0,a)),h);
   }
   return A_r + A_l;
#undef        RA
}
