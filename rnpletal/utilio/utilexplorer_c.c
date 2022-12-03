#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "utilexplorer_c.h"

/* Local version of dvdump ... */
 
void l_dvdump(double *v,int n,char *s) {
   int     i, per_line = 4;
 
   if( v != NULL ) {
      printf("<<< %s >>>\n\n",s);
      for( i = 0; i < n; i++ ) {
         printf( ((i + 1) % per_line ? "%19.10E" : "%19.10E\n"),v[i]);
      }
      printf( (n % per_line) ? "\n\n" : "\n" );
   }
}

/* Takes spherical "profile"  f(r), 2-sphere description and outputs 
   3D (cartesian) lattice suitable for input into ReadXYZ3DData ... */

void spherical_to_XYZ3DData0(char *fname,double *vf,double *vr,int nr,
                             double thmin,double thmax,int nth,
                             double phmin,double phmax,int nph) {
   double     dph,    ph,     cph,    sph;
   double     dth,    th,     cth,    sth;
   double     f,      r,      x,      y,      z;

   int        ir,     ith,    iph; 

   int        phclosed,       thsymmetric;

   FILE      *output;

   
   int        ltrace = 1;

   if( ltrace ) {
      fprintf(stderr,"spherical_to_XYZ3DData0: Parameter dump ...\n");
      fprintf(stderr,"fname: %s   nr: %d\n",fname,nr);
      l_dvdump(vf,nr,"f");
      l_dvdump(vr,nr,"r");
      fprintf(stderr,"thmin: %g  thmax: %g  nth: %d\n",
              thmin,thmax,nth);
      fprintf(stderr,"phmin: %g  phmax: %g  nph: %d\n",
              phmin,phmax,nph);
   } 

   if( nr < 1 ) {
      fprintf(stderr,
         "spherical_to_XYZ3DData0: nr(%d) < 1\n",nr);
      return;
   }
   if( nth < 1 || nph < 1 || thmin >= thmax || phmin >= phmax ) {
      fprintf(stderr,
         "spherical_to_XYZ3DData0: Bad 2-sphere args %g %g %d %g %g %d.\n",
         thmin,thmax,nth,phmin,phmax,nph);
      return;
   }

   if( ! (output = fopen(fname,"w")) ) {
      fprintf(stderr,
         "spherical_to_XYZ3DData0: Error opening %s for write.\n",fname);
      return;
   }

   dph = (phmax - phmin) / nph;
   phclosed = fuzzeq(cos(phmin),cos(phmax));
   dth = (thmax - thmin) / nth;
   thsymmetric = fuzzeq(abs(cos(thmin)),abs(cos(thmax)));

   if( ltrace ) {
      fprintf(stderr,
         "spherical_to_XYZ3DData0: ph %s closed.\n",
         phclosed ? "is" : "is not");
      fprintf(stderr,
         "spherical_to_XYZ3DData0: th %s symmetric.\n",
         thsymmetric ? "is" : "is not");
   }

   fprintf(output,"%d %d %d\n",nr,nth,nph);
   for( ir = 0; ir < nr; ir++ ) {
      r = vr[ir];
      f = vf[ir];
      for( iph = 0, ph = phmin; iph < nph; iph++, ph += dph ) {
         cph = cos(ph);
         sph = sin(ph);
         for( ith = 0, th = thmin; ith < nth; ith++, th += dth ) {
            sth = sin(th);
            cth = cos(th);
            x = r * sth * cph;
            y = r * sth * sph;
            z = r * cth;
            fprintf(output,"%g %g %g %g\n",x,y,z,f);
         }
      }
   }
   return;
}

int fuzzeq(double f1,double f2) {
   double   fuzz = 5.0e-5;
   return( fabs(f1 - f2) < fuzz );
}
