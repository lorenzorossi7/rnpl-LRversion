#include <stdio.h>
#include <stdlib.h>
#include "./bbhutil.h"

int main(int argc, char **argv) {
   char *R = argv[0];
   int np, nxa;
   int d2, i, rc;
   double *data1 = (double *) NULL;
   double *data2 = (double *) NULL;

   int ltrace = 1;

   if( argc < 3 ) goto Usage;
   if( sscanf(argv[1],"%d",&np) != 1 ) goto Usage;
   if( sscanf(argv[2],"%d",&nxa) != 1 ) goto Usage;
   fprintf(stderr,"Tpart: np=%d  nxa=%d\n",np,nxa);

   /* Generate some random data */
   d2 = 3;
   if( !(data1 = (double *) malloc(np*d2*sizeof(double))) ) {
      fprintf(stderr,"%s: malloc(%d x %d) doubles failed.\n",R,np,d2);
   }
   for( i = 0; i < np * d2; i++ ) {
      data1[i] = drand48();
   }

   rc = gft_out_part_xyz("Tpart_xyz",-1.0,data1,data1+np,data1+2*np,np);

   d2 = 3 + nxa;
   if( !(data2 = (double *) malloc(np*d2*sizeof(double))) ) {
      fprintf(stderr,"%s: malloc(%d x %d) doubles failed.\n",R,np,d2);
   }
   for( i = 0; i < np * d2; i++ ) {
      data2[i] = drand48();
   }

   rc = gft_out_part_xyzf("Tpart_xyzf",-1.0,data2,np,nxa);

   exit(0);
Usage:
   fprintf(stderr,"usage: Tpart <np> <nxa>\n");
   exit(1);
}
