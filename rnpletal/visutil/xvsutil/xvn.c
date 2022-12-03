/*==============================================================================
Dumps xy--vec read from standard input to xvs.
==============================================================================*/
#include      <sysincludes.h>
#include      "libxvs_f.h"

#define        MAX_NPAIR      1000000

int main(int argc, char **argv) {
   double   *x, *y;
   char     *name;
   double    time, default_time = 0.0;
   int       i,               wt,
             rc;

   int       verbose = OFF;

   double    ltime;

   if( argc < 2 ) goto Usage;
   name = argv[1];
   if( argc > 2 ) {
      if( sscanf(argv[2],"%lf",&time) != 1 ) goto Usage;
   } else {
      time = default_time;
   }

   if( ! (x = (double *) malloc(MAX_NPAIR * sizeof(double))) ) goto Malloc;
   if( ! (y = (double *) malloc(MAX_NPAIR * sizeof(double))) ) goto Malloc;

   for( i = 0; scanf("%lf %lf",x+i,y+i) == 2; ) {
      if( (++i) == MAX_NPAIR ) {
         fprintf(stderr,"%s: Internal buffer full, possible loss of data.\n",argv[0]);
         goto full;
      }
   }
full:
   if( i == 0 ) {
      printf("%s: Null input.\n",argv[0]);
   } else {
      if( verbose ) {
         printf("%s: %d pairs read.\n",argv[0],i);
      }
      rc = vsxynt(name,time,x,y,i);
      if(         rc < 0 ) {
         fprintf(stderr,"%s: Could not transmit to server.\n",argv[0]);
         exit(-1);
      } else if ( rc != i ) {
         fprintf(stderr,"%s: Error in data transmission.\n",argv[0]);
         exit(-1);
      }
   }
   exit(0);

Usage:
   fprintf(stderr,"usage: %s <window name> [<time>] < <list of xy pairs>.\n\n",
           argv[0]);
   fprintf(stderr,"Note: Data is read from standard input.\n");
   exit(1);
Malloc:
   fprintf(stderr,"%s: Allocation of %d doubles failed\n",argv[0],MAX_NPAIR);
   exit(1);
}
