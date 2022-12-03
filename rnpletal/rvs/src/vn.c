/* Remote version of vn ... */

/* Dumps xy--vec read from standard input to server. */
/* Queries vs for window tag by name, optional time argument ... */

#include      <stdio.h>
#include      <string.h>
#include      <libgen.h>
#include      "rvs_cli.h"

#define        MAX_NPAIR      100000
#define        MAX_LABEL      256

main(int argc, char **argv) {
   char     *pgmname;
   double    x[MAX_NPAIR],    y[MAX_NPAIR];
   char     *label;
   double    time = 0.0;
   int       i,               rc;

   int       ltrace = 0;


   pgmname = basename(argv[0]);
   if( argc < 2 ) {
Usage:
      printf("usage: %s <window name> [<time>] < <list of xy pairs>.\n",
             pgmname);
      exit(-1);
   }
   label = argv[1];
   if( (argc > 2) && (sscanf(argv[2],"%lf",&time) != 1) ) goto Usage;
   for( i = 0; scanf("%lf %lf",x+i,y+i) == 2; ) {
      if( (++i) == MAX_NPAIR ) {
         printf("%s:: Internal buffer full, possible loss of data.\n",pgmname);
         goto full;
      }
   }
full:
   if( i == 0 ) {
      printf("%s:: Null input.\n",pgmname);
   } else {
      if( ltrace ) {
         printf("%s:: %d pairs read.\n",pgmname,i);
      }
      rc = vsxynt(label,time,x,y,i);
      if(         rc < 0 ) {
         printf("%s:: Could not transmit to server.\n",pgmname);
         exit(-1);
      } else if ( rc != i ) {
         printf("%s:: Error in data transmission.\n",pgmname);
         exit(-1);
      }
   }
   exit(0);
}
