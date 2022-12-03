/* Remote version of v1 ... */

#include      <stdio.h>
#include      <string.h>
#include      <libgen.h>
#include      "rvs_cli.h"

#define        MAX_NPAIR      100000
#define        MAX_LABEL      256

main(int argc, char **argv) {
   double    x[MAX_NPAIR],    y[MAX_NPAIR];
   char     *label;
   int       i,               rc;

   int       verbose = 0;

   if( argc > 2 ) {
      printf("usage: %s <list of xy pairs>.\n",argv[0]);
      exit(-1);
   }
   if( argc > 1 ) {
      label = argv[1];
   } else {
      label = (char *) malloc(MAX_LABEL * sizeof(char));
      label = strcpy(label,"Standard input");
   }
   for( i = 0; scanf("%lf %lf",x+i,y+i) == 2; ) {
      if( (++i) == MAX_NPAIR ) {
         printf("%s:: Internal buffer full, possible loss of data.\n",argv[0]);
         goto full;
      }
   }
full:
   if( i == 0 ) {
      printf("%s:: Null input.\n",argv[0]);
   } else {
      if( verbose ) {
         printf("%s:: %d pairs read.\n",argv[0],i);
      }
      if( (rc = vsxynt(label,0.0,x,y,i)) != i ) {
         printf("%s:: Could not transmit to server.\n",argv[0]);
         printf("%s:: Error code: %d\n",argv[0],rc);
      }
   }
   exit(0);
}
