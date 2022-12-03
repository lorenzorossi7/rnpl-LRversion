/* Generates uniform mesh on standard output. */

#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include "NEWGA.h"

#define  ON     1
#define  OFF    0

main(int argc,char **argv)
{
   double    xmin,     xmax;
   int       n; 

   int       ltrace    =  OFF;

   double    dx;
   int       i;

   switch( argc ) {
   case 4:
      GA(1,xmin,"%lf");    
      GA(2,xmax,"%lf");    
      GA(3,n,"%d");
      break;
   default:
      goto Usage;
      break;
   }
   if( ltrace ) fprintf(stderr,"Invocation:: %s %G %G %d.\n",         
                        argv[0],xmin,xmax,n);    
   if(      n == 1 ) {
      printf("%24.16E\n",xmin);
   } else if( n > 0 ) {
      dx = (xmax - xmin) / (n - 1);
      for( i = 0; i < n - 1; i++, xmin += dx ) {
         printf("%24.16E\n",xmin);
      } 
      printf("%24.16E\n",xmax);
   } else {
      goto Usage;
   }
   exit(0);
Usage:
   fprintf(stderr,"usage: %s <xmin> <xmax> <n > 0>\n",argv[0]);
   exit(-1);
}
