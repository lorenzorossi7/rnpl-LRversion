/* Generates uniform mesh on standard output. */

#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include "dv.h"
#include "NEWGA.h"

#define  ON     1
#define  OFF    0

main(argc,argv)
   int       argc;
   char    **argv;
{
   DVEC      x;
   double    xmin,     xmax,      rho;
   int       n; 

   int       ltrace    =  OFF;

   double    dx;
   int       i;

   switch( argc ) {
   case 5:
      GA(1,xmin,"%lf");    
      GA(2,xmax,"%lf");    
      GA(3,n,"%d");
      GA(4,rho,"%lf");
      break;
   default:
      goto Usage;
      break;
   }
   if( ltrace ) fprintf(stderr,"Invocation:: %s %G %G %d %g.\n",         
                        argv[0],xmin,xmax,n,rho);    
   if( n > 1 ) {
      if( x = make_DVEC(n) ) {
         rho = -rho;
         dvgmsh_(x,&n,&xmin,&xmax,&rho);
         dvfout(stdout,x,n);
      } else {
        fprintf(stderr,"%s: make_DVEC(%d) failed.\n",argv[0],n);
      }
   } else {
      goto Usage;
   }
   exit(0);
Usage:
   fprintf(stderr,"usage: %s <xmin> <xmax> <n .gt. 1> <rho>.\n",argv[0]);
   exit(-1);
}
