#include <stdio.h>
#include <math.h>
#include "svs.h" //LR: this seems to be needed by modern compilers
#include "bbhutil.h" //LR: this seems to be needed by modern compilers

int main(int argc,char **argv) {
   int     vsrc;
   double  x[101],   y[101];
   int     i,        n,      j;

   n = 101;
   for( i = 0; i < n; i++ ) {
      x[i] = 0.1 * i;
   }
   for( j = 0; j < n; j++ ) {
      for( i = 0; i < n; i++ ) {
         y[i] = cos(x[i] + 0.1 * j);
      }
      vsrc = vsxynt("c_thvs",0.1*j,x,y,n);
   }
   gft_close_all();
}
