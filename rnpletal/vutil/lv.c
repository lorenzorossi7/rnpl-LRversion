/* L O N G     V E C T O R     R O U T I N E S  */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <limits.h>
#include <time.h>
#include "v.h"

LVEC make_LVEC(int n) {
   return (LVEC) malloc(n * sizeof(long));
}

void free_LVEC(LVEC v) {
   if( v != (LVEC) NULL ) {
      free(v);
   }
}

LVEC Lvcopy(LVEC v1,int n) {
   LVEC  v2;
   int   i; 
   
   if( (v2 = make_LVEC(n)) != NULL ) {
      for( i = 0; i < n; i++ ) {
         v2[i] = v1[i];
      }
   } else {
      printf("Lvcopy: copy failed.\n");
   }
   return v2;
}

LVEC LVcopy(LVEC v1,LVEC v2,int n) {
   int   i; 
   
   if( (v1 != (LVEC) NULL) && (v2 != (LVEC) NULL) )  {
      for( i = 0; i < n; i++ ) {
         v2[i] = v1[i];
      }
   } 
   return v2;
}

void lvdump(LVEC v,int n,char *s) {
   int     i, per_line = 6;

   if( v != NULL ) {
      printf("<<< %s >>>\n\n",s);
      for( i = 0; i < n; i++ ) {
         printf( ((i + 1) % per_line ? "%12i" : "%12i\n"),v[i]);
      }
      printf( (n % per_line) ? "\n\n" : "\n" );
   }
}

long lvsum(LVEC v,int n) {
   long    j,   sum = 0;

   for( j = 0; j < n; sum += v[j++] );
 
   return sum;
}

long lvprod(LVEC v,int n) {
   long    i,  result;

   if( n > 0 ) {
      result = v[0];
      for( i = 1; i < n; i++ ) {
         result *= v[i];
      }
   } else {
      result = 0;
   }
   return  result;
}
