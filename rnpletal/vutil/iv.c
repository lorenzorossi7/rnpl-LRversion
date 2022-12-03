/* I N T E G E R     V E C T O R     R O U T I N E S  */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <limits.h>
#include <time.h>
#include "v.h"

int iv_imin(int i1,int i2) {
   return( i1 < i2 ? i1 : i2 );
}

int iv_imax(int i1,int i2) {
   return( i1 > i2 ? i1 : i2 );
}

IVEC make_IVEC(r_int n) {
   return (IVEC) malloc(n * sizeof(int));
}

void free_IVEC(r_IVEC v) {
   if( v != (IVEC) NULL ) {
      free(v);
   }
}

/* make IVEC with "periodic" extension. */

IVEC make_IVEC_p(r_int n) {
   r_IVEC  v;

   v = (IVEC) malloc( (n + 2) * sizeof(int) );
   return v != NULL ? v + 1 :  v;
}

void free_IVEC_p(r_IVEC v) {
   if( v != (IVEC) NULL ) {
      free(v-1);
   }
}

IVECN make_IVECN(int n) {
   IVECN    the;
   
   the.v = make_IVEC(n);
   the.n = n;
   return(the);
}

void free_IVECN(IVECN the) {
   if( the.v ) free(the.v);
}

IVECN free_and_null_IVECN(IVECN the) {
   if( the.v ) free(the.v);
   the.n = 0; the.v = (IVEC) NULL;
   return the;
}

IVECN IVECN_iota(int n) {
   IVECN    the = {(IVEC) NULL,0};
   if( (the.v = make_IVEC(n)) )  {
      int i;
      the.n = n;
      for( i = 0; i < n; i++ ) {
         the.v[i] = i+1;
      }
   }
   return(the);
}

/* Vector minmum and maximum. */ 

int ivmin(r_IVEC v,r_int n) {
   r_int      temp;
   r_int      i;

   temp = v[0];
   for( i = 1; i < n; i++ ) {
      if( v[i] < temp )
         temp = v[i];
   }
   return(temp);
}

int ivmax(r_IVEC v,r_int n) {
   r_int   temp;
   r_int      i;

   temp = v[0];
   for( i = 1; i < n; i++ ) {
      if( v[i] > temp )
         temp = v[i];
   }
   return(temp);
}

int ixymin_range(r_IVEC x,r_IVEC y,r_int n,r_int xmin,r_int xmax) {
   r_int      temp;
   r_int      i;

   temp = MAXINT;
   for( i = 0; i < n; i ++ ) {
      if( xmin <= x[i]  &&  x[i] <=  xmax ) {
         if( y[i] < temp ) temp = y[i];
      }
   }
   return temp;
}

int ixymax_range(r_IVEC x,r_IVEC y,r_int n,r_int xmin,r_int xmax) {
   r_int      temp;
   r_int      i;

   temp = -MAXINT;
   for( i = 0; i < n; i ++ ) {
      if( xmin <= x[i]  &&  x[i] <=  xmax ) {
         if( y[i] > temp ) temp = y[i];
      }
   }
   return temp;
}

void ivprint(IVEC v,int n,char *s) {
   int     i, per_line = 10;

   if( v != NULL ) {
      printf("<<< %s >>>\n\n",s);
      for( i = 0; i < n; i++ ) {
         printf( ((i + 1) % per_line ? "  %5d" : "  %5d\n"),v[i]);
      }
      printf( (n % per_line) ? "\n\n" : "\n" );
   }
}

void ivprint_inc(IVEC v,int n,int inc,char *s) {
   int     i, lines, per_line = 10;

   if( inc <= 0 ) inc = 1;
   if( v != NULL ) {
      printf("<<< %s >>>\n\n",s);
      for( i = lines = 0; i < n; ++lines, i+=inc ){
         printf( ((lines + 1) % per_line ? "  %5d" : "  %5d\n"),v[i]);
      }
      printf( (lines % per_line) ? "\n\n" : "\n" );
   }
}

IVEC Iviota(r_int n) {
   r_IVEC  v;
   r_int   i;

   if( (v = make_IVEC(n)) != NULL ) {
      for( i = 0; i < n; i++ )
         v[i] = (double) (i + 1);
   } else {
      printf("iviota: malloc failure.\n");
   }
   return v;
}

IVEC IViota(r_IVEC v,r_int n) {
   r_int   i;

   if( v != NULL ) {
      for( i = 0; i < n; i++ )
         v[i] = i + 1;
   }
   return v;
}

IVEC Ivcopy(r_IVEC v1,r_int n) {
   r_IVEC  v2;
   r_int   i; 
   
   if( (v2 = make_IVEC(n)) != NULL ) {
      for( i = 0; i < n; i++ ) {
         v2[i] = v1[i];
      }
   } else {
      printf("ivcopy: copy failed.\n");
   }
   return v2;
}

IVEC IVcopy(r_IVEC v1,r_IVEC v2,r_int n) {
   r_int   i; 
   
   if( (v1 != (IVEC) NULL) && (v2 != (IVEC) NULL) ) {
      for( i = 0; i < n; i++ ) {
         v2[i] = v1[i];
      }
   } 
   return v2;
}

void ivls(r_IVEC v,r_int sc,r_int n) {
   r_int    i;
   if( v != NULL ) {
      for( i = 0; i < n; i++ ) {
         v[i] = sc;
      }
   }
}

void ivva(r_IVEC v1,r_IVEC v2,r_IVEC v3,r_int n) {
   r_int    i;

   for( i = 0; i < n; i++ ) {
     v3[i] = v2[i] + v1[i];
   }
}

void ivvm(r_IVEC v1,r_IVEC v2,r_IVEC v3,r_int n) {
   r_int    i;

   for( i = 0; i < n; i++ ) {
     v3[i] = v2[i] * v1[i];
   }
}

void ivvs(r_IVEC v1,r_IVEC v2,r_IVEC v3,r_int n) {
   r_int    i;

   for( i = 0; i < n; i++ ) {
     v3[i] = v1[i] - v2[i];
   }
}

void ivsm(r_IVEC v1,r_int s1,r_IVEC v2,int n) {
   r_int    i;
   
   for( i = 0; i < n; i++ ) {
      v2[i] *= s1;
   }
}

void ivsa(r_IVEC v1,r_int s1,r_IVEC v2,int n) {
   r_int    i;

   for( i = 0; i < n; i++ ) {
      v2[i] += s1;
   }
}

IVEC IVva(r_IVEC v1,r_IVEC v2,r_IVEC v3,r_int n) {
   r_int    i;

   for( i = 0; i < n; i++ ) {
     v3[i] = v2[i] + v1[i];
   }
   return v3;
}

IVEC IVvm(r_IVEC v1,r_IVEC v2,r_IVEC v3,r_int n) {
   r_int    i;

   for( i = 0; i < n; i++ ) {
     v3[i] = v2[i] * v1[i];
   }
   return v3;
}

IVEC IVvs(r_IVEC v1,r_IVEC v2,r_IVEC v3,r_int n) {
   r_int    i;

   for( i = 0; i < n; i++ ) {
     v3[i] = v1[i] - v2[i];
   }
   return v3;
}

IVEC IVsm(r_IVEC v1,r_int s1,r_IVEC v2,int n) {
   r_int    i;
   
   for( i = 0; i < n; i++ ) {
      v2[i] *= s1;
   }
   return v2;
}

IVEC IVsa(r_IVEC v1,r_int s1,r_IVEC v2,int n) {
   r_int    i;

   for( i = 0; i < n; i++ ) {
      v2[i] += s1;
   }
   return v2;
}

int IVscanf(r_IVEC v,r_int n) {
   r_int      ln; 

   for( ln = 0; scanf("%d",v + ln) == 1; ln++ );
   return ln;
}

int IVprintf(r_IVEC v,r_int n) {
   int        j;
   
   for( j = 0; j < n; printf("%d\n",v[j++]));
   return n; 
}

int IVVprintf(r_IVEC v1,r_IVEC v2,r_int n) {
   int        j;
   
   for( j = 0; j < n; j++ ) {
      printf("%d %d\n",v1[j],v2[j]);
   }
   return n; 
}

#if !defined(INTEL_8_OR_LATER) 
IVEC  Ivrand(int imin,int imax,int n) {
   r_IVEC     v;
   r_int      scale;
   double     random;
   
   if( v = make_IVEC(n) ) {
      scale = imax - imin;
      for( ; n > 0; n-- ) {
         random = scale * drand48();
         v[n-1] = imin + (int) (random + 0.5);
         if( v[n-1] > imax ) --v[n-1];
      }
   }
   return  v;
}

IVEC  IVrand(IVEC v,int imin,int imax,int n) {
   r_int      scale;
   double     random;
   
   if( v ) {
      scale = imax - imin;
      for( ; n > 0; n-- ) {
         random = scale * drand48();
         v[n-1] = imin + (int) (random + 0.5);
         if( v[n-1] > imax ) --v[n-1];
      }
   }
   return  v;
}
#endif

int  ivsum(IVEC v,int n) {
   int     j,   sum = 0;

   for( j = 0; j < n; sum += v[j++] );
 
   return sum;
}

void ivdump(IVEC v,int n,char *s)
{
   int     i, per_line = 6;

   if( v != NULL ) {
      printf("<<< %s >>>\n\n",s);
      for( i = 0; i < n; i++ ) {
         printf( ((i + 1) % per_line ? "%12i" : "%12i\n"),v[i]);
      }
      printf( (n % per_line) ? "\n\n" : "\n" );
   }
}

void ivfdump(FILE *fp,IVEC v,int n,char *s)
{
   int     i, per_line = 6;

   if( v != NULL ) {
      fprintf(fp,"<<< %s >>>\n\n",s);
      for( i = 0; i < n; i++ ) {
         fprintf(fp, ((i + 1) % per_line ? "%12i" : "%12i\n"),v[i]);
      }
      fprintf(fp, (n % per_line) ? "\n\n" : "\n" );
   }
}

/* Sorting routines use qsort() ... */

void ivsortup(IVEC v, int n) {
   (void) qsort((char *) v,n,sizeof(int),(COMPAR) sort_up_i);
}

void ivsortdown(IVEC v, int n) {
   (void) qsort((char *) v,n,sizeof(int),(COMPAR) sort_down_i);
}

int sort_up_i(int *a, int *b)  {
   return (*a < *b) ? -1 : ((*a > *b) ? 1 : 0);
}

int sort_down_i(int *a,int *b) {
   return (*a > *b) ? -1 : ((*a < *b) ? 1 : 0);
}

void ivsortupuniq(IVEC vin, int nin, IVEC vout,int *pnout) {
   IVEC      vt;
   int       i, iout, nout;

   vt = Ivcopy(vin,nin);
   (void) ivsortup(vt,nin);
   for( nout = 1, i = 1; i < nin; i++ ) {
      if( vt[i] != vt[i-1] ) nout++;
   }
   vout[0] = vt[0];
   for( iout = 1, i = 1; i < nin; i++ ) {
      if( vt[i] != vt[i-1] ) {
         vout[iout++] = vt[i];
      }
   }
   *pnout = nout;
   free_IVEC(vt);
}

void ivsortdownuniq(IVEC vin, int nin, IVEC vout,int *pnout) {
   IVEC      vt;
   int       i, iout, nout;

   vt = Ivcopy(vin,nin);
   (void) ivsortdown(vt,nin);
   for( nout = 1, i = 1; i < nin; i++ ) {
      if( vt[i] != vt[i-1] ) nout++;
   }
   vout[0] = vt[0];
   for( iout = 1, i = 1; i < nin; i++ ) {
      if( vt[i] != vt[i-1] ) {
         vout[iout++] = vt[i];
      }
   }
   *pnout = nout;
   free_IVEC(vt);
}

IVECN ivecn_sortupuniq(IVECN vin) {
   IVECN vout = {NULL,0};
   IVEC  vout_v;
   int   nout;

   if( vin.n > 0 ) {
      vout_v = make_IVEC(vin.n);
      ivsortupuniq(vin.v,vin.n,vout_v,&nout);
      vout.v = Ivcopy(vout_v,nout);
      vout.n = nout;
      free_IVEC(vout_v);
   }
   return(vout);
}

IVECN ivecn_sortdownuniq(IVECN vin) {
   IVECN vout = {NULL,0};
   IVEC  vout_v;
   int   nout;

   if( vin.n > 0 ) {
      vout_v = make_IVEC(vin.n);
      ivsortdownuniq(vin.v,vin.n,vout_v,&nout);
      vout.v = Ivcopy(vout_v,nout);
      vout.n = nout;
      free_IVEC(vout_v);
   }
   return(vout);
}

int eq_fuzz_i(int x1,int x2,int fuzz) {

   if(        x1 == 0.0 ) {
      return abs(x2) <= fuzz;
   } else if( x2 == 0.0 ) {
      return abs(x1) <= fuzz;
   } else {
      return (abs(x1 - x2) / abs(x1)) <= fuzz;
   }

}

IVEC ivfscanf(FILE *fp,IVEC v,int *pn) {
   int  i = 0;
   if( fp && v ) {
      while( fscanf(fp,"%d",v+i) == 1 ) i++;
   }
   *pn = i;
   if( fp ) fclose(fp);
   return(v);
}

void ivfprintf(FILE *fp,IVEC v,int n) { 
   int   i = 0;
   if( fp && v ) {
      for( i = 0; i < n; i++ ) {
         fprintf(fp,"%d\n",v[i]);
      }
   }
}

int ivget(char *fname,IVEC v,int *pn) {
   FILE   *fp;
   if( fp = fopen(fname,"r") ) {
      v = ivfscanf(fp,v,pn);
      return(*pn);
   } else {
      return(0);
   }
}

IVEC Ivget(char *fname,int *pn) {
   IVEC    v = (IVEC) NULL;
   FILE   *fp;

   if( fp = fopen(fname,"r") ) {
      v = Ivfscanf(fp,pn);
   } 
   return(v);
}

IVECN ivecn_get(char *fname) {
   IVECN   the = {NULL,0};

   the.v = Ivget(fname,&the.n);
   return(the);
}

int ivput(char *fname,IVEC v,int n) {
   FILE   *fp;
   if( fp = fopen(fname,"w") ) {
      ivfprintf(fp,v,n);
      fclose(fp);
   }
   return(n);
}

void ivvfscanf(FILE *fp,IVEC v1,IVEC v2,int *pn) {
   int  i = 0;
   if( fp && v1 && v2 ) {
      while( fscanf(fp,"%d %d",v1+i,v2+i) == 2 ) i++;
   }
   *pn = i;
   if( fp ) fclose(fp);
}

int ivvget(char *fname,IVEC v1,IVEC v2,int *pn) {
   FILE    *fp;
   if( fp = fopen(fname,"r") ) {
      ivvfscanf(fp,v1,v2,pn);
      return(*pn);
   } else {
      return(0);
   }
}

int ivvput(char *fname,IVEC v1,IVEC v2,int n) {
   FILE    *fp;
   if( fp = fopen(fname,"w") ) {
      ivvfprintf(fp,v1,v2,n);
      fclose(fp);
      return(n);
   } else {
      return(0);
   }
}

void ivvfprintf(FILE *fp,IVEC v1,IVEC v2,int n) { 
   int   i = 0;
   if( fp && v1 && v2 ) {
      for( i = 0; i < n; i++ ) {
         fprintf(fp,"%d %d\n",v1[i],v2[i]);
      }
   }
}

IVEC Ivfscanf(FILE *fp,int *pn) {
   fpos_t   pos;
   IVEC     v;
   int      vi;
   int      i = 0;

   if( fp ) {
      (void) fgetpos(fp,&pos);
      while( fscanf(fp,"%d",&vi) == 1 ) i++;
      if( i > 0 ) {
         (void) fsetpos(fp,&pos);
         v = make_IVEC(i);
         i = 0;
         while( fscanf(fp,"%d",v+i) == 1 ) i++;
      }
   }
   *pn = i;
   if( fp ) fclose(fp);
   return(v);
}

IVEC ivdel_el(IVEC v,int n,int i) {
   IVEC    new_v;
   int     j,    new_j;

   if( 0 <= i && i < n ) {
      if( n == 1 ) {
         new_v = NULL;
      } else {
         new_v = make_IVEC(n-1);
         for( j = 0, new_j = 0; j < n; j++ ) {
            if( j != i ) new_v[new_j++] = v[j];
         }
      }
      free_IVEC(v);
      return new_v;
   } else {
      fprintf(stderr,"ivdel_el: Element %d out of range 0 .. %d\n",i,n);
      return v;
   }
}

/* Routine should not be called with non-malloced IVEC! ... */

IVEC ivappend(IVEC v,int n,int i) {
   IVEC  newv;
   if( !v ) {
      (newv = make_IVEC(1))[0] = i;
   } else {
      (newv = IVcopy(v,make_IVEC(n + 1),n))[n] = i;
      free(v);
   }
   return(newv);
}

IVECN ivecn_append(IVECN the,int i) {
   the.v = ivappend(the.v,the.n,i);
   ++the.n;
   return(the);
}

void ivecn_dump(IVECN the,char *s) {
   ivdump(the.v,the.n,s);
}

void ivramp(IVEC v,int v0,int iv,int n) {
   int       j; 

   if( n > 0 ) {
      v[0] = v0;
      for( j = 1; j < n; j++ ) {
         v[j] = v[j-1] + iv;
      }
   }
}

IVEC Ivramp(int v0,int iv,int n) {
   IVEC     v;

   if( v = make_IVEC(n) ) {
      ivramp(v,v0,iv,n);
   } else {
      printf("Ivramp:: make_IVEC(%d) fails.\n",n);
   }
   return   v;
}

int ivprod(int *v,int n) {
   int    i,  result;

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

/*--------------------------------------------------------------------------
   Grows an int array by factor of two, copying old values and zeroing
	new ones.  Pointer to original array must have been malloc'ed, as it 
	is free'd. 
--------------------------------------------------------------------------*/
int *Ivgrow(int *v, int n) {
   int  *vret = (int *) NULL;
   int      i;
   if( n && v ) {
      if( (vret = (int *) malloc(2 * n * sizeof(int))) ) {
         for( i = 0; i < n; i++ ) vret[i] = v[i];
         for( i = n; i < 2 * n; i++ ) vret[i] = 0;
         free(v);
      }
   }
   return vret;
}
