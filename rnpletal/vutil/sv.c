/* S I N G L E    P R E C I S I O N    V E C T O R     R O U T I N E S  */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <limits.h>
#include <time.h>
#include "v.h"

#define IL(_code) int j; for( j = 0; j < n; j++ ) { _code; }
#define IL_j(_jmin,_jmax,_code) int j; for( j = _jmin; j < _jmax; j++ )  \
                                { _code; }

float sv_smin(float s1,float s2) {
   return( s1 < s2 ? s1 : s2 );
}

float sv_smax(float s1,float s2) {
   return( s1 > s2 ? s1 : s2 );
}

SVEC make_SVEC(r_int n) {
   return (SVEC) malloc(n * sizeof(float));
}

void free_SVEC(r_SVEC v) {
   if( v != (SVEC) NULL ) {
      free(v);
   }
}

/* make SVEC with "periodic" extension. */

SVEC make_SVEC_p(r_int n) {
   r_SVEC  v;

   v = (SVEC) malloc( (n + 2) * sizeof(float) );
   return v != NULL ? v + 1 :  v;
}

void free_SVEC_p(r_SVEC v) {
   if( v != (SVEC) NULL ) {
      free(v-1);
   }
}

SVECN make_SVECN(int n) {
   SVECN    the;
   
   the.v = make_SVEC(n);
   the.n = n;
   return(the);
}

void free_SVECN(SVECN the) {
   if( the.v ) free(the.v);
}

SIVEC make_SIVEC(r_int n) {
   return (SIVEC) malloc(n * sizeof(SI));
}

void  free_SIVEC(SIVEC v) {
   if( v ) free(v);
}
   

/* Vector minmum and maximum. */ 

float svmin(r_SVEC v,r_int n) {
   r_float   temp;
   r_int      i;

   temp = v[0];
   for( i = 1; i < n; i++ ) {
      if( v[i] < temp )
         temp = v[i];
   }
   return(temp);
}

float svmax(r_SVEC v,r_int n) {
   r_float   temp;
   r_int      i;

   temp = v[0];
   for( i = 1; i < n; i++ ) {
      if( v[i] > temp )
         temp = v[i];
   }
   return(temp);
}

float sxymin_range(r_SVEC x,r_SVEC y,r_int n,r_float xmin,r_float xmax) {
   r_float   temp;
   r_int      i;

   temp = MAXFLOAT;
   for( i = 0; i < n; i ++ ) {
      if( xmin <= x[i]  &&  x[i] <=  xmax ) {
         if( y[i] < temp ) temp = y[i];
      }
   }
   return temp;
}

float sxymax_range(r_SVEC x,r_SVEC y,r_int n,r_float xmin,r_float xmax) {
   r_float   temp;
   r_int      i;

   temp = -MAXFLOAT;
   for( i = 0; i < n; i ++ ) {
      if( xmin <= x[i]  &&  x[i] <=  xmax ) {
         if( y[i] > temp ) temp = y[i];
      }
   }
   return temp;
}

void svprint(SVEC v,int n,char *s) {
   int     i, per_line = 6;

   if( v != NULL ) {
      printf("<<< %s >>>\n\n",s);
      for( i = 0; i < n; i++ ) {
         printf( ((i + 1) % per_line ? "  %f" : "  %f\n"),v[i]);
      }
      printf( (n % per_line) ? "\n\n" : "\n" );
   }
}

void sivprint(SIVEC v,int n,char *s) {
   int     i, per_line = 4;

   if( v  ) {
      printf("<<< %s >>>\n\n",s);
      for( i = 0; i < n; i++ ) {
         printf( ((i + 1) % per_line ? "  %9e:%4d" : "  %9e:%4d\n"),
                 v[i].x,v[i].ix);
      }
      printf( (n % per_line) ? "\n\n" : "\n" );
   }
}

void svprint_inc(SVEC v,int n,int inc,char *s) {
   int     i, lines, per_line = 6;

   if( inc <= 0 ) inc = 1;
   if( v != NULL ) {
      printf("<<< %s >>>\n\n",s);
      for( i = lines = 0; i < n; ++lines, i+=inc ){
         printf( ((lines + 1) % per_line ? "  %f" : "  %f\n"),v[i]);
      }
      printf( (lines % per_line) ? "\n\n" : "\n" );
   }
}

void svprint_range(SVEC v,int n,char *s) {
   printf("svprint_range:: %s: %15g %15g.\n",s,svmin(v,n),svmax(v,n));
}

SVEC Sviota(r_int n) {
   r_SVEC  v;
   r_int   i;

   if( (v = make_SVEC(n)) != NULL ) {
      for( i = 0; i < n; i++ )
         v[i] = (float) (i + 1);
   } else {
      printf("Sviota: malloc failure.\n");
   }
   return v;
}

SVEC SViota(r_SVEC v,r_int n) {
   r_int   i;

   if( v != NULL ) {
      for( i = 0; i < n; i++ )
         v[i] = (float) (i + 1);
   }
   return v;
}

SVEC Svcopy(r_SVEC v1,r_int n) {
   r_SVEC  v2;
   r_int   i; 
   
   if( (v2 = make_SVEC(n)) != NULL ) {
      for( i = 0; i < n; i++ ) {
         v2[i] = v1[i];
      }
   } else {
      printf("Svcopy: copy failed.\n");
   }
   return v2;
}

SVEC SVcopy(r_SVEC v1,r_SVEC v2,r_int n) {
   r_int   i; 
   
   if( (v1 != (SVEC) NULL) && (v2 != (SVEC) NULL) ) {
      for( i = 0; i < n; i++ ) {
         v2[i] = v1[i];
      }
   } 
   return v2;
}

void svva(r_SVEC v1,r_SVEC v2,r_SVEC v3,r_int n) {
   r_int    i;

   for( i = 0; i < n; i++ ) {
     v3[i] = v2[i] + v1[i];
   }
}

void svvm(r_SVEC v1,r_SVEC v2,r_SVEC v3,r_int n) {
   r_int    i;

   for( i = 0; i < n; i++ ) {
     v3[i] = v2[i] * v1[i];
   }
}

void svvs(r_SVEC v1,r_SVEC v2,r_SVEC v3,r_int n) {
   r_int    i;

   for( i = 0; i < n; i++ ) {
     v3[i] = v1[i] - v2[i];
   }
}

void svsm(r_SVEC v1,r_float s1,r_SVEC v2,r_int n) {
   r_int    i;
   
   for( i = 0; i < n; i++ ) {
      v2[i] = v1[i] * s1;
   }
}

void svsa(r_SVEC v1,r_float s1,r_SVEC v2,r_int n) {
   r_int    i;

   for( i = 0; i < n; i++ ) {
      v2[i] = v1[i] + s1;
   }
}

SVEC SVva(r_SVEC v1,r_SVEC v2,r_SVEC v3,r_int n) {
   r_int    i;

   for( i = 0; i < n; i++ ) {
     v3[i] = v2[i] + v1[i];
   }
   return v3;
}

SVEC SVvm(r_SVEC v1,r_SVEC v2,r_SVEC v3,r_int n) {
   r_int    i;

   for( i = 0; i < n; i++ ) {
     v3[i] = v2[i] * v1[i];
   }
   return v3;
}

SVEC SVvs(r_SVEC v1,r_SVEC v2,r_SVEC v3,r_int n) {
   r_int    i;

   for( i = 0; i < n; i++ ) {
     v3[i] = v1[i] - v2[i];
   }
   return v3;
}

SVEC SVsm(r_SVEC v1,r_float s1,r_SVEC v2,r_int n) {
   r_int    i;
   
   for( i = 0; i < n; i++ ) {
      v2[i] *= s1;
   }
   return v2;
}

SVEC SVsa(r_SVEC v1,r_float s1,r_SVEC v2,r_int n) {
   r_int    i;

   for( i = 0; i < n; i++ ) {
      v2[i] += s1;
   }
   return v2;
}

#if !defined(INTEL_8_OR_LATER) 
SVEC SVrand(r_SVEC v,r_float v_min,r_float v_max,r_int n) {
   r_float  v_range;
   r_int     i;

   if( (v_range = (v_max - v_min)) <= 0.0 ) {
      printf("SVrand:: Invalid range. v_min: %g  v_max: %g.\n",
                v_min,v_max);
   }
   for( i = 0; i < n; i++ ) {
      v[i] = v_min + v_range * (float) drand48();
   }
   return v;
}
#endif

int SVscanf(r_SVEC v,r_int n) {
   r_int      ln; 

   for( ln = 0; scanf("%f",v + ln) == 1; ln++ );
   return ln;
}

int SVprintf(SVEC v,r_int n) {
   int        j;
   
   for( j = 0; j < n; printf("%g\n",v[j++]));
   return n; 
}

int SVVscanf(r_SVEC v1,r_SVEC v2,r_int n) {
   r_int       ln;

   for( ln = 0; scanf("%f %f",v1 + ln,v2 + ln) == 2; ln++ );
   return ln;
}

int SVVprintf(r_SVEC v1,r_SVEC v2,r_int n)  {
   int        j;
   
   for( j = 0; j < n; j++ ) {
      printf("%e %e\n",v1[j],v2[j]);
   }
   return n; 
}

void svls(r_SVEC v1,float s1,int n) {
   IL( v1[j] = s1 )
}

/* Only affects n-1 elements. */

void svav(r_SVEC v1,r_SVEC v2,int n) {
   IL_j( 1, n-1, v2[j] = 0.5 * (v1[j] + v1[j+1]) )
}

/* Returns number of non--zero elements. */

int isvnz(SVEC v,int n) {
   int       s = 0;

   IL( if( (int) v[j] ) ++s; )
   return s;
}

void svramp(SVEC v,float v0,float dv,int n) {
   int       j; 

   if( n > 0 ) {
      v[0] = v0;
      for( j = 1; j < n; j++ ) {
         v[j] = v[j-1] + dv;
      }
   }
}

SVEC Svramp(float v0,float dv,int n) {
   SVEC     v;

   if( v = make_SVEC(n) ) {
      svramp(v,v0,dv,n);
   } else {
      printf("Svramp:: make_SVEC(%d) fails.\n",n);
   }
   return   v;
}

SVEC Svumesh(float v0,float vnm1,int n) {
   SVEC      v = (SVEC) NULL;

   if( n >= 1 ) {
      if( n == 1 ) {
         if( v = make_SVEC(1) ) {
            v[0] = v0;
         } else {
            fprintf(stderr,"Svumesh: make_SVEC(%d) fails.\n",n);
         }
      } else {
         if( v = make_SVEC(n) ) {
            svramp(v,v0,(vnm1 - v0) / (n - 1),n);
            v[n-1] = vnm1;
         } else {
            fprintf(stderr,"Svumesh: make_SVEC(%d) fails.\n",n);
         }
      }
   }
   return( v );

}

float svsum(SVEC v,int n)  {
   float    sum = 0.0;

   IL( sum += v[j] )
   return sum;
}

void svdd01(SVEC v1,SVEC v2,float h,int n) {
   float    half_hm1;
   int      j;

   half_hm1 = 0.5 / h;
   for( j = 0; j < n; j++ ) {
      v2[j] = half_hm1 * (v1[j+1] - v1[j-1]); 
   }
}

void svdump(SVEC v,int n,char *s) {
   int     i, per_line = 4;

   if( v != NULL ) {
      printf("<<< %s >>>\n\n",s);
      for( i = 0; i < n; i++ ) {
         printf( ((i + 1) % per_line ? "%19.10E" : "%19.10E\n"),v[i]);
      }
      printf( (n % per_line) ? "\n\n" : "\n" );
   }
}

float srange(float xmin,float x,float xmax,float fuzz) {
   float     sx = fabsf(fuzz * (xmax - xmin));

   if( xmin <= xmax ) {
      return (xmin - sx <= x) && (x <= xmax + sx);
   } else {
      return (xmax - sx <= x) && (x <= xmin + sx);
   }
}

void svfout(FILE *fout,SVEC v,int n) {
   int      i;

   for( i = 0; i < n; i++ ) fprintf(fout,"%24.16E\n",v[i]);
}

/* Sorting routines use qsort() ... */

void svsortup(SVEC v, int n) {
   (void) qsort((char *) v,n,sizeof(float),(COMPAR) sort_up_f);
}

void svsortdown(SVEC v, int n) {
   (void) qsort((char *) v,n,sizeof(float),(COMPAR) sort_down_f);
}

int sort_up_f(float *a, float *b)  {
   return (*a < *b) ? -1 : ((*a > *b) ? 1 : 0);
}

int sort_down_f(float *a,float *b) {
   return (*a > *b) ? -1 : ((*a < *b) ? 1 : 0);
}

void svsortupuniq(SVEC vin, int nin, SVEC vout,int *pnout,float fuzz) {
   SVEC      vt;
   int       i, iout, nout;

   vt = Svcopy(vin,nin);
   (void) svsortup(vt,nin);
   for( nout = 1, i = 1; i < nin; i++ ) {
      if( !eq_fuzz_f(vt[i],vt[i-1],fuzz) ) nout++;
   }
   vout[0] = vt[0];
   for( iout = 1, i = 1; i < nin; i++ ) {
      if( !eq_fuzz_f(vt[i],vt[i-1],fuzz) ) {
         vout[iout++] = vt[i];
      }
   }
   *pnout = nout;
   free_SVEC(vt);
}

void svsortdownuniq(SVEC vin, int nin, SVEC vout,int *pnout,float fuzz) {
   SVEC      vt;
   int       i, iout, nout;

   vt = Svcopy(vin,nin);
   (void) svsortdown(vt,nin);
   for( nout = 1, i = 1; i < nin; i++ ) {
      if( !eq_fuzz_f(vt[i],vt[i-1],fuzz) ) nout++;
   }
   vout[0] = vt[0];
   for( iout = 1, i = 1; i < nin; i++ ) {
      if( !eq_fuzz_f(vt[i],vt[i-1],fuzz) ) {
         vout[iout++] = vt[i];
      }
   }
   *pnout = nout;
   free_SVEC(vt);
}

int eq_fuzz_f(float x1,float x2,float fuzz) {

   if(        x1 == 0.0 ) {
      return fabsf(x2) <= fuzz;
   } else if( x2 == 0.0 ) {
      return fabsf(x1) <= fuzz;
   } else {
      return (fabsf(x1 - x2) / fabsf(x1)) <= fuzz;
   }

}

int svlsindex(SVEC v,int n,float key,float fuzz) {
   int      i;
   
   for( i = 0; i < n; i++ ) {
      if( eq_fuzz_f(v[i],key,fuzz) ) return i;
   }
   return -1;
}

IVEC Svlsindex(SVEC v,int n,SVEC key,int nkey,float fuzz) {
   IVEC     I;
   int      i;
   
   I = make_IVEC_sv(nkey);
   for( i = 0; i < nkey; i++ ) {
      I[i] = svlsindex(v,n,key[i],fuzz);
   }
   return I;
}

IVEC make_IVEC_sv(r_int n) {
   return (IVEC) malloc(n * sizeof(int));
}


void svvmergeup(SVEC v1,int n1,SVEC v2,int n2,SVEC v3,int *pn3,float fuzz) {
   SVEC     vt1,    vt2;

   vt1 = make_SVEC(n1 + n2);
   (void) SVcopy(v1,vt1,n1);
   (void) SVcopy(v2,vt1+n1,n2);
   (void) svsortupuniq(vt1,n1+n2,v3,pn3,fuzz);
   free_SVEC(vt1);
}

void svvmergedown(SVEC v1,int n1,SVEC v2,int n2,SVEC v3,int *pn3,float fuzz) {
   SVEC     vt1,    vt2;

   vt1 = make_SVEC(n1 + n2);
   (void) SVcopy(v1,vt1,n1);
   (void) SVcopy(v2,vt1+n1,n2);
   (void) svsortdownuniq(vt1,n1+n2,v3,pn3,fuzz);
   free_SVEC(vt1);
}

void svmrg(SVEC v1,SVEC v2,SVEC vmrg,int n) {
   int j;

   for( j = 0; j < n; j++ ) {
      vmrg[2*j] = v1[j];
      vmrg[2*j+1] = v2[j];
   }
}

void svumrg(SVEC vmrg,SVEC v1,SVEC v2,int n) {
   int j;

   for( j = 0; j < n; j++ ) {
      v1[j] = vmrg[2*j];
      v2[j] = vmrg[2*j+1];
   }
}

int sv2sort(SVEC v1,SVEC v2,int n,int code) {
   SVEC     vmrg;
   
   switch( code ) {
   case SORT_UP:
   case SORT_DOWN:
      break;
   default:
      fprintf(stderr,"sv2sort: Invalid code: %d\n",code);
      return -1;
   }
   if( !(vmrg = make_SVEC(2 * n)) ) {
      fprintf(stderr,"sv2sort: make_SVEC(%d) failed.\n",2 * n);
      return -1;
   }
   svmrg(v1,v2,vmrg,n);
   switch( code ) {
   case SORT_UP:
      qsort(vmrg,n,sizeof(S2),(COMPAR) sort_up_s2);
      break;
   case SORT_DOWN:
      qsort(vmrg,n,sizeof(S2),(COMPAR) sort_down_s2);
      break;
   default:
      break;
   }
   svumrg(vmrg,v1,v2,n);
   free_SVEC(vmrg);

   return 0;
}

int sort_up_s2(PS2 el1,PS2 el2) {
   return (el1->x1 < el2->x1) ? -1 : ((el1->x1 > el2->x1) ? 1 : 0);
}

int sort_down_s2(PS2 el1,PS2 el2) {
   return (el1->x1 > el2->x1) ? -1 : ((el1->x1 < el2->x1) ? 1 : 0);
}

SVEC svdel_el(SVEC v,int n,int i) {
   SVEC    new_v;
   int     j,    new_j;

   if( 0 <= i && i < n ) {
      if( n == 1 ) {
         new_v = NULL;
      } else {
         new_v = make_SVEC(n-1);
         for( j = 0, new_j = 0; j < n; j++ ) {
            if( j != i ) new_v[new_j++] = v[j];
         }
      }
      free_SVEC(v);
      return new_v;
   } else {
      fprintf(stderr,"svdel_el: Element %d out of range 0 .. %d\n",i,n);
      return v;
   }
}

int sv2insert(SVEC *pv1,SVEC *pv2,int n,float new1,float new2,int code) {
   SVEC     new_v1,      new_v2;
   int      new_n;

   new_n = n + 1;
   if( !(new_v1 = make_SVEC(new_n)) || !(new_v2 = make_SVEC(new_n)) ) {
      fprintf(stderr,"sv2insert: make_SVEC(%d) failed.\n",new_n);
      return n;
   } else {
      new_v1 = SVcopy(*pv1,new_v1,n); new_v1[n] = new1;
      new_v2 = SVcopy(*pv2,new_v2,n); new_v2[n] = new2;
      if( !sv2sort(new_v1,new_v2,new_n,code) ) {
         free_SVEC(*pv1); *pv1 = new_v1;
         free_SVEC(*pv2); *pv2 = new_v2;
         return new_n;
      } else {
         fprintf(stderr,"sv2insert: sv2sort failed.\n");
         free_SVEC(new_v1);
         free_SVEC(new_v2);
         return n;
      }
   }
}

#define   NEWEXT(a)     iext = a; vext = v[a];
#define   NEWABSEXT(a)  iext = a; vext = fabsf(v[a]);
int ixsvmin(SVEC v,int n) {
   float   vext;
   int      i,    iext;

   if( n < 1 ) return -1;

   NEWEXT(0);
   for( i = 1 ; i < n; i++ ) {
      if( v[i] < vext ) { NEWEXT(i) }
   }
   return iext;
}

int ixsvmax(SVEC v,int n) {
   float   vext;
   int      i,    iext;

   if( n < 1 ) return -1;

   NEWEXT(0);
   for( i = 1 ; i < n; i++ ) {
      if( v[i] > vext ) { NEWEXT(i) }
   }
   return iext;
}

int ixsvabsmin(SVEC v,int n) {
   float   vext;
   int      i,    iext;

   if( n < 1 ) return -1;

   NEWABSEXT(0);
   for( i = 1 ; i < n; i++ ) {
      if( fabsf(v[i]) < vext ) { NEWABSEXT(i) }
   }
   return iext;
}

int ixsvabsmax(SVEC v,int n) {
   float   vext;
   int      i,    iext;

   if( n < 1 ) return -1;

   NEWABSEXT(0);
   for( i = 1 ; i < n; i++ ) {
      if( fabsf(v[i]) > vext ) { NEWABSEXT(i) }
   }
   return iext;
}

#undef    NEWABSEXT
#undef    NEWEXT

int ixsvnearest(SVEC v,int n,float key) {
   SVEC   tv;
   int    i;
   
   if( n < 1 ) return -1;

   if( !(tv = make_SVEC(n)) ) {
      fprintf(stderr,"ixsvnearest: make_SVEC(%d) failed.\n",n);
      return -1;
   }
   svsa(v,-key,tv,n);
   i = ixsvabsmin(tv,n);
   free_SVEC(tv);
   return i;
}

int ixsv2nearest(SVEC v1,SVEC v2,int n,float key1,float key2,float asp1by2) {
   SVEC   tv1,    tv2,     tv3;
   int    i;
   
   if( n < 1 ) return -1;

   if( !(tv1 = make_SVEC(n)) || !(tv2 = make_SVEC(n)) || 
       !(tv3 = make_SVEC(n)) ) {
      fprintf(stderr,"ixsvnearest: make_SVEC(%d) failed.\n",n);
      return -1;
   }
   svsa(v1,-key1,tv1,n);
   svsa(v2,-key2,tv2,n);
   svsm(tv2,asp1by2,tv2,n);
   
   svpyth(tv1,tv2,tv3,n);
   i = ixsvabsmin(tv3,n);
   free_SVEC(tv1); free_SVEC(tv2); free_SVEC(tv3);
   return i;
}

void svpyth(SVEC v1,SVEC v2,SVEC v3,int n) {
   int   i;
   for( i = 0; i < n; i++ ) {
      v3[i] = sqrtf(v1[i]*v1[i] + v2[i]*v2[i]);
   }
}

float svhash(SVEC v,int n) {
   float val = 0;  
   int    j;

   for( j = 0; j < n; j++ ) {
      val += (v[j] + (float) (j+1)) * (v[j] + (double) (j+1));
   }
   val = sqrtf(val / n);
   return val;
}

void svfapl(SVEC v1,SVEC v2,PFF f,int n) {
   int     i;

   for( i = 0; i < n; i++ ) {
      v2[i] = (*f)(v1[i]);
   }
}

SVEC Svfapl(SVEC v1,PFF f,int n) {
   SVEC    v2 = make_SVEC(n);
   int     i;

   if( v2 ) {
      for( i = 0; i < n; i++ ) {
         v2[i] = (*f)(v1[i]);
      }
   }
   return(v2);
}

SVEC svfscanf(FILE *fp,SVEC v,int *pn) {
   int  i = 0;
   if( fp && v ) {
      while( fscanf(fp,"%f",v+i) == 1 ) i++;
   }
   *pn = i;
   if( fp ) fclose(fp);
   return(v);
}

void svfprintf(FILE *fp,SVEC v,int n) { 
   int   i = 0;
   if( fp && v ) {
      for( i = 0; i < n; i++ ) {
         fprintf(fp,"%13.6e\n",v[i]);
      }
   }
}

int svget(char *fname,SVEC v,int *pn) {
   FILE   *fp;
   if( fp = fopen(fname,"r") ) {
      v = svfscanf(fp,v,pn);
      return(*pn);
   } else {
      return(0);
   }
}

SVEC Svget(char *fname,int *pn) {
   SVEC    v = (SVEC) NULL;
   FILE   *fp;

   if( fp = fopen(fname,"r") ) {
      v = Svfscanf(fp,pn);
   } 
   return(v);
}

int svput(char *fname,SVEC v,int n) {
   FILE   *fp;
   if( fp = fopen(fname,"w") ) {
      svfprintf(fp,v,n);
      fclose(fp);
   }
   return(n);
}

void svvfscanf(FILE *fp,SVEC v1,SVEC v2,int *pn) {
   int  i = 0;
   if( fp && v1 && v2 ) {
      while( fscanf(fp,"%f %f",v1+i,v2+i) == 2 ) i++;
   }
   *pn = i;
   if( fp ) fclose(fp);
}

int svvget(char *fname,SVEC v1,SVEC v2,int *pn) {
   FILE    *fp;
   if( fp = fopen(fname,"r") ) {
      svvfscanf(fp,v1,v2,pn);
      return(*pn);
   } else {
      return(0);
   }
}

int svvput(char *fname,SVEC v1,SVEC v2,int n) {
   FILE    *fp;
   if( fp = fopen(fname,"w") ) {
      svvfprintf(fp,v1,v2,n);
      fclose(fp);
      return(n);
   } else {
      return(0);
   }
}

void svvfprintf(FILE *fp,SVEC v1,SVEC v2,int n) { 
   int   i = 0;
   if( fp && v1 && v2 ) {
      for( i = 0; i < n; i++ ) {
         fprintf(fp,"%13.6e %13.6e\n",v1[i],v2[i]);
      }
   }
}

SVEC Svfscanf(FILE *fp,int *pn) {
   fpos_t   pos;
   SVEC     v;
   float    vi;
   int      i = 0;

   if( fp ) {
      (void) fgetpos(fp,&pos);
      while( fscanf(fp,"%f",&vi) == 1 ) i++;
      if( i > 0 ) {
         (void) fsetpos(fp,&pos);
         v = make_SVEC(i);
         i = 0;
         while( fscanf(fp,"%f",v+i) == 1 ) i++;
      }
   }
   *pn = i;
   if( fp ) fclose(fp);
   return(v);
}

PSVEC Svvfscanf(FILE *fp,int *pn) {
   PSVEC    p;
   fpos_t   pos;
   float    v1i,    v2i;
   int      i = 0;

   p.v1 = (SVEC) NULL;
   p.v2 = (SVEC) NULL;
   if( fp ) {
      (void) fgetpos(fp,&pos);
      while( fscanf(fp,"%f %f",&v1i,&v2i) == 2 ) i++;
      if( i > 0 ) {
         (void) fsetpos(fp,&pos);
         p.v1 = make_SVEC(i);
         p.v2 = make_SVEC(i);
         i = 0;
         while( fscanf(fp,"%f %f",p.v1+i,p.v2+i) == 2 ) i++;
      }
   }
   *pn = i;
   if( fp ) fclose(fp);
   return(p);
}

PSVEC Svvget(char *fname,int *pn) {
   PSVEC   p;
   FILE   *fp;

   p.v1 = (SVEC) NULL;
   p.v2 = (SVEC) NULL;
   if( fp = fopen(fname,"r") ) {
      p = Svvfscanf(fp,pn);
      if( fp ) fclose(fp);
   }
   return(p);
}

/* Routine should not be called with non-malloced SVEC! ... */

SVEC svappend(SVEC v,int n,float val) {
   SVEC  newv;
   if( !v ) {
      (newv = make_SVEC(1))[0] = val;
   } else {
      (newv = SVcopy(v,make_SVEC(n + 1),n))[n] = val;
      free(v);
   }
   return(newv);
}

SVECN svecn_append(SVECN the,float val) {
   the.v = svappend(the.v,the.n,val);
   ++the.n;
   return(the);
}

IVEC ivmakemasksv(SVEC vin,int nin,PFI_F logic) {
   IVEC      vout = (IVEC) NULL;
   int       i;

   if( nin > 0 ) {
      if( (vout = make_IVEC(nin)) ) {
         for( i = 0; i < nin; i++ ) {
            vout[i] = (*logic)(vin[i]);
         }
      } else {
         fprintf(stderr,"ivmakemasksv: make_IVEC(%d) failed\n",nin);
      }
   }
   return  vout;
}

IVEC ivmakemasksvsvsv(SVEC vin1, SVEC vin2, SVEC vin3,int nin,PFI_FFF logic) {
   IVEC      vout = (IVEC) NULL;
   int       i;

   if( nin > 0 ) {
      if( (vout = make_IVEC(nin)) ) {
         for( i = 0; i < nin; i++ ) {
            vout[i] = (*logic)(vin1[i],vin2[i],vin3[i]);
         }
      } else {
         fprintf(stderr,"ivmakemasksvsvsv: make_IVEC(%d) failed\n",nin);
      }
   }
   return  vout;
}

SVEC svreduce(SVEC vin,IVEC mask,int nin,int *pnout) {
   SVEC      vout = (SVEC) NULL;
   int       i;

   *pnout = 0;
   for( i = 0; i < nin; i++ ) {
      if( mask[i] ) (*pnout)++;
   }
   if( *pnout ) {
      if( (vout = make_SVEC(*pnout)) ) {
         *pnout = 0;
         for( i = 0; i < nin; i++ ) {
            if( mask[i] ) {
               vout[*pnout] = vin[i];
               (*pnout)++;
            }
         }
      } else {
         fprintf(stderr,"svreduce: make_SVEC(%d) failed\n",*pnout);
         *pnout = 0;
      }
   }
   return vout;
}
