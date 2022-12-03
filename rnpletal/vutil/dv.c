/* D O U B L E    P R E C I S I O N    V E C T O R     R O U T I N E S  */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <limits.h>
#include <time.h>
#include "v.h"

#define IL(_code) int j; for( j = 0; j < n; j++ ) { _code; }
#define IL_j(_jmin,_jmax,_code) int j; for( j = _jmin; j < _jmax; j++ )  \
                                { _code; }
#define ON  1
#define OFF 0

double dv_dmin(double d1,double d2) {
   return( d1 < d2 ? d1 : d2 );
}

double dv_dmax(double d1,double d2) {
   return( d1 > d2 ? d1 : d2 );
}

DVEC make_DVEC(r_int n) {
   return (DVEC) malloc(n * sizeof(double));
}

void free_DVEC(r_DVEC v) {
   if( v != (DVEC) NULL ) {
      free(v);
   }
}

/* make DVEC with "periodic" extension. */

DVEC make_DVEC_p(r_int n) {
   r_DVEC  v;

   v = (DVEC) malloc( (n + 2) * sizeof(double) );
   return v != NULL ? v + 1 :  v;
}

void free_DVEC_p(r_DVEC v) {
   if( v != (DVEC) NULL ) {
      free(v-1);
   }
}

DVECN make_DVECN(int n) {
   DVECN    the;
   
   the.v = make_DVEC(n);
   the.n = n;
   return(the);
}

void free_DVECN(DVECN the) {
   if( the.v ) free(the.v);
}

DIVEC make_DIVEC(r_int n) {
   return (DIVEC) malloc(n * sizeof(DI));
}

void  free_DIVEC(DIVEC v) {
   if( v ) free(v);
}
   

/* Vector minmum and maximum. */ 

double dvmin(r_DVEC v,r_int n) {
   r_double   temp;
   r_int      i;

   temp = v[0];
   for( i = 1; i < n; i++ ) {
      if( v[i] < temp )
         temp = v[i];
   }
   return(temp);
}

double dvmax(r_DVEC v,r_int n) {
   r_double   temp;
   r_int      i;

   temp = v[0];
   for( i = 1; i < n; i++ ) {
      if( v[i] > temp )
         temp = v[i];
   }
   return(temp);
}

/* Vector abs minmum and abs maximum. */ 

double dvabsmin(r_DVEC v,r_int n) {
   r_double   temp;
   r_int      i;

   temp = fabs(v[0]);
   for( i = 1; i < n; i++ ) {
      if( fabs(v[i]) < temp )
         temp = fabs(v[i]);
   }
   return(temp);
}

double dvabsmax(r_DVEC v,r_int n) {
   r_double   temp;
   r_int      i;

   temp = fabs(v[0]);
   for( i = 1; i < n; i++ ) {
      if( fabs(v[i]) > temp )
         temp = fabs(v[i]);
   }
   return(temp);
}

double dxymin_range(r_DVEC x,r_DVEC y,r_int n,r_double xmin,r_double xmax) {
   r_double   temp;
   r_int      i;

   temp = MAXDOUBLE;
   for( i = 0; i < n; i ++ ) {
      if( xmin <= x[i]  &&  x[i] <=  xmax ) {
         if( y[i] < temp ) temp = y[i];
      }
   }
   return temp;
}

double dxymax_range(r_DVEC x,r_DVEC y,r_int n,r_double xmin,r_double xmax) {
   r_double   temp;
   r_int      i;

   temp = -MAXDOUBLE;
   for( i = 0; i < n; i ++ ) {
      if( xmin <= x[i]  &&  x[i] <=  xmax ) {
         if( y[i] > temp ) temp = y[i];
      }
   }
   return temp;
}

void dvprint(DVEC v,int n,char *s) {
   int     i, per_line = 6;

   if( v != NULL ) {
      printf("<<< %s >>>\n\n",s);
      for( i = 0; i < n; i++ ) {
         printf( ((i + 1) % per_line ? "  %f" : "  %f\n"),v[i]);
      }
      printf( (n % per_line) ? "\n\n" : "\n" );
   }
}

void divprint(DIVEC v,int n,char *s) {
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

void dvprint_inc(DVEC v,int n,int inc,char *s) {
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

void dvprint_range(DVEC v,int n,char *s) {
   printf("dvprint_range:: %s: %15g %15g.\n",s,dvmin(v,n),dvmax(v,n));
}

DVEC Dviota(r_int n) {
   r_DVEC  v;
   r_int   i;

   if( (v = make_DVEC(n)) != NULL ) {
      for( i = 0; i < n; i++ )
         v[i] = (double) (i + 1);
   } else {
      printf("Dviota: malloc failure.\n");
   }
   return v;
}

DVEC DViota(r_DVEC v,r_int n) {
   r_int   i;

   if( v != NULL ) {
/*    printf("DViota: defining iota vector length %d.\n");  */
      for( i = 0; i < n; i++ )
         v[i] = (double) (i + 1);
   }
   return v;
}

DVEC Dvcopy(r_DVEC v1,r_int n) {
   r_DVEC  v2;
   r_int   i; 
   
   if( (v2 = make_DVEC(n)) != NULL ) {
      for( i = 0; i < n; i++ ) {
         v2[i] = v1[i];
      }
   } else {
      printf("Dvcopy: copy failed.\n");
   }
   return v2;
}

DVEC DVcopy(r_DVEC v1,r_DVEC v2,r_int n) {
   r_int   i; 
   
   if( (v1 != (DVEC) NULL) && (v2 != (DVEC) NULL) ) {
      for( i = 0; i < n; i++ ) {
         v2[i] = v1[i];
      }
   } 
   return v2;
}

void dvva(r_DVEC v1,r_DVEC v2,r_DVEC v3,r_int n) {
   r_int    i;

   for( i = 0; i < n; i++ ) {
     v3[i] = v2[i] + v1[i];
   }
}

void dvvm(r_DVEC v1,r_DVEC v2,r_DVEC v3,r_int n) {
   r_int    i;

   for( i = 0; i < n; i++ ) {
     v3[i] = v2[i] * v1[i];
   }
}

void dvvs(r_DVEC v1,r_DVEC v2,r_DVEC v3,r_int n) {
   r_int    i;

   for( i = 0; i < n; i++ ) {
     v3[i] = v1[i] - v2[i];
   }
}

void dvsm(r_DVEC v1,r_double s1,r_DVEC v2,int n) {
   r_int    i;
   
   for( i = 0; i < n; i++ ) {
      v2[i] = v1[i] * s1;
   }
}

void dvsa(r_DVEC v1,r_double s1,r_DVEC v2,int n) {
   r_int    i;

   for( i = 0; i < n; i++ ) {
      v2[i] = v1[i] + s1;
   }
}

DVEC DVva(r_DVEC v1,r_DVEC v2,r_DVEC v3,r_int n) {
   r_int    i;

   for( i = 0; i < n; i++ ) {
     v3[i] = v2[i] + v1[i];
   }
   return v3;
}

DVEC DVvm(r_DVEC v1,r_DVEC v2,r_DVEC v3,r_int n) {
   r_int    i;

   for( i = 0; i < n; i++ ) {
     v3[i] = v2[i] * v1[i];
   }
   return v3;
}

DVEC DVvs(r_DVEC v1,r_DVEC v2,r_DVEC v3,r_int n) {
   r_int    i;

   for( i = 0; i < n; i++ ) {
     v3[i] = v1[i] - v2[i];
   }
   return v3;
}

DVEC DVsm(r_DVEC v1,r_double s1,r_DVEC v2,int n) {
   r_int    i;
   
   for( i = 0; i < n; i++ ) {
      v2[i] *= s1;
   }
   return v2;
}

DVEC DVsa(r_DVEC v1,r_double s1,r_DVEC v2,int n) {
   r_int    i;

   for( i = 0; i < n; i++ ) {
      v2[i] += s1;
   }
   return v2;
}

#if !defined(INTEL_8_OR_LATER) 
DVEC DVrand(r_DVEC v,r_double v_min,r_double v_max,r_int n) {
   r_double  v_range;
   r_int     i;

   if( (v_range = (v_max - v_min)) <= 0.0 ) {
      printf("DVrand:: Invalid range. v_min: %g  v_max: %g.\n",
                v_min,v_max);
   }
   for( i = 0; i < n; i++ ) {
      v[i] = v_min + v_range * drand48();
   }
   return v;
}
#endif

int DVscanf(r_DVEC v,r_int n) {
   r_int      ln; 

   for( ln = 0; scanf("%lf",v + ln) == 1; ln++ );
   return ln;
}

int DVprintf(r_DVEC v,r_int n) {
   int        j;
   
   for( j = 0; j < n; printf("%g\n",v[j++]));
   return n; 
}

int DVVscanf(r_DVEC v1,r_DVEC v2,r_int n) {
   r_int       ln;

   for( ln = 0; scanf("%lf %lf",v1 + ln,v2 + ln) == 2; ln++ );
   return ln;
}

int DVVprintf(r_DVEC v1,r_DVEC v2,r_int n) {
   int        j;
   
   for( j = 0; j < n; j++ ) {
      printf("%e %e\n",v1[j],v2[j]);
   }
   return n; 
}

void dvls(r_DVEC v1,double s1,int n) {
   IL( v1[j] = s1 )
}

/* Only affects n-1 elements. */

void dvav(r_DVEC v1,r_DVEC v2,int n) {
   IL_j( 1, n-1, v2[j] = 0.5 * (v1[j] + v1[j+1]) )
}

/* Returns number of non--zero elements. */

int idvnz(DVEC v,int n) {
   int       s = 0;

   IL( if( (int) v[j] ) ++s; )
   return s;
}

void dvramp(DVEC v,double v0,double dv,int n) {
   int       j; 

   if( n > 0 ) {
      v[0] = v0;
      for( j = 1; j < n; j++ ) {
         v[j] = v[j-1] + dv;
      }
   }
}

DVEC Dvramp(double v0,double dv,int n) {
   DVEC     v;

   if( v = make_DVEC(n) ) {
      dvramp(v,v0,dv,n);
   } else {
      printf("Dvramp:: make_DVEC(%d) fails.\n",n);
   }
   return   v;
}

void dvumsh(DVEC v,int n,double v0,double vnm1) {
   if( n ) {
      if( n == 1 ) {
         v[0] = v0;
      } else {
         dvramp(v,v0,(vnm1 - v0) / (n - 1),n);
         v[n-1] = vnm1;
      }
   }
}

double dvsum(DVEC v,int n) {
   double    sum = 0.0;

   IL( sum += v[j] )
   return sum;
}

double dvvsum(DVEC v1,DVEC v2,int n) {
   double    sum = 0.0;

   IL( sum += v1[j] * v2[j] )
   return sum;
}

void dvdd01(DVEC v1,DVEC v2,double h,int n) {
   double    half_hm1;
   int       j;

   half_hm1 = 0.5 / h;
   for( j = 0; j < n; j++ ) {
      v2[j] = half_hm1 * (v1[j+1] - v1[j-1]); 
   }
}

void dvdump(DVEC v,int n,char *s) {
   int     i, per_line = 4;

   if( v != NULL ) {
      printf("<<< %s >>>\n\n",s);
      for( i = 0; i < n; i++ ) {
         printf( ((i + 1) % per_line ? "%19.10E" : "%19.10E\n"),v[i]);
      }
      printf( (n % per_line) ? "\n\n" : "\n" );
   }
}

void dvpdmp(DVEC v1,DVEC v2,int n,char *s) {
   int     i;

   if( (v1 != NULL) && (v2 != NULL) ) {
      printf("<<< %s >>>\n\n",s);
      for( i = 0; i < n; i++ ) {
         printf("%g %g\n",v1[i],v2[i]);
      }
   }
}

void dvfdump(FILE *fp,DVEC v,int n,char *s) {
   int     i, per_line = 4;

   if( v != NULL ) {
      fprintf(fp,"<<< %s >>>\n\n",s);
      for( i = 0; i < n; i++ ) {
         fprintf(fp, ((i + 1) % per_line ? "%19.10E" : "%19.10E\n"),v[i]);
      }
      fprintf(fp, (n % per_line) ? "\n\n" : "\n" );
   }
}

double drange(double xmin,double x,double xmax,double fuzz) {
   double     dx = fabs(fuzz * (xmax - xmin));

   if( xmin <= xmax ) {
      return (xmin - dx <= x) && (x <= xmax + dx);
   } else {
      return (xmax - dx <= x) && (x <= xmin + dx);
   }
}

/* Routines called by orthofit etc. to generate code. */

#define  MAX_F77_CONT_LINE  50
#define  MAX(a,b) ((a > b) ? a : b)
#define  MIN(a,b) ((a < b) ? a : b)

void generate_code(char *f_name,char *r_name,char *sub_name,
                   DVEC c,int nc,double xmin,double xmax) {
   FILE         *code_file;
   int           ic;
   time_t       *tloc = (time_t *) NULL;

   code_file = fopen(f_name,"w");
   fprintf(code_file,"c\n");
   (void) time(tloc);
   fprintf(code_file,"c     Generated by generate_code() ... %s",
                     asctime(localtime(tloc)));
   fprintf(code_file,"c\n");
   fprintf(code_file,"      subroutine %s(x,y,n)\n\n",r_name);
   fprintf(code_file,"         implicit        none\n\n");
   fprintf(code_file,"         integer         n\n");
   fprintf(code_file,"         real*8          x(n),         y(n)\n\n");
   fprintf(code_file,"         real*8          c(0:%d) /\n",nc);
   for( ic = 0; ic <= nc; ic++ ) {
      fprintf(code_file,ic == nc ? "     *       %30.16G\n" :
                                   "     *       %30.16G,\n",c[ic]);
   }
   fprintf(code_file,"     *                           /\n\n");
   fprintf(code_file,"         real*8          xmin\n");
   fprintf(code_file,"         parameter     ( xmin = %30.16G )\n",xmin);
   fprintf(code_file,"         real*8          xmax\n");
   fprintf(code_file,"         parameter     ( xmax = %30.16G )\n\n",xmax);
   fprintf(code_file,"         call %s(x,y,n,xmin,xmax,c,%d)\n\n",sub_name,nc);
   fprintf(code_file,"         return\n\n");
   fprintf(code_file,"      end\n");
   fclose(code_file);
}

void generate_c_data(FILE *code_file,DVEC c,int nc,int nseg) {
   int           i,               j;
   fprintf(code_file,"         real*8          c(0:%d,%d) \n",nc,nseg);
   for( i = 1; i <= nseg; i++, c += (nc + 1) ) {
      fprintf(code_file,"         data            (c(i,%d) , i = 0 , %d) /\n",
              i, nc);
      for( j = 0; j <= nc; j++ ) {
         fprintf(code_file,j == nc ? "     *       %30.16G\n" :
                                     "     *       %30.16G,\n",c[j]);
      }
      fprintf(code_file,"     *                           /\n\n");
   }
}

/* This version splits up statement if necessary ... */

void generate_xseg_data(FILE *code_file,DVEC xseg,int nseg) {
   int           fi_st,  fi_fin,  fi_last = nseg + 1,
                 fi_nmax = MAX_F77_CONT_LINE;
   fprintf(code_file,"         real*8          xseg(1:%d) \n",nseg+1);
   for( fi_st = 1; fi_st <= fi_last; fi_st = fi_fin + 1 ) {
      fi_fin = MIN(fi_st+fi_nmax-1,fi_last);
      generate_xseg_data_1(code_file,xseg,fi_st,fi_fin);
      xseg += (fi_fin - fi_st + 1);
   }
   fprintf(code_file,"\n");
}

void generate_xseg_data_1(FILE *code_file,DVEC xseg,int fi_st,int fi_fin) {
   int           i;

   fprintf(code_file,"         data            ");
   fprintf(code_file,"( xseg(i) , i = %d , %d ) /\n",fi_st,fi_fin);
   for( i = fi_st; i <= fi_fin; i++, xseg++ ) {
      fprintf(code_file,i == fi_fin     ? "     *       %30.16G\n" :
                                          "     *       %30.16G,\n",*xseg);
   }
   fprintf(code_file,"     *                        /\n");
}

void old_generate_xseg_data(FILE *code_file,DVEC xseg,int nseg) {
   int           i;
   fprintf(code_file,"         real*8          xseg(1:%d) \n",nseg+1);
   fprintf(code_file,"         data            xseg /\n");
   for( i = 1; i <= (nseg + 1); i++, ++xseg ) {
      fprintf(code_file,i == (nseg + 1) ? "     *       %30.16G\n" :
                                          "     *       %30.16G,\n",*xseg);
   }
   fprintf(code_file,"     *                        /\n\n");
}

/* Caveat ... DATA statements interspersed with declarations. */

void generate_multi_segment_code(char *f_name,char *r_name,char *sub_name,
                                 DVEC c,int nc,DVEC xseg,int  nseg) {
   FILE         *code_file;
   int           ic;
   time_t       *tloc;

   code_file = fopen(f_name,"w");
   fprintf(code_file,"c\n");
/* (void) time(tloc);
   fprintf(code_file,"c     Generated by generate_multi_segment_code() ... %s",
                     asctime(localtime(tloc))); 
   fprintf(code_file,"c\n"); */
   fprintf(code_file,"      subroutine %s(x,y,n)\n\n",r_name);
   fprintf(code_file,"         implicit        none\n\n");
   fprintf(code_file,"         integer         n\n");
   fprintf(code_file,"         real*8          x(n),         y(n)\n");
   fprintf(code_file,"         integer         i\n\n");
   generate_c_data(code_file,c,nc,nseg);
   generate_xseg_data(code_file,xseg,nseg);
   fprintf(code_file,"         call %s(x,y,n,xseg,%d,c,%d,%d)\n\n",

                     sub_name,nseg,nc,nc);
   fprintf(code_file,"         return\n\n");
   fprintf(code_file,"      end\n");
   fclose(code_file);
}

void generate_multi_segment_code_x(char *f_name,char *r_name,char *sub_name,
                                   DVEC c,int nc,DVEC xseg,int nseg) {
   FILE         *code_file;
   int           ic;
   time_t       *tloc;

   code_file = fopen(f_name,"w");
   fprintf(code_file,"c\n");
/* (void) time(tloc);
   fprintf(code_file,"c     Generated by generate_multi_segment_code() ... %s",
                     asctime(localtime(tloc))); 
   fprintf(code_file,"c\n"); */
   fprintf(code_file,"      subroutine %s(x,y,n)\n\n",r_name);
   fprintf(code_file,"         implicit        none\n\n");
   fprintf(code_file,"         integer         n\n");
   fprintf(code_file,"         real*8          x(n),         y(n)\n\n");
   generate_c_data(code_file,c,nc,nseg);
   generate_xseg_data(code_file,xseg,nseg);
   fprintf(code_file,"         call %s(x,y,n,xseg,%d,c,%d,%d)\n",

                     sub_name,nseg,nc,nc);
   fprintf(code_file,"         call dvvdxx(y,x,y,n)\n");
   fprintf(code_file,"         return\n\n");
   fprintf(code_file,"      end\n");
   fclose(code_file);
}

void generate_multi_segment_code_dn(char *f_name, char *r_name, char *sub_name,
                                    DVEC c, int nc,DVEC xseg, int nseg) {
   FILE         *code_file;
   int           ic;
   time_t       *tloc;

   code_file = fopen(f_name,"w");
   fprintf(code_file,"c\n");
/* (void) time(tloc);
   fprintf(code_file,"c     Generated by generate_multi_segment_code() ... %s",
                     asctime(localtime(tloc))); 
   fprintf(code_file,"c\n"); */
   fprintf(code_file,"      subroutine %s(x,y,n,d_ord)\n\n",r_name);
   fprintf(code_file,"         implicit        none\n\n");
   fprintf(code_file,"         integer         n\n");
   fprintf(code_file,"         real*8          x(n),         y(n)\n");
   fprintf(code_file,"         integer         d_ord\n");                
   fprintf(code_file,"         integer         i\n\n");
   generate_dc_and_c_data(code_file,c,nc,nseg);
   generate_xseg_data(code_file,xseg,nseg);
   fprintf(code_file,"         call dvmchbdnc(c,%d,%d,dc,ndc,xseg,%d,d_ord)\n",
                     nc,nc,nseg);
   fprintf(code_file,"         call %s(x,y,n,xseg,%d,dc,%d,ndc)\n\n",
                     sub_name,nseg,nc);
   fprintf(code_file,"         return\n\n");
   fprintf(code_file,"      end\n");
   fclose(code_file);
}

void generate_dc_and_c_data(FILE *code_file, DVEC c, int nc, int nseg) {
   int           i,               j;
   fprintf(code_file,"         integer         ndc\n");
   fprintf(code_file,"         real*8         dc(0:%d,%d) \n",nc,nseg);
   fprintf(code_file,"         real*8          c(0:%d,%d) \n",nc,nseg);
   for( i = 1; i <= nseg; i++, c += (nc + 1) ) {
      fprintf(code_file,"         data            (c(i,%d) , i = 0 , %d) /\n",
              i, nc);
      for( j = 0; j <= nc; j++ ) {
         fprintf(code_file,j == nc ? "     *       %30.16G\n" :
                                     "     *       %30.16G,\n",c[j]);
      }
      fprintf(code_file,"     *                           /\n\n");
   }
}

void dvfout(FILE *fout,DVEC v,int n) {
   int      i;

   for( i = 0; i < n; i++ ) fprintf(fout,"%24.16E\n",v[i]);
}

/* Sorting routines use qsort() ... */

void dvsortup(DVEC v, int n) {
   (void) qsort((char *) v,n,sizeof(double),(COMPAR) sort_up);
}

void dvsortdown(DVEC v, int n) {
   (void) qsort((char *) v,n,sizeof(double),(COMPAR) sort_down);
}

int sort_up(double *a, double *b) {
   return (*a < *b) ? -1 : ((*a > *b) ? 1 : 0);
}

int sort_down(double *a,double *b) {
   return (*a > *b) ? -1 : ((*a < *b) ? 1 : 0);
}

void dvsortupuniq_(DVEC vin,int *pnin,DVEC vout,int *pnout,double *pfuzz) {
   dvsortupuniq(vin,*pnin,vout,pnout,*pfuzz);
}

void dvsortupuniq(DVEC vin, int nin, DVEC vout,int *pnout,double fuzz) {
   DVEC      vt;
   int       i, iout, nout;

   vt = Dvcopy(vin,nin);
   (void) dvsortup(vt,nin);
   for( nout = 1, i = 1; i < nin; i++ ) {
      if( !eq_fuzz(vt[i],vt[i-1],fuzz) ) nout++;
   }
   vout[0] = vt[0];
   for( iout = 1, i = 1; i < nin; i++ ) {
      if( !eq_fuzz(vt[i],vt[i-1],fuzz) ) {
         vout[iout++] = vt[i];
      }
   }
   *pnout = nout;
   free_DVEC(vt);
}

void dvsortdownuniq(DVEC vin, int nin, DVEC vout,int *pnout,double fuzz) {
   DVEC      vt;
   int       i, iout, nout;

   vt = Dvcopy(vin,nin);
   (void) dvsortdown(vt,nin);
   for( nout = 1, i = 1; i < nin; i++ ) {
      if( !eq_fuzz(vt[i],vt[i-1],fuzz) ) nout++;
   }
   vout[0] = vt[0];
   for( iout = 1, i = 1; i < nin; i++ ) {
      if( !eq_fuzz(vt[i],vt[i-1],fuzz) ) {
         vout[iout++] = vt[i];
      }
   }
   *pnout = nout;
   free_DVEC(vt);
}

int dvlookup_linear(DVEC v,int n,double vkey,double fuzz) {
   int    j;
   for( j = 0; j < n; j++ ) {
      if( eq_fuzz(v[j],vkey,fuzz) ) return j;
   }
   return -1;
}

int eq_fuzz(double x1,double x2,double fuzz) {

   if(        x1 == 0.0 ) {
      return fabs(x2) <= fuzz;
   } else if( x2 == 0.0 ) {
      return fabs(x1) <= fuzz;
   } else {
      return (fabs(x1 - x2) / fabs(x1)) <= fuzz;
   }

}

int dvlsindex(DVEC v,int n,double key,double fuzz) {
   int      i;
   
   for( i = 0; i < n; i++ ) {
      if( eq_fuzz(v[i],key,fuzz) ) return i;
   }
   return -1;
}

IVEC Dvlsindex(DVEC v,int n,DVEC key,int nkey,double fuzz) {
   IVEC     I;
   int      i;
   
   I = make_IVEC_dv(nkey);
   for( i = 0; i < nkey; i++ ) {
      I[i] = dvlsindex(v,n,key[i],fuzz);
   }
   return I;
}

IVEC make_IVEC_dv(r_int n) {
   return (IVEC) malloc(n * sizeof(int));
}

void dvvmergeup(DVEC v1,int n1,DVEC v2,int n2,DVEC v3,int *pn3,double fuzz) {
   DVEC     vt1,    vt2;

   vt1 = make_DVEC(n1 + n2);
   (void) DVcopy(v1,vt1,n1);
   (void) DVcopy(v2,vt1+n1,n2);
   (void) dvsortupuniq(vt1,n1+n2,v3,pn3,fuzz);
   free_DVEC(vt1);
}

void dvvmergedown(DVEC v1,int n1,DVEC v2,int n2,DVEC v3,int *pn3,double fuzz) {
   DVEC     vt1,    vt2;

   vt1 = make_DVEC(n1 + n2);
   (void) DVcopy(v1,vt1,n1);
   (void) DVcopy(v2,vt1+n1,n2);
   (void) dvsortdownuniq(vt1,n1+n2,v3,pn3,fuzz);
   free_DVEC(vt1);
}

void dvmrg(DVEC v1,DVEC v2,DVEC vmrg,int n) {
   int j;

   for( j = 0; j < n; j++ ) {
      vmrg[2*j] = v1[j];
      vmrg[2*j+1] = v2[j];
   }
}

void dvumrg(DVEC vmrg,DVEC v1,DVEC v2,int n) {
   int j;

   for( j = 0; j < n; j++ ) {
      v1[j] = vmrg[2*j];
      v2[j] = vmrg[2*j+1];
   }
}

int dv2sort(DVEC v1,DVEC v2,int n,int code) {
   DVEC     vmrg;
   
   switch( code ) {
   case SORT_UP:
   case SORT_DOWN:
      break;
   default:
      fprintf(stderr,"dv2sort: Invalid code: %d\n",code);
      return -1;
   }
   if( !(vmrg = make_DVEC(2 * n)) ) {
      fprintf(stderr,"dv2sort: make_DVEC(%d) failed.\n",2 * n);
      return -1;
   }
   dvmrg(v1,v2,vmrg,n);
   switch( code ) {
   case SORT_UP:
      qsort(vmrg,n,sizeof(D2),(COMPAR) sort_up_d2);
      break;
   case SORT_DOWN:
      qsort(vmrg,n,sizeof(D2),(COMPAR) sort_down_d2);
      break;
   default:
      break;
   }
   dvumrg(vmrg,v1,v2,n);
   free_DVEC(vmrg);

   return 0;
}

int sort_up_d2(PD2 el1,PD2 el2) {
   return (el1->x1 < el2->x1) ? -1 : ((el1->x1 > el2->x1) ? 1 : 0);
}

int sort_down_d2(PD2 el1,PD2 el2) {
   return (el1->x1 > el2->x1) ? -1 : ((el1->x1 < el2->x1) ? 1 : 0);
}

DVEC dvdel_el(DVEC v,int n,int i) {
   DVEC    new_v;
   int     j,    new_j;

   if( 0 <= i && i < n ) {
      if( n == 1 ) {
         new_v = NULL;
      } else {
         new_v = make_DVEC(n-1);
         for( j = 0, new_j = 0; j < n; j++ ) {
            if( j != i ) new_v[new_j++] = v[j];
         }
      }
      free_DVEC(v);
      return new_v;
   } else {
      fprintf(stderr,"dvdel_el: Element %d out of range 0 .. %d\n",i,n);
      return v;
   }
}

int dv2insert(DVEC *pv1,DVEC *pv2,int n,double new1,double new2,int code) {
   DVEC     new_v1,      new_v2;
   int      new_n;

   new_n = n + 1;
   if( !(new_v1 = make_DVEC(new_n)) || !(new_v2 = make_DVEC(new_n)) ) {
      fprintf(stderr,"dv2insert: make_DVEC(%d) failed.\n",new_n);
      return n;
   } else {
      new_v1 = DVcopy(*pv1,new_v1,n); new_v1[n] = new1;
      new_v2 = DVcopy(*pv2,new_v2,n); new_v2[n] = new2;
      if( !dv2sort(new_v1,new_v2,new_n,code) ) {
         free_DVEC(*pv1); *pv1 = new_v1;
         free_DVEC(*pv2); *pv2 = new_v2;
         return new_n;
      } else {
         fprintf(stderr,"dv2insert: dv2sort failed.\n");
         free_DVEC(new_v1);
         free_DVEC(new_v2);
         return n;
      }
   }
}

int dv2insert_nosort(DVEC *pv1,DVEC *pv2,int n,double new1,double new2,int i) {
   DVEC     new_v1,      new_v2;
   int      new_n = n;
   int      o;

   if( i < 0  || i > n ) {
      fprintf(stderr,"dv2insert_nosort: i (%d) out of range (0--%d).\n",i,n);
   } else {
      if( !(new_v1 = make_DVEC(n + 1)) || !(new_v2 = make_DVEC(n + 1)) ) {
         fprintf(stderr,"dv2insert_nosort: make_DVEC(%d) failed.\n",n + 1);
      } else {
         DVcopy(*pv1,new_v1,i);
         DVcopy(*pv2,new_v2,i);
         new_v1[i] = new1;
         new_v2[i] = new2;
         DVcopy(*pv1+i,new_v1+i+1,n-i);
         DVcopy(*pv2+i,new_v2+i+1,n-i);
         free_DVEC(*pv1); *pv1 = new_v1;
         free_DVEC(*pv2); *pv2 = new_v2;
         new_n = n + 1;
      }
   }
   return(new_n);
}

#define   NEWEXT(a)     iext = a; vext = v[a];
#define   NEWABSEXT(a)  iext = a; vext = fabs(v[a]);
int ixdvmin(DVEC v,int n) {
   double   vext;
   int      i,    iext;

   if( n < 1 ) return -1;

   NEWEXT(0);
   for( i = 1 ; i < n; i++ ) {
      if( v[i] < vext ) { NEWEXT(i) }
   }
   return iext;
}

int ixdvmax(DVEC v,int n) {
   double   vext;
   int      i,    iext;

   if( n < 1 ) return -1;

   NEWEXT(0);
   for( i = 1 ; i < n; i++ ) {
      if( v[i] > vext ) { NEWEXT(i) }
   }
   return iext;
}

int ixdvabsmin(DVEC v,int n) {
   double   vext;
   int      i,    iext;

   if( n < 1 ) return -1;

   NEWABSEXT(0);
   for( i = 1 ; i < n; i++ ) {
      if( fabs(v[i]) < vext ) { NEWABSEXT(i) }
   }
   return iext;
}

int ixdvabsmax(DVEC v,int n) {
   double   vext;
   int      i,    iext;

   if( n < 1 ) return -1;

   NEWABSEXT(0);
   for( i = 1 ; i < n; i++ ) {
      if( fabs(v[i]) > vext ) { NEWABSEXT(i) }
   }
   return iext;
}

#undef    NEWABSEXT
#undef    NEWEXT

int ixdvnearest(DVEC v,int n,double key) {
   DVEC   tv;
   int    i;
   
   if( n < 1 ) return -1;

   if( !(tv = make_DVEC(n)) ) {
      fprintf(stderr,"ixdvnearest: make_DVEC(%d) failed.\n",n);
      return -1;
   }
   dvsa(v,-key,tv,n);
   i = ixdvabsmin(tv,n);
   free_DVEC(tv);
   return i;
}

int ixdv2nearest(DVEC v1,DVEC v2,int n,double key1,double key2,double asp1by2) {
   DVEC   tv1,    tv2,     tv3;
   int    i;
   
   if( n < 1 ) return -1;

   if( !(tv1 = make_DVEC(n)) || !(tv2 = make_DVEC(n)) || 
       !(tv3 = make_DVEC(n)) ) {
      fprintf(stderr,"ixdvnearest: make_DVEC(%d) failed.\n",n);
      return -1;
   }
   dvsa(v1,-key1,tv1,n);
   dvsa(v2,-key2,tv2,n);
   dvsm(tv2,asp1by2,tv2,n);
   
/* dvpyth_(tv1,tv2,tv3,&n);  */
   dvpyth(tv1,tv2,tv3,n);    
   i = ixdvabsmin(tv3,n);
   free_DVEC(tv1); free_DVEC(tv2); free_DVEC(tv3);
   return i;
}


double dvhash(DVEC v,int n) {
   double val = 0;  
   int    j;

   for( j = 0; j < n; j++ ) {
      val += (v[j] + (double) (j+1)) * (v[j] + (double) (j+1));
   }
   val = sqrt(val / n);
   return val;
}

void dvfapl(DVEC v1,DVEC v2,PFD f,int n) {
   int     i;

   for( i = 0; i < n; i++ ) {
      v2[i] = (*f)(v1[i]);
   }
}

DVEC Dvfapl(DVEC v1,PFD f,int n) {
   DVEC    v2 = make_DVEC(n);
   int     i;

   if( v2 ) {
      for( i = 0; i < n; i++ ) {
         v2[i] = (*f)(v1[i]);
      }
   }
   return(v2);
}

DVEC dvfscanf(FILE *fp,DVEC v,int *pn) {
   int  i = 0;
   if( fp && v ) {
      while( fscanf(fp,"%lf",v+i) == 1 ) i++;
   }
   *pn = i;
   if( fp ) fclose(fp);
   return(v);
}

void dvfprintf(FILE *fp,DVEC v,int n) { 
   int   i = 0;
   if( fp && v ) {
      for( i = 0; i < n; i++ ) {
         fprintf(fp,"%23.16e\n",v[i]);
      }
   }
}

int dvget(char *fname,DVEC v,int *pn) {
   FILE   *fp;
   if( fp = fopen(fname,"r") ) {
      v = dvfscanf(fp,v,pn);
      return(*pn);
   } else {
      return(0);
   }
}

DVEC Dvget(char *fname,int *pn) {
   DVEC    v = (DVEC) NULL;
   FILE   *fp;

   if( fp = fopen(fname,"r") ) {
      v = Dvfscanf(fp,pn);
   }  else {
      *pn = 0;
   }
   return(v);
}

int dvput(char *fname,DVEC v,int n) {
   FILE   *fp;
   if( fp = fopen(fname,"w") ) {
      dvfprintf(fp,v,n);
      fclose(fp);
   }
   return(n);
}

void dvvfscanf(FILE *fp,DVEC v1,DVEC v2,int *pn) {
   int  i = 0;
   if( fp && v1 && v2 ) {
      while( fscanf(fp,"%lf %lf",v1+i,v2+i) == 2 ) i++;
   }
   *pn = i;
   if( fp ) fclose(fp);
}

int dvvget(char *fname,DVEC v1,DVEC v2,int *pn) {
   FILE    *fp;
   if( fp = fopen(fname,"r") ) {
      dvvfscanf(fp,v1,v2,pn);
      return(*pn);
   } else {
      *pn = 0;
      return(0);
   }
}

int dvvput(char *fname,DVEC v1,DVEC v2,int n) {
   FILE    *fp;
   if( fp = fopen(fname,"w") ) {
      dvvfprintf(fp,v1,v2,n);
      fclose(fp);
      return(n);
   } else {
      return(0);
   }
}

void dvvfprintf(FILE *fp,DVEC v1,DVEC v2,int n) { 
   int   i = 0;
   if( fp && v1 && v2 ) {
      for( i = 0; i < n; i++ ) {
         fprintf(fp,"%23.16e %23.16e\n",v1[i],v2[i]);
      }
   }
}

DVEC Dvfscanf(FILE *fp,int *pn) {
   fpos_t   pos;
   DVEC     v;
   double   vi;
   int      i = 0;

   if( fp ) {
      (void) fgetpos(fp,&pos);
      while( fscanf(fp,"%lf",&vi) == 1 ) i++;
      if( i > 0 ) {
         (void) fsetpos(fp,&pos);
         v = make_DVEC(i);
         i = 0;
         while( fscanf(fp,"%lf",v+i) == 1 ) i++;
      }
   }
   *pn = i;
   if( fp ) fclose(fp);
   return(v);
}

PDVEC Dvvfscanf(FILE *fp,int *pn) {
   PDVEC    p;
   fpos_t   pos;
   double   v1i,    v2i;
   int      i = 0;

   p.v1 = (DVEC) NULL;
   p.v2 = (DVEC) NULL;
   if( fp ) {
      (void) fgetpos(fp,&pos);
      while( fscanf(fp,"%lf %lf",&v1i,&v2i) == 2 ) i++;
      if( i > 0 ) {
         (void) fsetpos(fp,&pos);
         p.v1 = make_DVEC(i);
         p.v2 = make_DVEC(i);
         i = 0;
         while( fscanf(fp,"%lf %lf",p.v1+i,p.v2+i) == 2 ) i++;
      }
   }
   *pn = i;
   if( fp ) fclose(fp);
   return(p);
}

PDVEC Dvvget(char *fname,int *pn) {
   PDVEC   p;
   FILE   *fp;

   p.v1 = (DVEC) NULL;
   p.v2 = (DVEC) NULL;
   if( fp = fopen(fname,"r") ) {
      p = Dvvfscanf(fp,pn);
      if( fp ) fclose(fp);
   } else {
      *pn = 0;
   }
   return(p);
}

void dvvvfscanf(FILE *fp,DVEC v1,DVEC v2,DVEC v3,int *pn) {
   int  i = 0;
   if( fp && v1 && v2 && v3 ) {
      while( fscanf(fp,"%lf %lf %lf",v1+i,v2+i,v3+i) == 3 ) i++;
   }
   *pn = i;
   if( fp ) fclose(fp);
}

PDVECN PDVECNget(char *fname) {
   PDVECN  pp;
   PDVEC   p;
   int     pn;

   p = Dvvget(fname,&pn);
   pp.v1 = p.v1;
   pp.v2 = p.v2;
   pp.n = pn;
   return(pp);
}

PDVECN free_and_null_PDVECN(PDVECN p) {
   if( p.v1 ) free(p.v1);
   p.v1 = (DVEC) NULL;
   if( p.v2 ) free(p.v2);
   p.v2 = (DVEC) NULL;
   p.n = 0;
   return(p);
}

void PDVECNfdump(FILE *fp,PDVECN the,char *s) {
   char    buffer[1024];
   if( fp  && the.n ) {
      sprintf(buffer,"%s:v1",s);
      dvfdump(fp,the.v1,the.n,buffer);
      sprintf(buffer,"%s:v2",s);
      dvfdump(fp,the.v2,the.n,buffer);
   }
}

void dvpyth(DVEC v1,DVEC v2,DVEC v3,int n) {
   dvvm(v1,v1,v3,n);
   dvavvm(v3,v2,v2,v3,n);
   dvfapl(v3,v3,sqrt,n);
}

void dvavvm(DVEC v1,DVEC v2,DVEC v3,DVEC v4,int n) {
   int    i;

   for( i = 0; i < n; i++ ) {
      v4[i] = v1[i] + v2[i] * v3[i];
   }
}

/* Routine should not be called with non-malloced DVEC! ... */

DVEC dvappend(DVEC v,int n,double val) {
   DVEC  newv;
   if( !v ) {
      (newv = make_DVEC(1))[0] = val;
   } else {
      (newv = DVcopy(v,make_DVEC(n + 1),n))[n] = val;
      free(v);
   }
   return(newv);
}

DVECN dvecn_append(DVECN the,double val) {
   the.v = dvappend(the.v,the.n,val);
   ++the.n;
   return(the);
}

int dv_between(double val1,double val,double val2) {
   return(val >= dv_dmin(val1,val2) && val <= dv_dmax(val1,val2));
}

DVEC Dv2perp(DVEC vx,DVEC vy,int n,double x,double y,double aspect) {
   DVEC     vperp = NULL;
   double   a,    b,   c,   th0,   th1;
   int      i;

   int      ltrace = OFF;

   if( n > 0 ) {
      if( vperp = make_DVEC(n-1) ) {
         for( i = 0; i < n - 1; i++ ) {
            a = hypot(vx[i+1] - vx[i],  aspect * (vy[i+1] - vy[i]));
            b = hypot( x      - vx[i],  aspect * ( y      - vy[i]));
            c = hypot( x      - vx[i+1],aspect * ( y      - vy[i+1]));
            if( a == 0.0  ||  b == 0.0 || c == 0.0 ) {
               vperp[i] = 0.0;
            } else {
               th0 = acos((a * a + b * b - c * c) / (2.0 * a * b));
               th1 = acos((a * a + c * c - b * b) / (2.0 * a * c));
               if( abs(th0) <= 0.25 * M_PI && abs(th1) <= 0.25 * M_PI ) {
                  vperp[i] = fabs(b * sin(th0));
               } else {
                  vperp[i] = dv_dmin(b,c);
               }
            }
         }
         if( ltrace ) {
            fprintf(stderr,"Dv2perp: x: %g  y: %g  aspect: %g.\n",
                    x,y,aspect);
            dvdump(vx   ,n,  "vx");
            dvdump(vy   ,n,  "vy");
            dvdump(vperp,n-1,"vperp");
         }
      } else {
         fprintf(stderr,"Dv2perp: make_DVEC(%d) failed.\n",n-1);
      }
   }
   return(vperp);
}

int ixdv2perp(DVEC vx,DVEC vy,int n,double x,double y,double aspect) {
   DVEC     vperp = NULL;
   int      i = 0;

   if( n > 0 ) {
      if( vperp = Dv2perp(vx,vy,n,x,y,aspect) ) {
         i = ixdvmin(vperp,n-1);
         free_DVEC(vperp);
      }
   }
   return(i);
}

SVEC DVSVcopy(DVEC vin,SVEC vout,int n) {
   int    i;

   for( i = 0; i < n; i++ ) {
      vout[i] = vin[i];
   }

   return( vout );
}

/* Interpolates y(x) data defined in 'from' to x coordinates of 'to' and 
   returns new PDVECN ... */

PDVECN PDVECN_interp(PDVECN from,PDVECN to,double vs,double vf,int intord) {
   PDVECN    new = {NULL, NULL, 0};
   if( from.n  && to.n ) {
      new.n  = to.n;
      new.v1 = Dvcopy(to.v1,to.n);
      new.v2 = Dvcopy(to.v2,to.n);
      dvinqn(from.v2,from.v1,new.v2,new.v1,from.n,new.n,vs,vf,intord);
   }
   return( new );
}

/* Vector linear interpolation based on segment length control ... */

#define   NIN   in.n
#define   XIN   in.v1
#define   YIN   in.v2
#define   NOUT  out.n
#define   XOUT  out.v1
#define   YOUT  out.v2

PDVECN PDVECN_lintseg(PDVECN in,double xscale,double yscale,
                      double mindseg,double maxdseg ) {
   PDVECN    out = {NULL, NULL, 0};

   double    dx,       dy,       dseg,      ddx,      ddy;
   int       i,        ip,       iout,      iinterp, 
             ninterp,  nadd,     ndel;
   int       thin_enabled;

   int       ltrace = OFF;

   if( ltrace ) {
      fprintf(stderr,"PDVECN_lintseg: in.n: %d\n",in.n);
      fprintf(stderr,"PDVECN_lintseg: xscale: %g  yscale: %g",xscale,yscale);
      fprintf(stderr,"  mindseg: %g  maxdseg: %g\n",mindseg,maxdseg);
   }

   if( xscale <= 0.0  || yscale <= 0.0  || maxdseg <= 0.0 ||
       maxdseg < mindseg ) {
      fprintf(stderr,"PDVECN_lintseg: Bad scalar arguments.\n");
      fprintf(stderr,"PDVECN_lintseg: xscale: %g  yscale: %g",xscale,yscale);
      fprintf(stderr,"  mindseg: %g  maxdseg: %g\n",mindseg,maxdseg);
      goto Return;
   }
   if( NIN <= 0 ) goto Return;
   if( NIN == 1 ) {
      NOUT = NIN;
      XOUT = Dvcopy(XIN,NIN);
      YOUT = Dvcopy(YIN,NIN);
      goto Return;
   }
   thin_enabled = mindseg > 0.0;
   if( ltrace ) {
      fprintf(stderr,"PDVECN_lintseg: Thinning %s enabled.\n",
              thin_enabled ? "is" : "is not" );
   }

/* First pass ... Determine required storage ... */

   NOUT = 0;
   i = 0;  nadd = 0;  ndel = 0;
   ip = i + 1;
   while( i < (NIN - 1)  &&  ip < NIN ) {
      dx = (XIN[ip] - XIN[i]) / xscale;
      dy = (YIN[ip] - YIN[i]) / yscale;
      dseg = sqrt(dx * dx + dy * dy); 
      if(        dseg > maxdseg ) {
         ninterp = (int) (dseg / maxdseg);
         if( ninterp * maxdseg == dseg ) ninterp--;
         NOUT = NOUT + (ninterp + 1);
         nadd += ninterp;
         i = ip; 
         ip = i + 1;
      } else if( dseg < mindseg ) {
         if( thin_enabled ) {
            ndel++;
            ip = ip + 1;
         } else {
            NOUT++;
            i = ip;
            ip = i + 1;
         }
      } else {
         NOUT++;
         i = ip;
         ip = ip + 1;
      }
   }
   NOUT++;
   if( ltrace ) {
      fprintf(stderr,"PDVECN_lintseg: n.in: %d  n.out: %d\n",NIN,NOUT);
      fprintf(stderr,"PDVECN_lintseg: %d added  %d deleted\n",nadd,ndel);
   }

/* Second pass ... Construct interpolated data ... */

   XOUT = make_DVEC(NOUT);
   YOUT = make_DVEC(NOUT);
#define  INC  iout++; if( iout == NOUT) { fprintf(stderr,"PDVECN_lintseg: Construction overflow! i: %d\n",i); goto Return;}
   i = 0;
   ip = i + 1;
   iout = 0;
   while( i < (NIN - 1)  &&  ip < NIN ) {
      dx = (XIN[ip] - XIN[i]) / xscale;
      dy = (YIN[ip] - YIN[i]) / yscale;
      dseg = sqrt(dx * dx + dy * dy); 
      if(        dseg > maxdseg ) {
         ninterp = (int) (dseg / maxdseg);
         if( ninterp * maxdseg == dseg ) ninterp--;
         ddx = (XIN[ip] - XIN[i]) / (ninterp + 1); 
         ddy = (YIN[ip] - YIN[i]) / (ninterp + 1);
         for( iinterp = 0; iinterp <= ninterp; iinterp++ ) {
            XOUT[iout] = XIN[i] + iinterp * ddx;
            YOUT[iout] = YIN[i] + iinterp * ddy;
            INC
         }
         i = ip; 
         ip = i + 1;
      } else if( dseg < mindseg ) {
         if( thin_enabled ) {
            ip = ip + 1;
         } else {
            XOUT[iout] = XIN[i];
            YOUT[iout] = YIN[i];
            INC
            i = ip;
            ip = i + 1;
         }
      } else {
         XOUT[iout] = XIN[i];
         YOUT[iout] = YIN[i];
         INC
         i = ip;
         ip = ip + 1;
      }
   }
/* assert(iout == (NOUT-1));  */
   XOUT[NOUT-1] = XIN[NIN-1];
   YOUT[NOUT-1] = YIN[NIN-1];

#undef   INC
Return:
   return(out);

}
#undef    NIN
#undef    XIN
#undef    YIN
#undef    NOUT
#undef    XOUT
#undef    YOUT

IVEC ivmakemaskdv(DVEC vin,int nin,PFI_D logic) {
   IVEC      vout = (IVEC) NULL;
   int       i;

   if( nin > 0 ) {
      if( (vout = make_IVEC_dv(nin)) ) {
         for( i = 0; i < nin; i++ ) {
            vout[i] = (*logic)(vin[i]);
         }
      } else {
         fprintf(stderr,"ivmakemaskdv: make_IVEC_dv(%d) failed\n",nin);
      }
   }
   return  vout;
}

DVEC dvreduce(DVEC vin,IVEC mask,int nin,int *pnout) {
   DVEC      vout = (DVEC) NULL;
   int       i;

   *pnout = 0;
   for( i = 0; i < nin; i++ ) {
      if( mask[i] ) (*pnout)++;
   }
   if( *pnout ) {
      if( (vout = make_DVEC(*pnout)) ) {
         *pnout = 0;
         for( i = 0; i < nin; i++ ) {
            if( mask[i] ) {
               vout[*pnout] = vin[i];
               (*pnout)++;
            }
         }
      } else {
         fprintf(stderr,"dvreduce: make_DVEC(%d) failed\n",*pnout);
         *pnout = 0;
      }
   }
   return vout;
}

int pdvramp(DVEC v,int n,double fuzz) {
   int     i;
   double  dv,     fdv;

   if( n > 2 ) {
      dv = v[1] - v[0];
      fdv = fabs(fuzz * dv);
      for( i = 2; i < n; i++ ) {
         if( fabs(dv - (v[i] - v[i-1])) > fdv ) return 0;
      }
   }
   return 1;
}

int isdvramp(DVEC v,int n,double fuzz) {
   double    dv0,     dv;
   int       i;

   if( n > 2 ) {
      dv0 = v[1] - v[0];
      for( i = 2; i < n; i++ ) {
         dv = v[i] - v[i-1];
         if( dv0 == 0.0 ) {
            if( fabs(dv) > fuzz ) return 0;
         } else {
            if( (dv * dv0 ) < 0 || fabs(1.0 - (dv / dv0)) > fuzz ) return 0;
         }
      }
   }
   return 1;
}

int limin(int i1,int i2) {
   return (i1 < i2) ? i1 : i2;
}

int limax(int i1,int i2) {
   return (i1 > i2) ? i1 : i2;
}

/*----------------------------------------------------------------------
 
     (NINTRP-1)th degree interpolation from one set of values to another
     (Both sets must be in ascending order.)
 
-----------------------------------------------------------------------*/
 
void dvinqn(DVEC v,DVEC x,DVEC vbar,DVEC xbar,int n,int nbar, 
            double vs, double vf,int nintrp) {
   int      ltrace = 0;

   int      i,   jbar = 0, j = 0;

   if( ltrace ) {
      printf("dvinqn: n: %d\n",n);
      dvpdmp(x,v,n,"v(x)");
   }
   while( jbar < nbar ) {
      if( xbar[jbar] < x[j] ) {
         if( j == 0 ) {
            vbar[jbar] = vs;
         } else {
            if( ! (nintrp % 2) ) {
               i = limin(n-nintrp,limax(0,j-(nintrp/2)));
            } else {
               i = limin(n-nintrp,limax(0,j-(nintrp+1)/2 +
                         (xbar[jbar] > 0.5 * (x[j] + x[j-1]))));
            }
            vbar[jbar] = flipn(xbar[jbar],&x[i],&v[i],nintrp);
         }
         jbar++;
      } else if( xbar[jbar] == x[j] ) {
         vbar[jbar] = v[j];
         jbar++;
      } else {
         if( j == (n - 1) ) {
            vbar[jbar] = vf;
            jbar++;
         } else {
            j++;
         }
      }
   }
   if( ltrace ) {
      printf("dvinqn: nbar: %d\n",nbar);
      dvpdmp(xbar,vbar,nbar,"vbar(xbar)");
   }
}
/*----------------------------------------------------------------------
 
      Low level routine for (N-1)th (N > 1) order polynomial interp-
      olation. Straightforward implementation of Neville's algortihm.
      Should only be used for reasonably well-conditioned problems.
 
----------------------------------------------------------------------*/
 
double flipn(double xbar,DVEC x,DVEC y,int n) {
   double  p[20];
   int     j,  l;

   for( l = 0; l < n; l++ ) {
      p[l] = y[l];
   }
   for( l = 0; l < (n - 1); l++ ) {
      for( j = 0; j < (n - l - 1); j++ ) {
         p[j] = ((xbar - x[j+l+1]) * p[j] +
                  (x[j] - xbar)   * p[j+1]) / (x[j] - x[j+l+1]);
      }
   }
   return p[0];
}

/*--------------------------------------------------------------------------
 
      Returns x-coordinate of first 0-crossing in data y using linear
      interpolation.  Also returns last grid index examined so that
      routine can be used with dv0x below.  0-crossing *only* detected
      through change in sign of y.

      Translated from fortran:

      FORTRAN  x(1:n),   y(1:n),   examine from (x(jx),y(jx)) forward
      C        x[0:n-1], y[0:n-1], examine from (x[jx],y[jx]) forward

      Array indexes need to be offset -1 
 
--------------------------------------------------------------------------*/

double fdvx0(DVEC x,DVEC y,int n,int jst,int *pjx) {
   int        j;
   int        ltrace = 0;
   double     rval = 0;

   *pjx = -1;

   if( ((n - 1) - jst + 1) < 1 ) return rval;

   if( ((n - 1) - jst + 1) == 1 ) {
      if( y[jst] == 0.0 ) {
         *pjx = jst;
         rval = x[jst];
      }
      return rval;
   }

   for( j = jst; j < (n - 1) - 1; j++ ) {
      if( x[j+1] <= x[j] ) return rval;
   }
   for( j = jst; j < (n - 1) - 1; j++ ) {
      if( y[j] * y[j+1] <= 0.0 ) {
         if( y[j] == 0.0 ) {
            rval = x[j];
            *pjx = j;
            return rval;
         }
         if( y[j+1] == 0.0 ) {
            rval = x[j+1];
            *pjx = j + 1;
            return rval;
         }
         rval = x[j] - (x[j+1] - x[j]) * y[j] / (y[j+1] - y[j]);
         *pjx = j;
         if( ltrace ) {
            fprintf(stderr,"fdvx0: Returning %g (%d)\n",rval,*pjx);
         }
         return rval;
      }
   }
   return rval;
}
/*--------------------------------------------------------------------------
 
      Returns x-coordinates of all 0-crossings of data in y by calling
      'fdvx0' repeatedly.

      Translated from fortran:

      FORTRAN  x(1:n),   y(1:n),   x0(1:*pn0)
      C        x[0:n-1], y[0:n-1], x0(0:*pn0-1)

      Array indexes need to be offset -1 
 
--------------------------------------------------------------------------*/
void dvx0(DVEC x,DVEC y,int n,DVEC x0,int *pn0) {
   double    lx0;
   int       jx,      jst;

   *pn0 = 0;
   if( n < 2 ) return;
   jst = 0;
   while( 1 ) {
      lx0 = fdvx0(x,y,n,jst,&jx);
      if( jx < 0 ) return;
      (*pn0)++;
      x0[*pn0-1] = lx0;
      jst = jx + 1;
   }
}

/*--------------------------------------------------------------------------
Returns 1 iff all elements of v are finite floating point values.
--------------------------------------------------------------------------*/
int isdvfinite(DVEC v,int n) {
   int    i;
   for( i = 0; i < n; i++ ) {
      if( !finite(v[i]) ) return 0;
   }
   return 1;
}

/*--------------------------------------------------------------------------
Replaces non-finite values of vector v with val.
--------------------------------------------------------------------------*/
void dvmkfinite(DVEC v,int n,double val) {
   int i;
   for( i = 0; i < n; i++ ) {
      if( !finite(v[i]) ) v[i] = val;
   }
}

/*--------------------------------------------------------------------------
Copies [v1in,v2in] to [v1out,v2out] predicated on uniqueness of v1in 
values which are assumed to be sorted.
--------------------------------------------------------------------------*/
void dvvcopyuniq(DVEC v1in, DVEC v2in, DVEC v1out, DVEC v2out, int nin,
                 int *pnout, double fuzz) {
   int         i;

   *pnout = 0;
   if( nin < 1 ) return;

   v1out[0] = v1in[0];
   v2out[0] = v2in[0];
   (*pnout)++;

   for( i = 1; i < nin; i++ ) {
      if( !eq_fuzz(v1in[i],v1out[(*pnout)-1],fuzz) ) {
         v1out[*pnout] = v1in[i];
         v2out[*pnout] = v2in[i];
         (*pnout)++;
      }
   }
}

/*--------------------------------------------------------------------------
   Grows a double array by factor of two, copying old values and zeroing
	new ones.  Pointer to original array must have been malloc'ed, as it 
	is free'd. 
--------------------------------------------------------------------------*/
double *Dvgrow(double *v, int n) {
   double  *vret = (double *) NULL;
   int      i;
   if( n && v ) {
      if( (vret = (double *) malloc(2 * n * sizeof(double))) ) {
         for( i = 0; i < n; i++ ) vret[i] = v[i];
         for( i = n; i < 2 * n; i++ ) vret[i] = 0.0;
         free(v);
      }
   }
   return vret;
}

#ifdef LINUX_PG
extern double ftn_dabs_   (double *px);
extern double ftn_dsqrt_  (double *px);
extern double ftn_dexp_   (double *px);
extern double ftn_dlog_   (double *px);
extern double ftn_dlog10_ (double *px);
extern double ftn_dsin_   (double *px);
extern double ftn_dcos_   (double *px);
extern double ftn_dtan_   (double *px);
extern double ftn_dacos_  (double *px);
extern double ftn_dasin_  (double *px);
extern double ftn_dacos_  (double *px);
extern double ftn_datan_  (double *px);
extern double ftn_dsinh_  (double *px);
extern double ftn_dcosh_  (double *px);
extern double ftn_dtanh_  (double *px);

double ftn_dabs__(double *px) {
   return ftn_dabs_(px);
}

double ftn_dsqrt__(double *px) {
   return ftn_dsqrt_(px);
}

double ftn_dexp__(double *px) {
   return ftn_dexp_(px);
}

double ftn_dlog__(double *px) {
   return ftn_dlog_(px);
}

double ftn_dlog10__(double *px) {
   return ftn_dlog10_(px);
}

double ftn_dsin__(double *px) {
   return ftn_dsin_(px);
}

double ftn_dcos__(double *px) {
   return ftn_dcos_(px);
}

double ftn_dtan__(double *px) {
   return ftn_dtan_(px);
}

double ftn_dasin__(double *px) {
   return ftn_dasin_(px);
}

double ftn_dacos__(double *px) {
   return ftn_dacos_(px);
}

double ftn_datan__(double *px) {
   return ftn_datan_(px);
}

double ftn_dcosh__(double *px) {
   return ftn_dcosh_(px);
}

double ftn_dsinh__(double *px) {
   return ftn_dsinh_(px);
}

double ftn_dtanh__(double *px) {
   return ftn_dtanh_(px);
}

#endif
