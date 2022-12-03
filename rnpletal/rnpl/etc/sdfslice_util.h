#ifndef SDFSLICE_UTIL_DEF
#define SDFSLICE_UTIL_DEF

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <bbhutil.h>
#include <math.h>

extern       int         Ltrace;

typedef      double     *DVEC;
typedef      int        *IVEC;

#define      r_DVEC        register DVEC
#define      r_IVEC        register IVEC

#define      r_int         register int
#define      r_double      register double

typedef struct     GFT {
   double           time;
   int              version;
   int              rank;
   int              dsize;
   int              csize;
   char            *pname;
   char            *cnames;
   char            *tag;
   int             *shape;
   double          *bbox;
   double          *coords;
   double          *data;
}                  GFT,   *PGFT;

#define SDFSLICE_IVLEN 8192
#define SDFSLICE_FVLEN 8192

#define SDFSLICE_BUFLEN 1024

GFT *slice_GFT(GFT *in, char *Xiv_s, char *Yiv_s, char *Ziv_s, 
   char *Xfv_s, char *Yfv_s, char *Zfv_s, double fuzz);
int ivectoiseq(int *iv, int ivlen, int ivmin, int ivmax, 
   int **piseq, int *piseqlen);
int ivecstrtoiseq(char *ivstr, int ivmin, int ivmax, 
   int **piseq, int *piseqlen);
int fvecstrtodseq(char *fvstr, double **pdseq, int *pdseqlen);
int fvecstrdvtoiseq(char *fvstr, double *dv, int dvlen, double fuzz,
  int **piseq, int *piseqlen);
DVEC make_DVEC(r_int n);
DVEC Dvcopy(r_DVEC v1,r_int n);
DVEC Dvumsh(int n,double v0,double vnm1);
void dvcopy(DVEC v1, DVEC v2, int n);
void dvramp(DVEC v,double v0,double dv,int n);
int dvlookup_linear(DVEC v,int n,double vkey,double fuzz);
int eq_fuzz(double x1,double x2,double fuzz);
void dvdump(DVEC v,int n,char *s);
IVEC make_IVEC(r_int n);
IVEC Ivcopy(r_IVEC v1,r_int n);
void ivdump(IVEC v,int n,char *s);
int ivlookup_linear(IVEC v,int n,int vkey);
IVEC Ivseq(int iv1, int n);
void d3dump(double *a, int d1, int d2, int d3, char *label, int unit);
int file_exists(char *fname);
void dvfdump(FILE *fp,DVEC v,int n,char *s);
void dump_GFT(FILE *fp, GFT *the, char *label, int option);
void free_GFT_data(GFT *in);

#endif
