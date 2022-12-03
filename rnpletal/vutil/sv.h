#ifndef _SVDEF
#define _SVDEF

#include <stdio.h>

#ifdef HAVE_VALUES_H
#include <values.h>
#else
/*  This assumes an IEEE implementation! */
#ifndef MAXDOUBLE
#define MAXDOUBLE  1.7976931348623157e+308
#endif
#ifndef MAXFLOAT
#define MAXFLOAT   ((float)3.40282347e+38)
#endif
#ifndef MAXINT
#define MAXINT     2147483647
#endif
#endif

#include "v_types.h"

extern     float        sv_smin(float s1,float s2);
extern     float        sv_smax(float s1,float s2);

extern     SVEC         make_SVEC(r_int n);
extern     void         free_SVEC(r_SVEC v);
extern     SVEC         make_SVEC_p(r_int n);
extern     void         free_SVEC_p(r_SVEC v);
extern     SVECN        make_SVECN(int n);
extern     void         free_SVECN(SVECN the);

extern     SIVEC        make_SIVEC(r_int n);
extern     void         free_SIVEC(SIVEC v);

extern     float        svmin(r_SVEC v,r_int n);
extern     float        svmax(r_SVEC v,r_int n);
extern     float        sxymin_range(r_SVEC x,r_SVEC y,r_int n,
                           r_float xmin,r_float xmax);
extern     float        sxymax_range(r_SVEC x,r_SVEC y,r_int n,
                           r_float xmin,r_float xmax);
extern     void         svprint(SVEC v,int n,char *s);
extern     void         sivprint(SIVEC v,int n,char *s);
extern     void         svprint_inc(SVEC v,int n,int inc,char *s);
extern     void         svprint_range(SVEC v,int n,char *s);
extern     SVEC         Sviota(r_int n);
extern     SVEC         SViota(r_SVEC v,r_int n);
extern     SVEC         Svcopy(r_SVEC v1,r_int n);
extern     SVEC         SVcopy(r_SVEC v1,r_SVEC v2,r_int n);

extern     void         svva(r_SVEC v1,r_SVEC v2,r_SVEC v3,r_int n);
extern     void         svvm(r_SVEC v1,r_SVEC v2,r_SVEC v3,r_int n);
extern     void         svvs(r_SVEC v1,r_SVEC v2,r_SVEC v3,r_int n);
extern     void         svsm(r_SVEC v1,r_float s1,r_SVEC v2,r_int n);
extern     void         svsa(r_SVEC v1,r_float s1,r_SVEC v2,r_int n);

extern     SVEC         SVva(r_SVEC v1,r_SVEC v2,r_SVEC v3,r_int n);
extern     SVEC         SVvm(r_SVEC v1,r_SVEC v2,r_SVEC v3,r_int n);
extern     SVEC         SVvs(r_SVEC v1,r_SVEC v2,r_SVEC v3,r_int n);
extern     SVEC         SVsm(r_SVEC v1,r_float s1,r_SVEC v2,r_int n);
extern     SVEC         SVsa(r_SVEC v1,r_float s1,r_SVEC v2,r_int n);

extern     SVEC         SVrand(r_SVEC v,r_float v_min,r_float v_max,r_int n);

extern     int          SVscanf(r_SVEC v,r_int n);
extern     int          SVprintf(SVEC v,r_int n);
extern     int          SVVscanf(r_SVEC v1,r_SVEC v2,r_int n);
extern     int          SVVprintf(r_SVEC v1,r_SVEC v2,r_int n);

extern     void         svls(r_SVEC v1,float s1,int n);
extern     void         svav(r_SVEC v1,r_SVEC v2,int n);
extern     int          isvnz(SVEC v,int n);
extern     void         svramp(SVEC v,float v0,float dv,int n);
extern     SVEC         Svramp(float v0,float dv,int n);
extern     SVEC         Svumesh(float v0,float vnm1,int n);

extern     float        svsum(SVEC v,int n);

extern     void         svdd01(SVEC v1,SVEC v2,float h,int n);

extern     void         svdump(SVEC v,int n,char *s);

extern     float        srange(float xmin,float x,float xmax,float fuzz);

extern     void         svfout(FILE *,SVEC,int);

extern     float        svmindv(SVEC v,int n);

extern     void         svsortup(SVEC v, int n);
extern     void         svsortdown(SVEC v, int n);
extern     int          sort_up_f(float *a, float  *b); 
extern     int          sort_down_f(float *a,float *b);
extern     void         svsortupuniq(SVEC vin, int nin, SVEC vout, 
                           int *pnout, float  fuzz);
extern     void         svsortdownuniq(SVEC vin, int nin, SVEC vout, 
                           int *pnout, float  fuzz);
extern     int          eq_fuzz_f(float x1,float x2,float fuzz);

extern     int          svlsindex(SVEC v,int n,float key,float fuzz);
extern     IVEC         make_IVEC_sv(r_int n);
extern     IVEC         Svlsindex(SVEC v,int n,SVEC key,int nkey,float fuzz);

extern     void         svvmergeup(SVEC v1,int n1,SVEC v2,int n2,
                           SVEC v3,int *pn3,float fuzz);
extern     void         svvmergedown(SVEC v1,int n1,SVEC v2,int n2,
                           SVEC v3,int *pn3,float fuzz);

extern     void         svmrg(SVEC v1,SVEC v2,SVEC v3,int n);
extern     void         svumrg(SVEC v1,SVEC v2,SVEC v3,int n);

#define    SORT_UP      0
#define    SORT_DOWN    1

extern     int          sv2sort(SVEC v1,SVEC v2,int n,int code);

extern     int          sort_up_s2(PS2 el1,PS2 el2);
extern     int          sort_down_s2(PS2 el1,PS2 el2);

extern     SVEC         svdel_el(SVEC v,int n,int i);
extern     int          sv2insert(SVEC *pv1,SVEC *pv2,int n,
                           float  new1,float new2,int code);

extern     int          ixsvmin(SVEC v,int n);
extern     int          ixsvmax(SVEC v,int n);
extern     int          ixsvabsmin(SVEC v,int n);
extern     int          ixsvabsmax(SVEC v,int n);

extern     int          ixsvnearest(SVEC v,int n,float key);
extern     int          ixsv2nearest(SVEC v1,SVEC v2,int n,
                           float  key1,float key2,float asp1by2);
extern     int          svpulse_reflect(SVEC yin, SVEC rin, int nrin, 
                           SVEC yout, SVEC rout, float  rmin, float  rmax, 
                           float roffdr, int roffp, float  gapdr);
extern     void         svpyth(SVEC v1,SVEC v2,SVEC v3,int n);
extern     float        svhash(SVEC v,int n);

extern     void         svfapl(SVEC v1,SVEC v2,PFF f,int n);
extern     SVEC         Svfapl(SVEC v1,PFF f,int n);

extern     SVEC         svfscanf(FILE *fp,SVEC v,int *pn);
extern     void         svfprintf(FILE *fp,SVEC v,int n);

extern     int          svget(char *fname,SVEC v,int *pn);
extern     SVEC         Svget(char *fname,int *pn);
extern     int          svput(char *fname,SVEC v,int n);

extern     void         svvfscanf(FILE *fp,SVEC v1,SVEC v2,int *pn);
extern     int          svvget(char *fname,SVEC v1,SVEC v2,int *pn);
extern     int          svvput(char *fname,SVEC v1,SVEC v2,int n);  
extern     void         svvfprintf(FILE *fp,SVEC v1,SVEC v2,int n);
extern     SVEC         Svfscanf(FILE *fp,int *pn);
extern     PSVEC        Svvfscanf(FILE *fp,int *pn);
extern     PSVEC        Svvget(char *fname,int *pn);

extern     SVEC         svappend(SVEC v,int n,float val);
extern     SVECN        svecn_append(SVECN the,float val);

extern     SVEC         svfscanf(FILE *fp,SVEC v,int *pn);
extern     void         svfprintf(FILE *fp,SVEC v,int n);

extern     IVEC         ivmakemasksv(SVEC vin,int nin,PFI_F logic);
extern     IVEC         ivmakemasksvsvsv(SVEC vin1, SVEC vin2, SVEC vin3,
                                         int nin,PFI_FFF logic);
extern     SVEC         svreduce(SVEC vin,IVEC mask,int nin,int *pnout);

#define     MSVFAIL(v,n)    !(v = make_SVEC(n))

#include  "sveclib.h"
#endif
