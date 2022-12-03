#ifndef    _IVDEF
#define    _IVDEF

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

#include    "v_types.h"

extern      int         iv_imin(int i1,int i2);
extern      int         iv_imax(int i1,int i2);

extern      IVEC        make_IVEC(r_int n);
extern      void        free_IVEC(r_IVEC v);
extern      IVEC        make_IVEC_p(r_int n);
extern      void        free_IVEC_p(r_IVEC v);

extern      IVECN       make_IVECN(int n);
extern      void        free_IVECN(IVECN the);
extern      IVECN       free_and_null_IVECN(IVECN the);
extern      IVECN       IVECN_iota(int n);

extern      int         ivmin(r_IVEC v,r_int n);
extern      int         ivmax(r_IVEC v,r_int n);
extern      int         ixymin_range(r_IVEC x,r_IVEC y,r_int n,r_int xmin,r_int xmax);
extern      int         ixymax_range(r_IVEC x,r_IVEC y,r_int n,r_int xmin,r_int xmax);

extern      void        ivprint(IVEC v,int n,char *s);
extern      void        ivprint_inc(IVEC v,int n,int inc,char *s);

extern      IVEC        Iviota(r_int n);
extern      IVEC        IViota(r_IVEC v,r_int n);
extern      IVEC        Ivcopy(r_IVEC v1,r_int n);
extern      IVEC        IVcopy(r_IVEC v1,r_IVEC v2,r_int n);

extern      void        ivls(r_IVEC v,r_int sc,r_int n);
extern      void        ivva(r_IVEC v1,r_IVEC v2,r_IVEC v3,r_int n);
extern      void        ivvm(r_IVEC v1,r_IVEC v2,r_IVEC v3,r_int n);
extern      void        ivvs(r_IVEC v1,r_IVEC v2,r_IVEC v3,r_int n);
extern      void        ivsm(r_IVEC v1,r_int s1,r_IVEC v2,int n);
extern      void        ivsa(r_IVEC v1,r_int s1,r_IVEC v2,int n);

extern      IVEC        IVva(r_IVEC v1,r_IVEC v2,r_IVEC v3,r_int n);
extern      IVEC        IVvm(r_IVEC v1,r_IVEC v2,r_IVEC v3,r_int n);
extern      IVEC        IVvs(r_IVEC v1,r_IVEC v2,r_IVEC v3,r_int n);
extern      IVEC        IVsm(r_IVEC v1,r_int s1,r_IVEC v2,int n);
extern      IVEC        IVsa(r_IVEC v1,r_int s1,r_IVEC v2,int n);

extern      int         IVscanf(r_IVEC v,r_int n);
extern      int         IVprintf(r_IVEC v,r_int n);
extern      int         IVVprintf(r_IVEC v1,r_IVEC v2,r_int n);

extern      IVEC        Ivrand(int imin,int imax,int n);
extern      IVEC        IVrand(IVEC v,int imin,int imax,int n);

extern      int         ivsum(IVEC v,int n);
extern      void        ivdump(IVEC v,int n,char *s);
extern      void        ivfdump(FILE *fp,IVEC v,int n,char *s);

extern      void        ivsortup(IVEC v, int n);
extern      void        ivsortdown(IVEC v, int n);
extern      int         sort_up_i(int *a, int  *b); 
extern      int         sort_down_i(int *a,int *b);
extern      void        ivsortupuniq(IVEC vin, int nin, IVEC vout,int *pnout);
extern      void        ivsortdownuniq(IVEC vin, int nin, IVEC vout,int *pnout);
extern      IVECN       ivecn_sortupuniq(IVECN vin);
extern      IVECN       ivecn_sortdownuniq(IVECN vin);
extern      int         eq_fuzz_i(int x1,int x2,int fuzz);

extern      IVEC        ivfscanf(FILE *fp,IVEC v,int *pn);
extern      void        ivfprintf(FILE *fp,IVEC v,int n);

extern      int         ivget(char *fname,IVEC v,int *pn);
extern      IVEC        Ivget(char *fname,int *pn);
extern      IVECN       ivecn_get(char *fname);
extern      int         ivput(char *fname,IVEC v,int n);

extern      void        ivvfscanf(FILE *fp,IVEC v1,IVEC v2,int *pn);
extern      int         ivvget(char *fname,IVEC v1,IVEC v2,int *pn);
extern      int         ivvput(char *fname,IVEC v1,IVEC v2,int n);  
extern      void        ivvfprintf(FILE *fp,IVEC v1,IVEC v2,int n);
extern      IVEC        Ivfscanf(FILE *fp,int *pn);

extern      IVEC        ivdel_el(IVEC v,int n,int i);
extern      IVEC        ivappend(IVEC v,int n,int i);

extern      IVECN       ivecn_append(IVECN the,int i);

extern      void        ivecn_dump(IVECN the,char *s);

extern      void        ivramp(IVEC v,int v0,int iv,int n);
extern      IVEC        Ivramp(int v0,int iv,int n);

extern      int         ivprod(int *v,int n);
extern      int        *Ivgrow(int *v, int n);
#endif
