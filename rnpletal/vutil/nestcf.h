#ifndef NESTCF_DEF
#define NESTCF_DEF

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <limits.h>
#include <time.h>
#include "v.h"

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


#define  ON   0
#define  OFF  1

typedef struct FNODE {
   int             i_min;
   int             i_max;
   int             opt_nc;
   DVEC            c;
   double          fit;
   struct FNODE   *next;
}              FNODE,   *PFNODE;

extern    PFNODE    make_FNODE(int i_min, int i_max, PFNODE next);
extern    void      dump_FNODE_list(char * mess,PFNODE head);
extern    void      dump_FNODE_list_brief(char * mess,PFNODE head,PFNODE curr);
extern    PFNODE    split_FNODE(PFNODE old);
extern    int       npts_FNODE(PFNODE p);
extern    int       nestcf(DVEC x, DVEC f, int n, int min_nc, int max_nc, 
                           double fit_crit, DVEC xseg, int *pnseg, DMAT c,
                           IVEC nc);
extern    int       old_nestcf(DVEC x, DVEC f, int n, int min_nc, int max_nc, 
                              double fit_crit, DVEC xseg, int *pnseg, DMAT c,
                              IVEC nc);

extern    int       dvnestcf_(DVEC x, DVEC f, int *pn, DVEC c, int *pmax_nc, 
                              DVEC xseg, int *pnseg);

extern     int      dvpulse_reflect(DVEC yin, DVEC rin, int nrin, 
                       DVEC yout, DVEC rout, double rmin, double rmax, 
                       double roffdr, int roffp, double gapdr);
#endif
