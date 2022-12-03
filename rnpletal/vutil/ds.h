#ifndef _DSDEF
#define _DSDEF

#include "ds_types.h"

extern  double   dmin(r_double x0,r_double x1);
extern  double   dmax(r_double x0,r_double x1);
extern  DSEG     make_DSEG(double x0,double x1);
extern  int      null_DSEG(DSEG ds);
extern  DSEG     and_DSEG(DSEG ds,DSEG dsp);

extern  void     pf_DSEG(DSEG ds);
extern  void     ppf_DSEG(DSEG ds,char *s);
extern  void     sf_DSEG(PDSEG pds);

extern  double   A_saw_DSEG(DSEG ds,r_double h);

#endif
