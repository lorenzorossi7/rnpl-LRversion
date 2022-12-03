#ifndef UTILMATHCDEF
#define UTILMATHCDEF

#include "v_types.h"

extern     double       drange_um(double dmin,double dmax,double d);
extern     int          irange_um(int imin,int imax,int i);

extern     double       dlintr(double frmin,double frmax,double fr,
                               double tomin,double tomax);
extern     int          dr2ir(double dmin,double dmax,double d,
                              int imin,int imax);
extern     double       ir2dr(int imin,int imax,int i,
                              double dmin,double dmax);

extern     double       udvint(DVEC v,int n,double x);
extern     double       uivint(IVEC v,int n,double x);
extern     int          countnans(DVEC v,int n);
extern     void         replacenans(DVEC v,int n,double value);
extern     double       dvmin_nonan(DVEC v,int n);
extern     double       dvmax_nonan(DVEC v,int n);

#endif
