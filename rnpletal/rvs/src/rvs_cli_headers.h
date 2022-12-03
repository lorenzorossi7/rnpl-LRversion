#ifndef RVS_CLI_HEADERS_DEF
#define RVS_CLI_HEADERS_DEF

#include "bbh_types.h"
#include "v_types.h"
#include "f77_types.h"

int vsconnect(void);
int vsopen(const char *name);
int vsxy1(int wt, double time, DVEC x, DVEC y, int n);
int vsxy2(int wt, double time, int tag, DVEC x, DVEC y, int n);
int vsclose(int wt);
int vshang(void);
int vsstatus(int wt);
int vsgetvspid(void);
int vsreset(void);
int vskillall(void);
int vssetmaxlxy(int wt, int lxy);
int vsgetparameter(int wt, int code);
int vssetparameter(int wt, int code, int value);
int vssetaf(int wt, int af);
int vssetthin(int wt);
int vsnameq(const char *name);
int vsglt(int wt);
int vsxyn(const char *name, DVEC x, DVEC y, int n);
int vsxynt(const char *name, double time, DVEC x, DVEC y, int n);
int vsxytagnt(const char *name, double time, DVEC x, DVEC y, IVEC tag, int n);
int vssxynt(const char *name, float time, SVEC x, SVEC y, int n);
int vsxn(const char *name, DVEC x, int n);
int vsxnt(const char *name, double time, DVEC x, int n);
int vsmxynt(const char *name, IVEC start, DVEC time, int nvec, DVEC x, DVEC y);

int vsgnxyni(const char *name,int index);
int vsgxyni(const char *name, int index, DVEC x, DVEC y, Pint pn, Pdouble ptime);
int vsnsct(const char *name,double sc,double time);
int vsgxynall(const char *name,DVEC x,DVEC y,IVEC N,DVEC Time);
int vsgetparametern(const char *name,int which_parameter);
int vsgetlxyn(const char *name);

#endif
