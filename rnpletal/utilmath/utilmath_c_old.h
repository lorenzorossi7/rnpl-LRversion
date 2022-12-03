#ifndef UTILMATH_C_OLD_DEF
#define UTILMATH_C_OLD_DEF

extern    double   drand48_(void);
extern    int      isnand_(double *pnum);
extern    int      countnans_(double *v,int *pn);
extern    void     replacenans_(double *v,int *pn,double *pvalue);

#endif
