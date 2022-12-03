#ifndef LIBXVS_F_DEF
#define LIBXVS_F_DEF

int vsxynt(const char *name, double time, double *x, double *y, int n);
int xvs(const char *name, double time, double *x, double *y, int n);
int vsmxynt(const char *name, int *start, double *time, int n, double *x, double *y);

#endif
