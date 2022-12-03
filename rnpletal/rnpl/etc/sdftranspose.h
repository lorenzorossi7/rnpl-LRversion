#ifndef _SDFTRANSPOSE_H
#define _SDFTRANSPOSE_H

void usage(void);
void d1d_transpose(double *d, double *dt, int n1);
void d2d_transpose(double *d, double *dt, int n1, int n2);
void d3d_transpose(double *d, double *dt, int n1, int n2, int n3);

#endif
