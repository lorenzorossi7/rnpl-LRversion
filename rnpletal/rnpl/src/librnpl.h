/* $Header: /home/cvs/rnpl/src/librnpl.h,v 1.1.1.1 2013/07/09 00:38:27 cvs Exp $ */
/* librnpl.h */
/* Copyright (c) 1995,1996,1997 by Robert Marsa */

#ifndef _LIBRNPL_H
#define _LIBRNPL_H

#include <sys/types.h>
#include <bbhutil.h>

#define STR_P_SIZE 64

#ifdef __cplusplus
extern "C" {
#endif

typedef struct {
  int cs; /* coordinate system */
  int rank;
  int shape[MAXRANK];
  int bounds[2*MAXRANK];
  double *coords;
} lattice_type;

typedef struct {
  int type;
  double *val;
  lattice_type *lat;
} gfunc_type;

typedef struct {
  int id;
  int cs;
  int rank;
  int shape[MAXRANK];
  int bounds[2*MAXRANK];
  int cptr[MAXRANK];
} latcb;

typedef struct {
  int n_coord;
  double *coord;
} coords;

int rvsxynt(char *n, double t, double *r, 
            double *u, int N);

int rvsmxynt(char *n, int *st, double *tv, int nlvls, double *r, 
             double *u);

int rjava_ser(char *nm, double time, double *cds, double *data, int size);
int rjava_mser(char *nm, int nt, double *time, double *x, double *data, int *size);
int rjava_bbser(char *nm, double time, double *bb, int size, double *data);
int rjava_bser2d(char *nm, double time, double *x, int xs, double *y, int ys,
                 double *data);
int rjava_bmser2d(char *nm, int nt, double *time, double *x, int *nx, double *y,
                  int *ny, double *data);
int rjava_bbser2d(char *nm, double time, double *bb, int xs, int ys, double *data);
int rjava_ser3d(char *nm, double time, int xs, int ys, int zs, double th, 
                double *data);
int rjava_mser3d(char *nm, int nt, double *time, int *xs, int *ys, int *zs, 
                 double th, double *data);
/*               
void gf_output(gfunc_type *gfuncs, char **fnames, char ***cnames,
               double t, int ser, int fout, 
               int nout_f, int *lout_f,
               int *gf_st, int *gf_ln);
*/
int encode(int *lar, int *ar, int nar);
/*
void gff_output(int *ptrs, double *q, char **fnames, int ngfls, 
                char **cnames, int ncnames, double t, int ser, int fout,
                int nout_f, int *lout_f, int *gf_st, int *gf_ln);
*/
void rnpl_id_set_file(const char *fn);
void rnpl_id_set_rank(int r);
void rnpl_id_set_shape(int s1, int s2, int s3);
void rnpl_id_write(const char *dsn, double *data);
void rnpl_id_end();

#ifdef __cplusplus
}
#endif

#endif /* _LIBRNPL_H */
