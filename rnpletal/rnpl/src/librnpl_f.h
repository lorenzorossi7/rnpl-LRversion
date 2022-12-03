/* librnpl_f.h */
/* Copyright (c) 1995,1996 by Robert Marsa */

#ifndef F77_HAS_SYSTEM
int system(const char *s);
#endif

#ifndef F77_HAS_CHDIR
int chdir(const char *s);
#endif

#ifndef F77_HAS_MKDIR
int mkdir(const char *path, int mode);
#endif

int rvsxynt(char *n, double t, double *r, 
            double *u, int N);

void realtostring(double r, char *s);

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

void set_interrupt();
void unset_interrupt();
void reset_interrupt();
int query_interrupt();

