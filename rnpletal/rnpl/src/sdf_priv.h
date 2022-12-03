/* $Header: /home/cvs/rnpl/src/sdf_priv.h,v 1.1.1.1 2013/07/09 00:38:27 cvs Exp $ */
/* sdf_priv.h */
/* header information needed by sdf.c but which shouldn't be "public" */
/* Copyright (c) 1997 by Robert L. Marsa and Matthew W. Choptuik */
#ifndef _SDF_PRIV_H
#define _SDF_PRIV_H

#include <stdlib.h>
#include <time.h>

#define BBH_MAXSTRINGLEN 512

#ifdef __cplusplus
extern "C" {
#endif

typedef struct sdfh {
  double time;
  double version;
  double rank;
  double dsize;
  double csize;
  double pnlen;
  double cnlen;
  double tglen;
} bbh_sdf_header;

typedef struct sdfb {
  char *pname;
  char *cnames;
  double *bbox;
  double *shape;
  char *tag;
  double *c;
  double *d;
} bbh_sdf_body;

typedef enum { StrClosed,StrRead,StrWrite } StrState;

typedef struct _dary {
  double *data;
  int clen,mlen,csize;
} dynarray;

typedef struct gsfi {
  int index;
  long position;
  char *name;
  char *tag;
  int rank;
  int *shape;
  double time;
  double *bbox;
  struct gsfi *left;
  struct gsfi *right;
} gft_sdf_file_index;

typedef struct _gary {
  gft_sdf_file_index **data;
  int clen,mlen,csize;
} gsfiarray;

typedef struct gsfd {
  char *gfname;
  char *fname;
  FILE *fp;
  StrState state;
  time_t mtime;
  int changed;
  dynarray *tarray;
  gsfiarray *idx;
  struct gsfd *left;
  struct gsfd *right;
} gft_sdf_file_data;

gft_sdf_file_index **gsfipvec_alloc(const int size);

dynarray *new_dynarray(void);
void delete_dynarray(dynarray *da);
void add_item(dynarray *da, const double it);
double get_item(dynarray *da, const int i);
int dynarray_len(dynarray *da);

gsfiarray *new_gsfiarray(void);
void delete_gsfiarray(gsfiarray *da);
void add_gsfiitem(gsfiarray *da, gft_sdf_file_index *it);
gft_sdf_file_index *get_gsfiitem(gsfiarray *da, const int i);
int gsfiarray_len(gsfiarray *da);

gft_sdf_file_index *gsfi_parent(gft_sdf_file_index *root, gft_sdf_file_index *gp);
gft_sdf_file_index *gsfi_successor(gft_sdf_file_index *gp);
gft_sdf_file_index *gsfi_rotate_r(gft_sdf_file_index *p, gft_sdf_file_index *g);
gft_sdf_file_index *gsfi_rotate_l(gft_sdf_file_index *p, gft_sdf_file_index *g);
void gsfi_insert(gft_sdf_file_index *root, gft_sdf_file_index *gp);
gft_sdf_file_index *gsfi_remove(gft_sdf_file_index *root, gft_sdf_file_index *gp);
gft_sdf_file_index *gsfi_find(gft_sdf_file_index *root, const char *gfname);
gft_sdf_file_index *gsfi_new(void);
void gsfi_delete(gft_sdf_file_index *gp);

gft_sdf_file_data *gsfd_parent(gft_sdf_file_data *root, gft_sdf_file_data *gp);
gft_sdf_file_data *gsfd_successor(gft_sdf_file_data *gp);
gft_sdf_file_data *gsfd_rotate_r(gft_sdf_file_data *p, gft_sdf_file_data *g);
gft_sdf_file_data *gsfd_rotate_l(gft_sdf_file_data *p, gft_sdf_file_data *g);
void gsfd_insert(gft_sdf_file_data *root, gft_sdf_file_data *gp);
gft_sdf_file_data *gsfd_remove(gft_sdf_file_data *root, gft_sdf_file_data *gp);
gft_sdf_file_data *gsfd_find(gft_sdf_file_data *root, const char *gfname);
gft_sdf_file_data *gsfd_new(void);
void gsfd_delete(gft_sdf_file_data *gp);
void gsfd_close(gft_sdf_file_data *gp);
void gsfd_close_all(gft_sdf_file_data *gp);
int gsfd_file_changed(gft_sdf_file_data *gp);
int gsfd_make_index(gft_sdf_file_data *gp);

int gft_make_index(const char *gfname);
int gft_file_changed(const char *gfname);
gft_sdf_file_data *gft_create_sdf_stream(const char *gfname);
gft_sdf_file_data *gft_open_sdf_stream(const char *gfname);
int mid_write_sdf_stream(const char *gfname, const double time, const int rank, 
                         const int dsize, const int csize, const char *cnames, 
                         const char *tag, const int *shape, const double *cds, const double *data);
int mid_read_sdf_stream(const int alloc_flag, const char *gfname, double *time, int *version, 
                        int *rank, int *dsize, int *csize, char **dpname, char **cnames, 
                        char **tag, int **shape, double **bbox, double **cds, double **data);
int low_write_sdf_stream(FILE *fp, const char *gfname, const double time, const int rank, 
                         const int dsize, const int csize, const char *cnames, 
                         const char *tag, const int *shape, const double *cds, const double *data);
int low_read_sdf_stream(const int alloc_flag, FILE *fp, double *time, int *version, 
                        int *rank, int *dsize, int *csize, char **dpname, char **cnames, 
                        char **tag, int **shape, double **bbox, double **cds, double **data);

gft_sdf_file_data *gft_open_sdf_file(char *fname);

#ifdef __cplusplus
}
#endif

#endif
