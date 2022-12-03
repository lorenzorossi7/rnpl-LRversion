/* $Header: /home/cvs/rnpl/src/librnpl.c,v 1.1.1.1 2013/07/09 00:38:27 cvs Exp $ */
/* Support routines for RNPL generated programs */
/* copyright (c) 1994-1996 by Robert Marsa */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <signal.h>
#include <librnpl.h>
#ifdef HAVE_HDF
#include <mfhdf.h>
#endif
#include <stdarg.h>
#include <errno.h>
#include <sys/stat.h>

#define IVEC_SIZE 41

int rvsxynt(char *n, double t, double *r, 
              double *u, int N)
{
#ifdef HAVE_LIBVS
  return vsxynt(n,t,r,u,N);
#else
  return 1;
#endif
}

void realtostring(double r, char *s)
{
  sprintf(s,"%g",r);
}

/*
void gff_output(int *ptrs, double *q, char **fnames, int ngfls, 
                char **cnames, int ncnames, double t, int ser, int fout,
                int nout_f, int *lout_f, int *gf_st, int *gf_ln)
{
  int i,j,k;
  double *crds,*c;
  latcb *l;

  for(i=0;i<nout_f;i++){
    if(gf_ln[lout_f[i]]==1)
      j=gf_st[lout_f[i]];
    else j=gf_st[lout_f[i]]+1;
    l=(latcb *)(ptrs + ptrs[ngfls+j]-1);
    if(ser && (l->rank == 1)){
      rvsxynt(fnames[lout_f[i]],t,&q[l->cptr[0]-1],&q[ptrs[j]-1],l->shape[0]);
    }
    if(fout){
#ifdef HAVE_HDF
      gft_set_d_type(DFNT_FLOAT64);
#endif
      crds=vec_alloc(rivprod(l->shape,l->rank));
      for(c=crds,k=0;k<l->rank;k++){
        rdvcpy(c,&q[l->cptr[k]-1],l->shape[k]);
        c+=l->shape[k];
      }
      gft_out_full(fnames[lout_f[i]],t,l->shape,*(cnames+l->cs*(MAXRANK+1)+1),
                   l->rank,crds,&q[ptrs[j]-1]);
      free(crds);
    }
  }
}

void gf_output(gfunc_type *gfuncs, char **fnames, char ***cnames,
               double t, int ser, int fout,
               int nout_f, int *lout_f,
               int *gf_st, int *gf_ln)
{
  int i,j,k;
  double *crds,*c;
  
  for(i=0;i<nout_f;i++){
    if(gf_ln[lout_f[i]]==1)
      j=gf_st[lout_f[i]];
    else j=gf_st[lout_f[i]]+1;
    if(ser && (gfuncs[j].lat->rank == 1)){
      rvsxynt(fnames[lout_f[i]],t,gfuncs[j].lat->coords,gfuncs[j].val,
             gfuncs[j].lat->shape[0]);
    }
    if(fout){
#ifdef HAVE_HDF
      gft_set_d_type(gfuncs[j].type);
#endif
      crds=vec_alloc(rivprod(gfuncs[j].lat->shape,gfuncs[j].lat->rank));
      for(c=crds,k=0;k<gfuncs[j].lat->rank;k++){
        rdvcpy(c,gfuncs[j].lat->coords,gfuncs[j].lat->shape[k]);
        c+=gfuncs[j].lat->shape[k];
      }
      gft_out_full(fnames[lout_f[i]],t,gfuncs[j].lat->shape,cnames[gfuncs[j].lat->cs][1],
                   gfuncs[j].lat->rank,crds,gfuncs[j].val);
      free(crds);
    }
  }
}
*/

int encode(int *lar, int *ar, int nar)
{
  int i,j;

  for(i=0,j=0;i<nar;i++)
    if(ar[i]==1)
      lar[j++]=i;
  return j;
}

/* support for initial data fragment */
static int id_rank;
static int id_shp[3];

void rnpl_id_set_file(const char *fn)
{
  gft_set_single(fn);
}

void rnpl_id_set_rank(int r)
{
  id_rank=r;
}

void rnpl_id_set_shape(int s1, int s2, int s3)
{
  id_shp[0]=s1;
  id_shp[1]=s2;
  id_shp[2]=s3;
}

void rnpl_id_write(const char *dsn, double *data)
{
  gft_write_id_gf((char *)dsn,id_shp,id_rank,data);
}

void rnpl_id_end()
{
  double td;
  int ti;
  
  td=0.0;
  ti=0;
  gft_write_id_float("start_t",&td,1);
  gft_write_id_int("s_step",&ti,1);
  gft_set_multi();
}

/* Interrupt handling for FORTRAN ... */
/* These routines are written by Matthew W. Choptuik */
/* Copyright (c) 1995 by Matthew W. Choptuik */

static int Interrupt_set = 0;

void sig_onintr(int foo) {
   Interrupt_set = 1;
   signal(SIGINT,sig_onintr);
}
  
void set_interrupt(void) {
  Interrupt_set = 0;
  signal(SIGINT,sig_onintr);
}

void unset_interrupt(void) {
  Interrupt_set = 0;
   signal(SIGINT,SIG_DFL);
}

void reset_interrupt(void) {
  Interrupt_set = 0;
}

int query_interrupt(void) {
  return Interrupt_set;
}

