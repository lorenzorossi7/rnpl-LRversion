#ifndef DEF_SDF_UTIL
#define DEF_SDF_UTIL

#include <stdio.h>
#include <string.h>
#include <sdf.h>
#include <sdf_priv.h>

#include "v_types.h"
#include "quad_types.h"
#include "sdf_types.h"

#define   OFF       0
#define   ON        1
#define   BUFLEN    1024

extern PSDFDS     new_SDFDS(void);
extern PSDFDS     free_SDFDS(PSDFDS the);
extern PSDFDS     message_SDFDS(char *message);
extern int        read_SDFDS(FILE *fp, PSDFDS the);
extern int        write_SDFDS(FILE *fp, PSDFDS the);
extern PSDFDS     new_read_SDFDS(FILE *fp, int *prc);
extern DQUAD      dquad_SDFDS(PSDFDS the);

extern void       dump_SDFDS(FILE *fp, PSDFDS the, char *label, int option);

extern PSDFDS     test_SDFDS_1d(char *name, int n);

extern double     l_dvmin(r_DVEC v,r_int n);
extern double     l_dvmax(r_DVEC v,r_int n);
extern void       l_ivfdump(FILE *fp,IVEC v,int n,char *s);
extern void       l_dvfdump(FILE *fp,DVEC v,int n,char *s);
extern void       l_dvvfscanf(FILE *fp, double **pv1, double **pv2, int *pn);

extern int        gft_read_rank_first(const char *gf_name, int *prank);

#endif
