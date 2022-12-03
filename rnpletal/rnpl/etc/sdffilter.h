#ifndef _SDFFILTER_H
#define _SDFFILTER_H

typedef double    *DVEC;

typedef struct     GFT {
   double           time;
   int              version;
   int              rank;
   int              dsize;
   int              csize;
   char            *pname;
   char            *cnames;
   char            *tag;
   int             *shape;
   double          *bbox;
   double          *coords;
   double          *data;
}                  GFT,   *PGFT;

DVEC     make_DVEC(int n);
void     free_DVEC(DVEC v);
DVEC     copy_DVEC(DVEC v, int n);
DVEC     make_umesh(int n, double c0, double c1);
void     dvsa(DVEC v1,double s1,DVEC v2,int n);
int      ixdvabsmin(DVEC v,int n);
int      ixdvnearest(DVEC v,int n,double key);
int      file_exists(char *fname);
void     reset_ivec(void);
void     string_to_bbox(char *s, double **pbbox, int *prank);
void     dump_GFT(FILE *fp, GFT *the, char *label, int option);
void     dvfdump(FILE *fp,DVEC v,int n,char *s);
int      read_GFT(char *sdfname, int step, GFT *the); 
int      write_GFT(char *sdfname,GFT *the); 
void     free_GFT_data(GFT *the);
GFT     *clip_GFT(GFT *in,double *bbox);
#endif
