#include <stdlib.h>
#include <math.h>
#include <string.h>
#include <unistd.h>
#include <stdio.h>
#include <libgen.h>
#include <sdf_priv.h>
#include <sdf.h>
#include "sdffilter.h"  

#define    ON       1
#define    OFF      0
#define    BUFLEN   1024
#define    IVLEN    1024
#define    NTMAX    1024*1024
#define    MAXROW   12
#define    MAXCOL   12
#define    MAXPLOT  144  

int        Ltrace = OFF;

char       B[BUFLEN];

int        Ivec[IVLEN];
char      *Infilename = (char *) NULL;
char      *Outfilename = (char *) NULL;
char      *Printname = (char *) NULL;
char      *Sivec = (char *) NULL;
char      *Sclipbbox;
double    *Clipbbox = (double *) NULL;
int        Clipbboxrank;
int        Ntlevs;

DVEC make_DVEC(int n) {
   return (DVEC) malloc(n * sizeof(double));
}

void free_DVEC(DVEC v) {
   if( v ) free(v);
}

DVEC copy_DVEC(DVEC v, int n) {
   DVEC  vcopy = (DVEC) NULL;
   int   i;

   if( n < 0 || !(vcopy = make_DVEC(n)) ) return vcopy;

   for( i = 0; i < n; i ++ ) vcopy[i] = v[i];

   return vcopy;
}

void dvsa(DVEC v1,double s1,DVEC v2,int n) {
   int    i;

   for( i = 0; i < n; i++ ) v2[i] = v1[i] + s1;
}

DVEC make_umesh(int n, double v1, double vn) {
   DVEC    v = (DVEC) NULL;
   double  dv;
   int     i;

   if( n < 0 || !(v = make_DVEC(n)) ) return v;
         if( n > 1 ) {
            dv = (vn - v1) / (n - 1);
            for( i = 0; i < n - 1; i++ ) {
               v[i] = v1 + i * dv;
            }
            v[n-1] = vn;
         } else {
            v[0] = v1;
         }
   return v;
}

#define   NEWABSEXT(a)  iext = a; vext = fabs(v[a]);
int ixdvabsmin(DVEC v,int n) {
   double   vext;
   int      i,    iext;

   if( n < 1 ) return -1;

   NEWABSEXT(0);
   for( i = 1 ; i < n; i++ ) {
      if( fabs(v[i]) < vext ) { NEWABSEXT(i) }
   }
   return iext;
}
#undef NEWBASEXT

int ixdvnearest(DVEC v,int n,double key) {
   DVEC   tv;
   int    i;
   
   if( n < 1 ) return -1;

   if( !(tv = make_DVEC(n)) ) {
      fprintf(stderr,"ixdvnearest: make_DVEC(%d) failed.\n",n);
      return -1;
   }
   dvsa(v,-key,tv,n);
   i = ixdvabsmin(tv,n);
   free_DVEC(tv);
   return i;
}


int file_exists(char *fname) {
   FILE    *fp;
 
   if( (fp = fopen(fname,"r")) != NULL ) {
      fclose(fp);
      return 1;
   } else {
      return 0;
   }
} 

void reset_ivec(void) {
   if( !Sivec ) return;
   sprintf(B,"ivec:=%s",Sivec);
   sget_ivec_param(B,"ivec",Ivec,IVLEN);
   fixup_ivec(1,NTMAX,0,Ivec);
} 

void string_to_bbox(char *s, double **pbbox, int *prank) {
   double   vals[6];
   int      i;
   *pbbox = (double *) NULL;
   *prank = 0;
   if( s ) {
      if (        sscanf(s,"[%lf %lf %lf %lf %lf %lf]",
               vals,vals+1,vals+2,vals+3,vals+4,vals+5) == 6 ) {
         *prank = 3;
         *pbbox = (double *) malloc(6 * sizeof(double));
      } else if ( sscanf(s,"[%lf %lf %lf %lf]",
               vals,vals+1,vals+2,vals+3) == 4 ) {
         *prank = 2;
         *pbbox = (double *) malloc(4 * sizeof(double));
      } else if ( sscanf(s,"[%lf %lf]",vals,vals+1) == 2 ) {
         *prank = 1;
         *pbbox = (double *) malloc(2 * sizeof(double));
      } 
      if( *prank ) for( i = 0; i < 2 * *prank; i++ ) (*pbbox)[i] = vals[i];
   }
   return;
}

void dump_GFT(FILE *fp, GFT *the, char *label, int option) {
   char    buf[128];
   int     i, dsize;

   if( !the ) return;

   fprintf(fp,"dump_GFT: %s BEGIN\n",label);
   fprintf(fp,"   time=%g\n",the->time);
   fprintf(fp,"   version=%d\n",the->version);
   fprintf(fp,"   rank=%d\n",the->rank);
   fprintf(fp,"   dsize=%d\n",the->dsize);
   fprintf(fp,"   csize=%d\n",the->csize);
   fprintf(fp,"   pname='%s'\n",the->pname);
   fprintf(fp,"   cnames='%s'\n",the->cnames);
   fprintf(fp,"   tag='%s'\n",the->tag);
   fprintf(fp,"   shape=[");
   for( i = 0; i < the->rank; i++ ) {
      fprintf(fp,"%d%s",the->shape[i], (i < the->rank-1) ? " " : "]\n");
   }
   if( option ) {
      dvfdump(fp,the->coords,the->csize,"coords");
   }
   if( option ) {
      dvfdump(fp,the->data,the->dsize,"data");
   }
   fprintf(fp,"dump_GFT: %s END\n",label);
}

void dvfdump(FILE *fp,DVEC v,int n,char *s) {
   int     i, per_line = 4;

   if( v != NULL ) {
      fprintf(fp,"<<< %s >>>\n\n",s);
      for( i = 0; i < n; i++ ) {
         fprintf(fp, ((i + 1) % per_line ? "%19.10E" : "%19.10E\n"),v[i]);
      }
      fprintf(fp, (n % per_line) ? "\n\n" : "\n" );
   }
}

void free_GFT_data(GFT *in) {
   if( in ) {
      if( in->pname ) free(in->pname);
      if( in->cnames ) free(in->cnames);
      if( in->shape ) free(in->shape);
      if( in->bbox ) free(in->bbox);
      if( in->coords ) free(in->coords);
      if( in->data ) free(in->data);
   }
}

/* 2013-09-08: Modified to work with bounding box coordinates ... */
GFT *clip_GFT(GFT *in,double *bb) {
   GFT  *out = (GFT *) NULL;
   int  i, imin, imax, j, jmin, jmax, k, kmin, kmax;
   int  nxout = 0, nyout = 0, nzout = 0;
   DVEC lcoords0 = (DVEC) NULL, lcoords1 = (DVEC) NULL, lcoords2 = (DVEC) NULL;

   if( in ) {
      switch( in->rank ) {
      case 1:
         if( Ltrace ) {
            fprintf(stderr,"clip_GFT: bb=[%g %g] csize=%d\n",
               bb[0],bb[1],in->csize);
         }
         /* If input coords in bbox form, convert to full ... */
         if( in->csize == 2 ) {
            lcoords0 = make_umesh(in->shape[0],in->coords[0],in->coords[1]);
         } else {
            lcoords0 = copy_DVEC(in->coords,in->csize);
         }
         imin = ixdvnearest(lcoords0,in->shape[0],bb[0]);
         imax = ixdvnearest(lcoords0,in->shape[0],bb[1]);
         if( Ltrace ) {
            fprintf(stderr,
               "imin=%d imax=%d\n",imin,imax);
         }
         nxout = (imin > -1 && imax > -1) ? (imax - imin + 1) : 0;
         if( nxout ) {
            out = (GFT *) malloc(sizeof(GFT));
            out->time    = in->time;
            out->version = in->version;
            out->rank    = 1;
            out->dsize   = nxout;
            out->csize   = nxout;
            out->pname   = in->pname ? strdup(in->pname) : (char *) NULL;
            out->cnames  = in->cnames ? strdup(in->cnames) : (char *) NULL;
            out->bbox    = (double *) NULL;
            out->tag     = in->tag ? strdup(in->tag) : (char *) NULL;
            out->shape   = (int *) malloc(1 * sizeof(int));
            out->shape[0] = nxout;
            out->coords   = (double *) malloc(out->csize * sizeof(double));
            out->data     = (double *) malloc(out->dsize * sizeof(double));
            for( i = imin; i <= imax; i++ ) {
               out->data[i-imin] = in->data[i];
            }
            if( Ltrace ) fprintf(stderr,"clip_GFT: Clipped data.\n");
            for( i = imin; i <= imax; i++ ) {
               out->coords[i-imin] = lcoords0[i];
            }
            if( Ltrace ) fprintf(stderr,"clip_GFT: Clipped coords.\n");
         }
         if( lcoords0 ) free(lcoords0);
         break;
      case 2:
         if( Ltrace ) {
            fprintf(stderr,"clip_GFT: bb=[%g %g %g %g] csize=%d\n",
               bb[0],bb[1],bb[2],bb[3],in->csize);
         }
         /* If input coords in bbox form, convert to full ... */
         if( in->csize == 4 ) {
            lcoords0 = make_umesh(in->shape[0],in->coords[0],in->coords[1]);
            lcoords1 = make_umesh(in->shape[1],in->coords[2],in->coords[3]);
         } else {
            lcoords0 = copy_DVEC(in->coords,in->shape[0]);
            lcoords1 = copy_DVEC(in->coords+in->shape[0],in->shape[1]);
         }
         imin = ixdvnearest(lcoords0,in->shape[0],bb[0]);
         imax = ixdvnearest(lcoords0,in->shape[0],bb[1]);
         jmin = ixdvnearest(lcoords1,in->shape[1],bb[2]);
         jmax = ixdvnearest(lcoords1,in->shape[1],bb[3]);
         if( Ltrace ) {
            fprintf(stderr,
               "imin=%d imax=%d jmin=%d jmax=%d\n",imin,imax,jmin,jmax);
         }
         nxout = (imin > -1 && imax > -1) ? (imax - imin + 1) : 0;
         nyout = (jmin > -1 && jmax > -1) ? (jmax - jmin + 1) : 0;
         if( nxout * nyout ) {
            out = (GFT *) malloc(sizeof(GFT));
            out->time    = in->time;
            out->version = in->version;
            out->rank    = 2;
            out->dsize   = nxout * nyout;
            out->csize   = nxout + nyout;
            out->pname   = in->pname ? strdup(in->pname) : (char *) NULL;
            out->cnames  = in->cnames ? strdup(in->cnames) : (char *) NULL;
            out->bbox    = (double *) NULL;
            out->tag     = in->tag ? strdup(in->tag) : (char *) NULL;
            out->shape   = (int *) malloc(2 * sizeof(int));
            out->shape[0] = nxout;
            out->shape[1] = nyout;
            out->coords   = (double *) malloc(out->csize * sizeof(double));
            out->data     = (double *) malloc(out->dsize * sizeof(double));
            for( j = jmin; j <= jmax; j++ ) {
               for( i = imin; i <= imax; i++ ) {
                  out->data[(j-jmin)*nxout+(i-imin)] = 
                     in->data[j*in->shape[0]+i];
               }
            }
            if( Ltrace ) fprintf(stderr,"clip_GFT: Clipped data.\n");
            for( i = imin; i <= imax; i++ ) {
               out->coords[i-imin] = lcoords0[i];
            }
            for( j = jmin; j <= jmax; j++ ) {
               out->coords[nxout+j-jmin] = lcoords1[j];
            }
            if( Ltrace ) fprintf(stderr,"clip_GFT: Clipped coords.\n");
         }
         if( lcoords0 ) free(lcoords0);
         if( lcoords1 ) free(lcoords1);
         break;
      case 3:
         if( Ltrace ) {
            fprintf(stderr,"clip_GFT: bb=[%g %g %g %g %g %g] csize=%d\n",
               bb[0],bb[1],bb[2],bb[3],bb[4],bb[5],in->csize);
         }
         /* If input coords in bbox form, convert to full ... */
         if( in->csize == 6 ) {
            lcoords0 = make_umesh(in->shape[0],in->coords[0],in->coords[1]);
            lcoords1 = make_umesh(in->shape[1],in->coords[2],in->coords[3]);
            lcoords2 = make_umesh(in->shape[2],in->coords[4],in->coords[5]);
         } else {
            lcoords0 = copy_DVEC(in->coords,in->shape[0]);
            lcoords1 = copy_DVEC(in->coords+in->shape[0],in->shape[1]);
            lcoords2 = copy_DVEC(in->coords+in->shape[0]+in->shape[1],in->shape[2]);
         }
         imin = ixdvnearest(lcoords0,in->shape[0],bb[0]);
         imax = ixdvnearest(lcoords0,in->shape[0],bb[1]);
         jmin = ixdvnearest(lcoords1,in->shape[1],bb[2]);
         jmax = ixdvnearest(lcoords1,in->shape[1],bb[3]);
         kmin = ixdvnearest(lcoords2,in->shape[2],bb[4]);
         kmax = ixdvnearest(lcoords2,in->shape[2],bb[5]);
         if( Ltrace ) {
            fprintf(stderr,"imin=%d imax=%d jmin=%d jmax=%d kmin=%d kmax=%d\n",
               imin,imax,jmin,jmax,kmin,kmax);
         }
         nxout = (imin > -1 && imax > -1) ? (imax - imin + 1) : 0;
         nyout = (jmin > -1 && jmax > -1) ? (jmax - jmin + 1) : 0;
         nzout = (kmin > -1 && kmax > -1) ? (kmax - kmin + 1) : 0;
         if( nxout * nyout * nzout ) {
            out = (GFT *) malloc(sizeof(GFT));
            out->time    = in->time;
            out->version = in->version;
            out->rank    = 3;
            out->dsize   = nxout * nyout * nzout;
            out->csize   = nxout + nyout + nzout;
            out->pname   = in->pname ? strdup(in->pname) : (char *) NULL;
            out->cnames  = in->cnames ? strdup(in->cnames) : (char *) NULL;
            out->bbox    = (double *) NULL;
            out->tag     = in->tag ? strdup(in->tag) : (char *) NULL;
            out->shape   = (int *) malloc(3 * sizeof(int));
            out->shape[0] = nxout;
            out->shape[1] = nyout;
            out->shape[2] = nzout;
            out->coords   = (double *) malloc(out->csize * sizeof(double));
            out->data     = (double *) malloc(out->dsize * sizeof(double));
            for( k = kmin; k <= kmax; k++ ) {
               for( j = jmin; j <= jmax; j++ ) {
                  for( i = imin; i <= imax; i++ ) {
                     out->data[(k-kmin)*nyout*nxout+(j-jmin)*nxout+(i-imin)] = 
                        in->data[k*in->shape[1]*in->shape[0]+j*in->shape[0]+i];
                  }
               }
            }
            if( Ltrace ) fprintf(stderr,"clip_GFT: Clipped data.\n");
            for( i = imin; i <= imax; i++ ) {
               out->coords[i-imin] = lcoords0[i];
            }
            for( j = jmin; j <= jmax; j++ ) {
               out->coords[nxout+j-jmin] = lcoords1[j];
            }
            for( k = kmin; k <= kmax; k++ ) {
               out->coords[nxout+nyout+k-kmin] = lcoords2[k];
            }
            if( Ltrace ) fprintf(stderr,"clip_GFT: Clipped coords.\n");
         }
         if( lcoords0 ) free(lcoords0);
         if( lcoords1 ) free(lcoords1);
         if( lcoords2 ) free(lcoords2);
         break;
      default:
         fprintf(stderr,"clip_GFT: Unexpected rank=%d.\n",in->rank);
         break;
      }
   }
Return:
   return out;
}


int main(int argc, char *argv[]) {
   const char P[] = "sdffilter";
   GFT   D = {0, 0, 0, 0, 0, NULL, NULL, NULL, NULL, NULL, NULL, NULL};
   GFT   *pDclipped = (GFT *) NULL, *pD = (GFT *) NULL;
   int   c, errflg = 0, noptargs = 0; 
   int   i, rc;
   gft_sdf_file_data *gpin  = (gft_sdf_file_data *) NULL;
   gft_sdf_file_data *gpout = (gft_sdf_file_data *) NULL;

/* Argument parsing and checking */

   while ((c = getopt(argc, argv, "hi:b:n:")) != -1) {
      switch(c) {
      case 'h':
         goto Usage;
         break;
      case 'i':
         Sivec = strdup(optarg);
         noptargs += 2;
         break;
      case 'b':
         Sclipbbox = strdup(optarg);
         noptargs += 2;
         break;
      case 'n':
         Printname = strdup(optarg);
         noptargs += 2;
         break;
      default:
         errflg = 1;
         break;
      }
   }
   if( errflg ) goto Usage;

   if( argc == (noptargs + 3) ) {
      Infilename = strdup(argv[noptargs+1]);
      Outfilename = strdup(argv[noptargs+2]);
      if( !file_exists(Infilename) ) {
         fprintf(stderr,"%s: Input file '%s' does not exist.\n",P,Infilename);
         errflg = 1;
      }
      if( file_exists(Outfilename) ) {
         fprintf(stderr,"%s: Output file '%s' already exists.",P,Outfilename);
         fprintf(stderr," Remove explicitly.\n");
         errflg = 1;
      }
   } else {
      goto Usage;
   }
   if( errflg ) goto Exit; 

   if( Ltrace ) {
      fprintf(stderr,"%s: Infilename='%s'\n",P,Infilename);
      fprintf(stderr,"%s: Outfilename='%s'\n",P,Outfilename);
      fprintf(stderr,"%s: Printname ='%s'\n",P,Printname);
      fprintf(stderr,"%s: Sivec='%s'\n",P,Sivec ? Sivec : "NULL");
      fprintf(stderr,"%s: Sclipbbox='%s'\n",P,Sclipbbox ? Sclipbbox : "NULL");
   }

   if( !Sivec ) {
      Sivec = strdup("*");
   }
   if( Sclipbbox ) {
      string_to_bbox(Sclipbbox,&Clipbbox,&Clipbboxrank);
      if( Clipbboxrank ) {
         if( Ltrace ) {
            fprintf(stderr,"%s: Clipbboxrank=%d\n",P,Clipbboxrank);
            fprintf(stderr,"%s: Clipbbox=[",P);
            for( i = 0; i < 2 * Clipbboxrank; i++ ) 
               fprintf(stderr,"%g ",Clipbbox[i]);
            fprintf(stderr,"]\n");
         }
      } else {
         fprintf(stderr,
            "%s: Parsing of clipping bounding box argument '%s' failed.\n",
            P,Sclipbbox);
      }
   }

   reset_ivec();
   if( !(gpin = gft_open_sdf_file(Infilename)) ) {
      fprintf(stderr,"%s: Could not open '%s'\n",P,Infilename);
      goto Exit;
   }
   if( !(gpout = gft_create_sdf_stream(Outfilename)) ) {
      fprintf(stderr,"%s: Could not create '%s'\n",P,Outfilename);
      goto Exit;
   }

   i = 0;
   while ( low_read_sdf_stream(1,gpin->fp,
         &(D.time),&(D.version),&(D.rank),&(D.dsize),&(D.csize),&(D.pname),
         &(D.cnames),&(D.tag),&(D.shape),&(D.bbox),&(D.coords),&(D.data)) ) {
      i++;
      if( Ltrace ) {
         fprintf(stderr,"%s: Read rank-%d data set %d(%s) from %s.\n",
            P,D.rank,i,D.pname,Infilename);
         dump_GFT(stderr,&D,"In",0);
      }
      if( do_ivec(i,IVLEN,Ivec) ) {
         if( Clipbboxrank ) {
            if( Clipbboxrank != D.rank ) {
               fprintf(stderr,
                  "%s: Inconsistent data rank (%d) and clipping bounding box rank (%d)\n",
                  P,D.rank,Clipbboxrank);
                  goto Exit;
            } else {
               if( Ltrace ) {
                  fprintf(stderr,"%s: Clipping data set %d.\n",P,i);
               }
            }
            pD = (pDclipped = clip_GFT(&D,Clipbbox)) ? pDclipped : &D;
            if( Ltrace ) {
               dump_GFT(stderr,pD,"Clipped",0);
            }
         } else {
            pD = &D;
         }
         low_write_sdf_stream(gpout->fp,
            Printname ? Printname : pD->pname,pD->time,pD->rank,pD->dsize,pD->csize,
            pD->cnames,pD->tag,pD->shape,pD->coords,pD->data);
         if( Ltrace ) {
            dump_GFT(stderr,pD,"Post-low_write_sdf_stream",0);
         }
         if( pDclipped ) {
            free_GFT_data(pDclipped);
            pDclipped = (GFT *) NULL;
         }
      }
      free_GFT_data(&D);
   }
Exit:
   return 0;
Usage:
   fprintf(stderr,"usage: %s ...\n",P);
   fprintf(stderr,"          [-h]           Generate this message\n");
   fprintf(stderr,"          [-i ivec]      Data set selection index vector (1-based)\n");
   fprintf(stderr,"          [-b bbox]      Clipping bounding box\n");
   fprintf(stderr,"          [-n name]      Output grid function print name\n");
   fprintf(stderr,"          infile         Input .sdf file\n");
   fprintf(stderr,"          outfile        Output .sdf file\n\n");
   fprintf(stderr,"   Note:  Data is clipped to *input* data coordinates which\n");
   fprintf(stderr,"          are *nearest* to relevant bounding box elements.\n\n");
   fprintf(stderr,"   Examples:\n\n");
   fprintf(stderr,"      sdffilter -i \"1-*/2\" in.sdf out.sdf\n");
   fprintf(stderr,"      sdffilter -b \"[0.1 0.3]\" rank1-in.sdf rank1-out.sdf\n");
   fprintf(stderr,"      sdffilter -b \"[0.1 0.3 1.1 1.3]\" rank2-in.sdf rank2-out.sdf\n");
   fprintf(stderr,"      sdffilter -b \"[0.1 0.3 1.1 1.3 2.1 2.3]\" rank3-in.sdf rank3-out.sdf\n");

   return 1;
} 

