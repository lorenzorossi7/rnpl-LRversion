#include "sdfslice_util.h"
#include "d3lib.h" //LR - modern compilers seem to require this or it won't recognise function d3slice_

/*----------------------------------------------------------------------
 * Slices data in 'in' according to ivec and/or fvec specificiations 
 * and returns pointer to sliced data.
 *
 * Hardwires "x|y|z" for coordinate names.
 *--------------------------------------------------------------------*/
GFT *slice_GFT(GFT *in, 
   char *xiv_s, char *yiv_s, char *ziv_s, 
   char *xfv_s, char *yfv_s, char *zfv_s, double fuzz) {

   char   R[] = "slice_GFT";
   GFT   *out = (GFT *) NULL;
   int    ltrace = Ltrace; 

   int    innx, inny, innz;
   DVEC   inxc = (DVEC) NULL, inyc = (DVEC) NULL, inzc = (DVEC) NULL;

   int    outnx = 0, outny = 0, outnz = 0;
   IVEC   xsl = (IVEC) NULL, ysl = (IVEC) NULL, zsl = (IVEC) NULL;
   int    outsize = 0, outrank = 0;

   int    f77_one = 1;

   int    i,  rc;

   /* Cowardly handle data of different ranks using separate code ... */ 
   switch( in->rank ) {

   /*-------------------------------------------------------------------
    * RANK 1 ...    
    *-----------------------------------------------------------------*/
   case 1:
      innx = (in->shape)[0];
      if( ltrace ) {
         fprintf(stderr,
            "%s: Rank-1 input data. Data size=%d Coord size=%d\n",
               R,in->dsize,in->csize);
      }
      if( in->csize == 2 * in->rank ) {
         /* Input coordinates specified via bounding box ... */
         inxc = Dvumsh(innx,(in->coords)[0],(in->coords[1]));
      } else {
         /* Input coordinates specified explicitly ... */
         inxc = Dvcopy(in->coords,innx);
      }
      if( ltrace > 1 ) {
         dvdump(inxc,innx,"inxc");
      }

      /* Determine vectors of slice indices ...*/ 
      if( xiv_s ) {
         rc = ivecstrtoiseq(xiv_s,1,innx,&xsl,&outnx);
      } else if( xfv_s ) {
         rc = fvecstrdvtoiseq(xfv_s,inxc,innx,fuzz,&xsl,&outnx);
      } else {
         fprintf(stderr,"%s: Both 'xiv_s' and 'xfv_s' are null.\n",R);
         goto Return;
      }
      if( ltrace ) {
         fprintf(stderr,"%s: outnx=%d\n",R,outnx);
         if( ltrace > 1 ) ivdump(xsl,outnx,"xsl");
      }
      outrank = 1;
      outsize = outnx;
      if( ltrace ) {
         fprintf(stderr,"%s: outrank=%d outsize=%d\n",R,outrank,outsize);
      }
      if( outsize == 0 ) goto Return;

      /* Allocate and define the output GFT ... */
      if( !(out = (GFT *) malloc(sizeof(GFT))) ) {
         fprintf(stderr,"%s: malloc(%d) failed.\n",R,sizeof(GFT));
         goto Return;
      }
      out->time    = in->time;
      out->version = in->version;
      out->rank    = outrank;
      out->dsize   = outsize;
      out->pname   = in->pname ? strdup(in->pname) : (char *) NULL;
      out->tag     = in->tag ? strdup(in->tag) : (char *) NULL;
      out->shape   = (int *) malloc(outrank * sizeof(int));
      out->shape[0] = outnx;
      out->bbox    = (DVEC) NULL;

      out->cnames = strdup("x");
      out->csize = outnx;
      out->coords  = (double *) malloc(out->csize * sizeof(double));
      for( i = 0; i < outnx; i++ ) {
         (out->coords)[i] = inxc[xsl[i]-1];
      }
      if( ltrace > 1 ) {
         dvdump(out->coords,out->csize,"out->coords (x)");
      }

      out->data     = (double *) malloc(out->dsize * sizeof(double));
      (void) d3slice_(in->data,out->data,&innx,&f77_one,&f77_one,
         &outnx,&f77_one,&f77_one,xsl,&f77_one,&f77_one);
      if( ltrace > 1 ) {
         d3dump(out->data,outnx,f77_one,f77_one,"out->data",0);
      }
      break;

   /*-------------------------------------------------------------------
    * RANK 2 ...    
    *-----------------------------------------------------------------*/
   case 2:
      innx = (in->shape)[0];
      inny = (in->shape)[1];
      if( ltrace ) {
         fprintf(stderr,
            "%s: Rank-2 input data. Data size=%d Coord size=%d fuzz=%lf\n",
               R,in->dsize,in->csize,fuzz);
      }
      if( in->csize == 2 * in->rank ) {
         /* Input coordinates specified via bounding box ... */
         inxc = Dvumsh(innx,(in->coords)[0],(in->coords[1]));
         inyc = Dvumsh(inny,(in->coords)[2],(in->coords[3]));
      } else {
         /* Input coordinates specified explicitly ... */
         inxc = Dvcopy(in->coords,innx);
         inyc = Dvcopy((in->coords) + innx,inny);
      }
      if( ltrace > 1 ) {
         dvdump(inxc,innx,"inxc");
         dvdump(inyc,inny,"inyc");
      }

      /* Determine vectors of slice indices ...*/ 
      if( xiv_s ) {
         rc = ivecstrtoiseq(xiv_s,1,innx,&xsl,&outnx);
      } else if( xfv_s ) {
         rc = fvecstrdvtoiseq(xfv_s,inxc,innx,fuzz,&xsl,&outnx);
      } else {
         fprintf(stderr,"%s: Both 'xiv_s' and 'xfv_s' are null.\n",R);
         goto Return;
      }
      if( ltrace ) {
         fprintf(stderr,"%s: outnx=%d\n",R,outnx);
         if( ltrace > 1 ) ivdump(xsl,outnx,"xsl");
      }
      if( yiv_s ) {
         rc = ivecstrtoiseq(yiv_s,1,inny,&ysl,&outny);
      } else if( yfv_s ) {
         rc = fvecstrdvtoiseq(yfv_s,inyc,inny,fuzz,&ysl,&outny);
      } else {
         fprintf(stderr,"%s: Both 'yiv_s' and 'yfv_s' are null.\n",R);
         goto Return;
      }
      if( ltrace ) {
         fprintf(stderr,"%s: outny=%d\n",R,outny);
         if( ltrace > 1 ) ivdump(ysl,outny,"ysl");
      }
      outrank = in->rank - ((outnx == 1) || (outny == 1));
      outsize = outnx * outny;
      if( ltrace ) {
         fprintf(stderr,"%s: outrank=%d outsize=%d\n",R,outrank,outsize);
      }
      if( outsize == 0 ) goto Return;

      /* Allocate and define the output GFT ... */
      if( !(out = (GFT *) malloc(sizeof(GFT))) ) {
         fprintf(stderr,"%s: malloc(%d) failed.\n",R,sizeof(GFT));
         goto Return;
      }
      out->time    = in->time;
      out->version = in->version;
      out->rank    = outrank;
      out->dsize   = outsize;
      out->pname   = in->pname ? strdup(in->pname) : (char *) NULL;
      out->tag     = in->tag ? strdup(in->tag) : (char *) NULL;
      out->shape   = (int *) malloc(outrank * sizeof(int));
      out->bbox    = (DVEC) NULL;

      switch( out->rank ) {
      case 1:
         if( outnx >= outny ) {
            out->shape[0] = outnx;
            out->cnames = strdup("x");
            out->csize = outnx;
            out->coords  = (double *) malloc(out->csize * sizeof(double));
            for( i = 0; i < outnx; i++ ) {
               (out->coords)[i] = inxc[xsl[i]-1];
            }
            if( ltrace > 1 ) {
               dvdump(out->coords,out->csize,"out->coords (x)");
            }
         } else {
            out->shape[0] = outny;
            out->cnames = strdup("y");
            out->csize = outny;
            out->coords  = (double *) malloc(out->csize * sizeof(double));
            dvcopy(inyc,out->coords,outny);
            for( i = 0; i < outny; i++ ) {
               (out->coords)[i] = inyc[ysl[i]-1];
            }
            if( ltrace > 1 ) {
               dvdump(out->coords,out->csize,"out->coords (y)");
            }
         }
         break;
      case 2:
         out->shape[0] = outnx;
         out->shape[1] = outny;
         out->cnames = strdup("x|y");
         out->csize = outnx + outny;
         out->coords  = (double *) malloc(out->csize * sizeof(double));
         for( i = 0; i < outnx; i++ ) {
            (out->coords)[i] = inxc[xsl[i]-1];
         }
         for( i = 0; i < outny; i++ ) {
            (out->coords)[i+outnx] = inyc[ysl[i]-1];
         }
         if( ltrace > 1 ) {
            dvdump(out->coords,outnx,"out->coords (x)");
            dvdump(out->coords+outnx,outny,"out->coords (y)");
         }
         break;
      default:
         break;
      }
      out->data     = (double *) malloc(out->dsize * sizeof(double));
      (void) d3slice_(in->data,out->data,&innx,&inny,&f77_one,
         &outnx,&outny,&f77_one,xsl,ysl,&f77_one);
      if( ltrace > 1 ) {
         d3dump(out->data,outnx,outny,f77_one,"out->data",0);
      }

      break;
   /*-------------------------------------------------------------------
    * RANK 3 ...    
    *-----------------------------------------------------------------*/
   case 3:
      innx = (in->shape)[0];
      inny = (in->shape)[1];
      innz = (in->shape)[2];
      if( ltrace ) {
         fprintf(stderr,
            "%s: Rank-3 input data. Data size=%d Coord size=%d\n",
               R,in->dsize,in->csize);
      }
      break;
   default:
      fprintf(stderr,"%s: Unexpected data rank -- in->rank=%d\n",R,in->rank);
      goto Return;
      break;
   }

Return:
   if( inxc ) free(inxc);
   if( inyc ) free(inyc);
   if( inzc ) free(inzc); 

   return(out);
}

/*----------------------------------------------------------------------
 * Allocates and returns int vector, '*piseq', and length thereof, 
 * '*piseqlen', of integers from 'ivmin' to 'ivmax' inclusive that 
 * are elements of index vector 'iv'.
 *--------------------------------------------------------------------*/
int ivectoiseq(int *iv, int ivlen, int ivmin, int ivmax, 
               int **piseq, int *piseqlen) {

   int   i, iseqlen = 0;
   int  *liv = (int *) NULL;

   *piseq = (int *) NULL;
   *piseqlen = 0;

   if( ivlen <= 0 ) return(0);

   /* do_ivec() modifies IVEC, so always work on local copy ...*/
   liv = Ivcopy(iv,ivlen);
   for( i = ivmin, iseqlen = 0; i <= ivmax; i++ ) {
      if( do_ivec(i,ivlen,liv) ) {
         iseqlen++;
      }
   }
   if( liv ) free(liv);

   if( iseqlen ) {
      if( ((*piseq) = make_IVEC(iseqlen)) ) { 
         liv = Ivcopy(iv,ivlen);
         for( i = ivmin, iseqlen = 0; i <= ivmax; i++ ) {
            if( do_ivec(i,ivlen,liv) ) { 
               (*piseq)[iseqlen++] = i; 
            }
         }
         if( liv ) free(liv);
      } else {
         fprintf(stderr,"ivectoiseq: make_IVEC(%d) failed.\n",iseqlen);
      }
   }
   *piseqlen = iseqlen;
   return(*piseqlen);
}

/*----------------------------------------------------------------------
 * Allocates and returns int vector, '*piseq', and length thereof, 
 * '*piseqlen', of integers from 'ivmin' to 'ivmax' inclusive that 
 * are elements of string form of index vector 'ivstr'.
 *--------------------------------------------------------------------*/
int ivecstrtoiseq(char *ivstr, int ivmin, int ivmax, 
                  int **piseq, int *piseqlen) {
   int  *iv = (int *) NULL;
   char  buffer[SDFSLICE_BUFLEN];
   int   rc = 0;

   *piseq = (int *) NULL;
   *piseqlen = 0;

   iv = make_IVEC(SDFSLICE_IVLEN);
   if( !iv) {
      fprintf(stderr,"ivecstrtoiseq: make_IVEC(%d) failed.\n",SDFSLICE_IVLEN);
      return(rc);
   }

   sprintf(buffer,"iv:=%s",ivstr);
   if( sget_ivec_param(buffer,"iv",iv,SDFSLICE_IVLEN) != 1 ) {
      fprintf(stderr,"ivecstrtoiseq: Error parsing ivec specification <%s>\n",
         buffer);
      return(rc);
   }

   fixup_ivec(ivmin,ivmax,0,iv);
   rc = ivectoiseq(iv,SDFSLICE_IVLEN,ivmin,ivmax,piseq,piseqlen);
   if( iv ) free(iv);

   return(rc);
}

/*----------------------------------------------------------------------
 * Converts string form of fvec specification to vector of doubles,
 * '*pdseq', also returning length of vector '*pdseqlen'.
 *--------------------------------------------------------------------*/
int fvecstrtodseq(char *fvstr, double **pdseq, int *pdseqlen) {
   double  *fv = (double *) NULL;
   char     buffer[SDFSLICE_BUFLEN];
   int      i, ldseqlen;
   int      rc = 0;

   int      ltrace = Ltrace;

   *pdseq = (DVEC) NULL;
   *pdseqlen = 0;

   fv = make_DVEC(SDFSLICE_FVLEN);
   if( !fv) {
      fprintf(stderr,"fvecstrtodseq: make_DVEC(%d) failed.\n",SDFSLICE_FVLEN);
      return(rc);
   }

   sprintf(buffer,"fv:=%s",fvstr);
   if( sget_fvec_param(buffer,"fv",fv,SDFSLICE_FVLEN) != 1 ) {
      fprintf(stderr,"fvecstrtodseq: Error parsing fvec specification <%s>\n",
         buffer);
      return(rc);
   }
   ldseqlen = size_fvec(fv);

   if( ltrace ) {
      fprintf(stderr,"fvecstrtodseq: ldseqlen=%d\n",ldseqlen);
   }
   if( ldseqlen <= 0 ) return(rc);

   (*pdseq) = make_DVEC(ldseqlen);
   if( !(*pdseq) ) {
      fprintf(stderr,"fvecstrtodseq: make_DVEC(%d) failed.\n",ldseqlen);
      return(rc);
   }
   *pdseqlen = ldseqlen;

   for( i = 0; i < ldseqlen; i++ ) {
      (*pdseq)[i] = get_fvec(fv,i);
   }
   if( fv ) free(fv);
   
   rc = *pdseqlen;

   return(rc);
}
/*----------------------------------------------------------------------
 * Allocates and returns int vector, '*piseq', and length thereof, 
 * '*piseqlen', of 1-based indices into array 'dv' whose corresponding
 * elements are members of string form of index vector 'fvstr'.
 *--------------------------------------------------------------------*/
int fvecstrdvtoiseq(char *fvstr, double *dv, int dvlen, double fuzz,
                    int **piseq, int *piseqlen) {
   double  *fv = (double *) NULL, *lfv = (double *) NULL,
           *dseq = (double *) NULL;
   double   key;
   char     buffer[SDFSLICE_BUFLEN];
   int      i, ix, fvlen, liseqlen;
   int      rc = 0;

   int      ltrace = Ltrace;

   *piseq = (int *) NULL;
   *piseqlen = 0;

   fv = make_DVEC(SDFSLICE_FVLEN);
   if( !fv) {
      fprintf(stderr,"fvecstrdvtoiseq: make_DVEC(%d) failed.\n",SDFSLICE_FVLEN);
      return(rc);
   }

   sprintf(buffer,"fv:=%s",fvstr);
   if( sget_fvec_param(buffer,"fv",fv,SDFSLICE_FVLEN) != 1 ) {
      fprintf(stderr,"fvecstrdvtoiseq: Error parsing fvec specification <%s>\n",
         buffer);
      return(rc);
   }
   fvlen = size_fvec(fv);
   if( ltrace ) {
      fprintf(stderr,"fvecstrdvtoiseq: fvlen=%d fuzz=%g\n",fvlen,fuzz);
      if( ltrace > 1 ) dvdump(dv,dvlen,"dv");
   }
   if( fvlen <= 0 ) return(rc);

   lfv = Dvcopy(fv,SDFSLICE_FVLEN);
   for( i = 0, liseqlen = 0; i < fvlen; i++ ) {
      key = get_fvec(lfv,i);
      if( ltrace ) {
         fprintf(stderr,"fvecstrdvtoiseq: Looking for %g Fuzz=%g\n",key,fuzz);
      }
      if( dvlookup_linear(dv,dvlen,key,fuzz) >= 0 ) liseqlen++;
   }
   if( ltrace ) {
      fprintf(stderr,"fvecstrdvtoiseq: liseqlen=%d\n",liseqlen);
   }
   if( lfv ) free(lfv);
   
   *piseqlen = liseqlen;
   rc = *piseqlen;

   if( (*piseqlen) == 0 ) return(rc);

   (*piseq) = make_IVEC(liseqlen);
   lfv = Dvcopy(fv,SDFSLICE_FVLEN);
   for( i = 0, liseqlen = 0; i < fvlen; i++ ) {
      key = get_fvec(lfv,i);
      if( (ix = dvlookup_linear(dv,dvlen,key,fuzz)) >= 0 ) {
         (*piseq)[liseqlen++] = ix + 1;
      }
   }

   if( dseq ) free(dseq);
   if( lfv ) free(lfv);
   if( fv ) free(fv);

   return(rc);
}

/*======================================================================
 * Utility routines from vutil/{dv.c,iv.c} (and other places).
 *
 * Included here so to minimize number of MWC-specific libraries on
 * which RNPL depends.
 *====================================================================*/

DVEC make_DVEC(r_int n) {
   return (DVEC) malloc(n * sizeof(double));
}

DVEC Dvcopy(r_DVEC v1,r_int n) {
   r_DVEC  v2;
   r_int   i; 
   
   if( (v2 = make_DVEC(n)) != NULL ) {
      for( i = 0; i < n; i++ ) {
         v2[i] = v1[i];
      }
   } else {
      printf("Dvcopy: copy failed.\n");
   }
   return v2;
}

DVEC Dvumsh(int n,double v0,double vnm1) {
   DVEC   v = (DVEC) NULL;

   if( n > 0 ) {
      if( v = make_DVEC(n) ) {
         if( n == 1 ) {
            v[0] = v0;
         } else {
            dvramp(v,v0,(vnm1 - v0) / (n - 1),n);
            v[n-1] = vnm1;
         }
      } else {
         fprintf(stderr,"Dvumsh: make_DVEC(%d) failed\n",n);
      }
   }

   return v;
}

void dvcopy(DVEC v1, DVEC v2, int n) {
   int i;

   if( n > 0 ) {
      for( i = 0; i < n; i++ ) v2[i] = v1[i];
   }

}

void dvramp(DVEC v,double v0,double dv,int n) {
   int       j;

   if( n > 0 ) {
      v[0] = v0;
      for( j = 1; j < n; j++ ) {
         v[j] = v[j-1] + dv;
      }
   }
}

int dvlookup_linear(DVEC v,int n,double vkey,double fuzz) {
   int    j;
   for( j = 0; j < n; j++ ) {
      if( eq_fuzz(v[j],vkey,fuzz) ) return j;
   }
   return -1;
}

int eq_fuzz(double x1,double x2,double fuzz) {

   if(        x1 == 0.0 ) {
      return fabs(x2) <= fuzz;
   } else if( x2 == 0.0 ) {
      return fabs(x1) <= fuzz;
   } else {
      return (fabs(x1 - x2) / fabs(x1)) <= fuzz;
   }

}

void dvdump(DVEC v,int n,char *s) {
   int     i, per_line = 4;

   if( v != NULL ) {
      printf("<<< %s >>>\n\n",s);
      for( i = 0; i < n; i++ ) {
         printf( ((i + 1) % per_line ? "%19.10E" : "%19.10E\n"),v[i]);
      }
      printf( (n % per_line) ? "\n\n" : "\n" );
   }
}

IVEC make_IVEC(r_int n) {
   return (IVEC) malloc(n * sizeof(int));
}

IVEC Ivcopy(r_IVEC v1,r_int n) {
   r_IVEC  v2;
   r_int   i; 
   
   if( (v2 = make_IVEC(n)) != NULL ) {
      for( i = 0; i < n; i++ ) {
         v2[i] = v1[i];
      }
   } else {
      printf("ivcopy: copy failed.\n");
   }
   return v2;
}
 
void ivdump(IVEC v,int n,char *s)
{
   int     i, per_line = 6;

   if( v != NULL ) {
      printf("<<< %s >>>\n\n",s);
      for( i = 0; i < n; i++ ) {
         printf( ((i + 1) % per_line ? "%12i" : "%12i\n"),v[i]);
      }
      printf( (n % per_line) ? "\n\n" : "\n" );
   }
}

int ivlookup_linear(IVEC v,int n,int vkey) {
   int    j;
   for( j = 0; j < n; j++ ) {
      if( v[j] == vkey ) return j;
   }
   return -1;
}

IVEC Ivseq(int iv1, int n) {
   IVEC  v = (IVEC) NULL;
   int   i;

   if( (v = make_IVEC(n)) != NULL ) {
      for( i = 0; i < n; i++ )
         v[i] = iv1 + i;
   } else {
      fprintf(stderr,"Ivseq: make_IVEC(%d) failed.\n",n);
   }
   return v;
}

void d3dump(double *a, int d1, int d2, int d3, char *label, int unit) {
   d3dump_(a,&d1,&d2,&d3,label,&unit,strlen(label));
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
