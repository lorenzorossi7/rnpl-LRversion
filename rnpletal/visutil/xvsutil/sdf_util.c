/* #include "project.h" */
#include "sdf_util.h"

/*======================================================================
   U t i l i t y    S D F   R o u t i n e s  
======================================================================*/

/*----------------------------------------------------------------------
Creates and returns new SDFDS. 
----------------------------------------------------------------------*/
PSDFDS new_SDFDS(void) {
   PSDFDS the;

   the = (PSDFDS) malloc(sizeof(SDFDS));

   the->time = 0.0;
   the->version = SDFVERSION;
   the->rank = 0;
   the->dsize = 0;
   the->csize = 0;
   the->pname = NULL;
   the->cnames = NULL; 
   the->tag = NULL;
   the->shape = NULL; 
   the->bbox = NULL; 
   the->coords = NULL; 
   the->data = NULL;

   return the;
}

/*----------------------------------------------------------------------
Frees storage associated with SDFDS (including that allocated 
via low_read_sdf_stream(). 
----------------------------------------------------------------------*/
PSDFDS free_SDFDS(PSDFDS the) {
#define FINN(a) if( the->a ) free(the->a)
   if( the ) {
      FINN(pname); 
      FINN(cnames); 
      FINN(tag);
      FINN(shape); 
      FINN(bbox); 
      FINN(coords); 
      FINN(data);

      free(the);
   }

   return NULL;
#undef  FINN
}

/*----------------------------------------------------------------------
Creates "dummy" SDFDS whose printname is to be used a message. 
Server-side detects via rank == -1.
----------------------------------------------------------------------*/
PSDFDS message_SDFDS(char *message) {
   PSDFDS the = (PSDFDS) NULL;

   if( message != NULL ) {
      the = new_SDFDS();
      the->rank = -1;
      the->pname = strdup(message);
   }

   return the;
}

/*----------------------------------------------------------------------
Reads SDFDS from stream *fp; low_read_sdf_stream() allocates 
memory for non-statically-sized DS components.  
 
Returns 1 on success, 0 on failure.
----------------------------------------------------------------------*/
int read_SDFDS(FILE *fp, PSDFDS the) {
   int rc = 1;
   int ltrace = OFF;

   if( fp ) {
      rc = low_read_sdf_stream(1,fp,
               &(the->time),
               &(the->version),
               &(the->rank),
               &(the->dsize),
               &(the->csize),
               &(the->pname),
               &(the->cnames),
               &(the->tag),
               &(the->shape),
               &(the->bbox),
               &(the->coords),
               &(the->data)
           );
      if( ltrace ) {
         fprintf(stderr,"read_SDFDS: the->pname='%s'\n",
               the->pname ? the->pname : "NULL");
      }
   }

   return rc;
}

/*----------------------------------------------------------------------
Writes SDFDS to stream *fp.
   
Returns 1 on success, 0 on failure.
----------------------------------------------------------------------*/
int write_SDFDS(FILE *fp, PSDFDS the) {
   int rc = 1;
   int ltrace = OFF;
   
   if( fp && the ) {
      if( ltrace ) {
         fprintf(stderr,"write_SDFDS: the->pname='%s'\n",the->pname);
      }
      rc = low_write_sdf_stream(fp,
               the->pname,
               the->time,
               the->rank,
               the->dsize,
               the->csize,
               the->cnames,
               the->tag,
               the->shape,
               the->coords,
               the->data
           );
   }

   return rc;
}

/*----------------------------------------------------------------------
Creates new SDFDS, then writes to stream *fp; completion code 
(0/1 for success/faliure) returned in *prc.
----------------------------------------------------------------------*/
PSDFDS new_read_SDFDS(FILE *fp, int *prc) {
   PSDFDS the = (PSDFDS) NULL;

   if( fp ) {
      the  = new_SDFDS();
      *prc = read_SDFDS(fp,the);
   }

   return the;
}

/*----------------------------------------------------------------------
Returns x-y limits of SDFDS.
----------------------------------------------------------------------*/
DQUAD dquad_SDFDS(PSDFDS the) {
   DQUAD dq = {0.0, 1.0, 0.0, 1.0};
   if( the ) {
      if( the->rank == 1 ) {
         dq.x1 = l_dvmin(the->coords,the->csize);
         dq.x2 = l_dvmax(the->coords,the->csize);
         dq.y1 = l_dvmin(the->data,the->dsize);
         dq.y2 = l_dvmax(the->data,the->dsize);
      } 
   }
   return dq;
}

int MAXDUMP = 100;
/*----------------------------------------------------------------------
Dumps SDFDS on stream *fp.
----------------------------------------------------------------------*/
void dump_SDFDS(FILE *fp, PSDFDS the, char *label, int option) {

   if( !the ) return;

   fprintf(fp,"dump_SDFDS: %s BEGIN\n",label);
   fprintf(fp,"   pname: %s\n",the->pname);
   fprintf(fp,"   time: %g\n",the->time);
   fprintf(fp,"   version: %d\n",the->version);
   fprintf(fp,"   rank: %d\n",the->rank);
   fprintf(fp,"   dsize: %d\n",the->dsize);
   fprintf(fp,"   csize: %d\n",the->csize);
   fprintf(fp,"   cnames: %s\n",the->cnames ? the->cnames : "NULL");
   fprintf(fp,"   tag: %s\n",the->tag ? the->tag : "NULL");
   if( option > 0 && the->rank > 0 ) {
      l_ivfdump(fp,the->shape,the->rank,"shape");
      l_dvfdump(fp,the->bbox,2*the->rank,"bbox");
      if( the->dsize <= MAXDUMP ) {
         l_dvfdump(fp,the->coords,the->csize,"coords");
         l_dvfdump(fp,the->data,the->dsize,"data");
      } else {
         fprintf(stderr,"   Suppressing full coordinate/data dump\n");
         fprintf(stderr,"   Coords: %g -- %g\n",
            the->coords[0],the->coords[the->csize-1]);
         fprintf(stderr,"   Data: %g -- %g\n",
            the->data[0],the->data[the->dsize-1]);
      }
   }
   fprintf(fp,"dump_SDFDS: %s END\n",label);
}

/*----------------------------------------------------------------------
Generate 1-d SDFDS for testing purposes.
----------------------------------------------------------------------*/
PSDFDS test_SDFDS_1d(char *name, int n) {
   PSDFDS the;
   int    i;
   double dx;

   the = new_SDFDS();
   if( the ) {
      the->time = 0.0;
      the->version = SDFVERSION;
      the->rank = 1;
      the->dsize = n;
      the->csize = n;
      the->pname = strdup(name);
      the->cnames = strdup("x");
      the->tag = strdup("");
      the->shape = (int *) malloc(sizeof(int));
      the->shape[0] = n;
      the->bbox = (double *) malloc(2 * sizeof(double));
      the->bbox[0] = -1.0;
      the->bbox[1] =  n > 1 ? 1.0 : -1.0;
      the->coords = (double *) malloc(n * sizeof(double));
      the->data   = (double *) malloc(n * sizeof(double));
      the->coords[0] = -1.0;
      the->data[0] = (the->coords[0])*(the->coords[0]);
      if( n > 1 ) {
         dx = 2.0 / (n - 1);
         for( i = 1; i < n; i++ ) {
            the->coords[i] = the->coords[i-1] + dx;
            the->data[i] = (the->coords[i])*(the->coords[i]);
         }
      }
   }
   return the;
}

/* Vector minmum and maximum. */ 

double l_dvmin(r_DVEC v,r_int n) {
   r_double   temp;
   r_int      i;

   temp = v[0];
   for( i = 1; i < n; i++ ) {
      if( v[i] < temp )
         temp = v[i];
   }
   return(temp);
}

double l_dvmax(r_DVEC v,r_int n) {
   r_double   temp;
   r_int      i;

   temp = v[0];
   for( i = 1; i < n; i++ ) {
      if( v[i] > temp )
         temp = v[i];
   }
   return(temp);
}

void l_ivfdump(FILE *fp,IVEC v,int n,char *s)
{
   int     i, per_line = 6;

   if( v != NULL ) {
      fprintf(fp,"<<< %s >>>\n\n",s);
      for( i = 0; i < n; i++ ) {
         fprintf(fp, ((i + 1) % per_line ? "%12i" : "%12i\n"),v[i]);
      }
      fprintf(fp, (n % per_line) ? "\n\n" : "\n" );
   }
}

void l_dvfdump(FILE *fp,DVEC v,int n,char *s) {
   int     i, per_line = 4;

   if( v != NULL ) {
      fprintf(fp,"<<< %s >>>\n\n",s);
      for( i = 0; i < n; i++ ) {
         fprintf(fp, ((i + 1) % per_line ? "%19.10E" : "%19.10E\n"),v[i]);
      }
      fprintf(fp, (n % per_line) ? "\n\n" : "\n" );
   }
}

void l_dvvfscanf(FILE *fp, double **pv1, double **pv2, int *pn) {
   static char R[] = "l_dvvfscanf";
   fpos_t   pos;
   double   v1i,    v2i;
   int      i = 0;

   int      ltrace = OFF;

   *pv1 = (double *) NULL;
   *pv2 = (double *) NULL;
   *pn  = 0;

   if( fp ) {
      while( fscanf(fp,"%lf %lf",&v1i,&v2i) == 2 ) {
         if( ltrace ) fprintf(stderr,"%s: i=%d v1i=%g v2i=%g\n",R,i,v1i,v2i);

         if( !(*pv1 = (double *) realloc(*pv1,(i + 1) * sizeof(double))) ) {
            fprintf(stderr,"%s: Can't realloc %d doubles\n",R,i+1);
         }
         if( !(*pv2 = (double *) realloc(*pv2,(i + 1) * sizeof(double))) ) {
            if( *pv1 ) free(*pv1);
            fprintf(stderr,"%s: Can't realloc %d doubles\n",R,i+1);
         }

         (*pv1)[i] = v1i;
         (*pv2)[i] = v2i;

         i++;
      }
   }
   *pn = i;
   if( ltrace ) fprintf(stderr,"%s: n=%d\n",R,*pn);
}

/* For use in lieu of gft_read_rank() which scans entire file ... */

int gft_read_rank_first(const char *gf_name, int *prank) {
   static char         R[] = "gft_read_rank_first";
   const  int          ncheck = 100;
   int                 ltrace = OFF;
   int                 rval = 0; 
   gft_sdf_file_data  *gp;
   PSDFDS              the = new_SDFDS();

   *prank = 0;

   if( ltrace ) {
      fprintf(stderr,"%s: gf_name=%s\n",R,gf_name ? gf_name : "NULL");
   }
   if( (gp = gft_open_sdf_stream(gf_name)) ) {
      if( read_SDFDS(gp->fp,the) ) {
         *prank = the->rank;
         free_SDFDS(the);
         rval = 1;
      }
      gft_close_sdf_stream(gf_name);
   }
   if( ltrace ) {
      fprintf(stderr,"%s: *prank=%d rval=%d\n",R,*prank,rval);
   }

   return rval;
}
