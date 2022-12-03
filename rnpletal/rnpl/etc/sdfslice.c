#include <stdlib.h>
#include <math.h>
#include <string.h>
#include <unistd.h>
#include <stdio.h>
#include <libgen.h>
#include <sdf_priv.h>
#include <sdf.h>
#include "sdfslice_util.h"  

#define    ON       1
#define    OFF      0
#define    BUFLEN   1024
#define    IVLEN    1024
#define    NTMAX    1024*1024
#define    MAXROW   12
#define    MAXCOL   12
#define    MAXPLOT  144  

const char P[] = "sdfslice";

int        Ltrace = OFF;

char       B[BUFLEN];

int        Errflg = 0;

char      *Infilename = (char *) NULL;
char      *Outfilename = (char *) NULL;

char      *Tiv_s = (char *) NULL;
char      *Xiv_s = (char *) NULL;
char      *Yiv_s = (char *) NULL;
char      *Ziv_s = (char *) NULL;
char      *Tfv_s = (char *) NULL;
char      *Xfv_s = (char *) NULL;
char      *Yfv_s = (char *) NULL;
char      *Zfv_s = (char *) NULL;

IVEC       Tiv = (IVEC) NULL;
int        Tiv_len;
DVEC       Tfv = (DVEC) NULL;
int        Tfv_len;

int        Inrank = -1;
int        Inngft = -1;

double     Fuzz = 1.0e-8;

void argerr(char *mess) {
   fprintf(stderr,"%s: Argument error -- %s\n",P,mess);
   Errflg = 1;
}

int main(int argc, char *argv[]) {
   GFT   D = {0, 0, 0, 0, 0, NULL, NULL, NULL, NULL, NULL, NULL, NULL};
   GFT   *pDsliced = (GFT *) NULL, *pD = (GFT *) NULL;
   int   c, errflg = 0, noptargs = 0; 
   int   i, rc;
   gft_sdf_file_data *gpin  = (gft_sdf_file_data *) NULL;
   gft_sdf_file_data *gpout = (gft_sdf_file_data *) NULL;

   /* Argument parsing ... */
   while ((c = getopt(argc, argv, "hvV0:1:2:3:t:x:y:z:")) != -1) {
      switch(c) {
      case 'h':
         goto Usage;
         break;
      case 'v':
         Ltrace = 1;
         noptargs += 1;
         break;
      case 'V':
         Ltrace = 2;
         noptargs += 1;
         break;
      case '0':
         Tiv_s ? argerr("'-0' option is repeated") : (Tiv_s = strdup(optarg));
         noptargs += 2;
         break;
      case '1':
         Xiv_s ? argerr("'-1' option is repeated") : (Xiv_s = strdup(optarg));
         noptargs += 2;
         break;
      case '2':
         Yiv_s ? argerr("'-2' option is repeated") : (Yiv_s = strdup(optarg));
         noptargs += 2;
         break;
      case '3':
         Ziv_s ? argerr("'-3' option is repeated") : (Ziv_s = strdup(optarg));
         noptargs += 2;
         break;
      case 't':
         Tfv_s ? argerr("'-t' option is repeated") : (Tfv_s = strdup(optarg));
         noptargs += 2;
         break;
      case 'x':
         Xfv_s ? argerr("'-x' option is repeated") : (Xfv_s = strdup(optarg));
         noptargs += 2;
         break;
      case 'y':
         Yfv_s ? argerr("'-y' option is repeated") : (Yfv_s = strdup(optarg));
         noptargs += 2;
         break;
      case 'z':
         Zfv_s ? argerr("'-z' option is repeated") : (Zfv_s = strdup(optarg));
         noptargs += 2;
         break;
      default:
         Errflg = 1;
         break;
      }
   }
   /* Check for argument incompatibilities ... */
   if( Tiv_s && Tfv_s ) argerr("Cannot specify both '-0' and '-t'");
   if( Xiv_s && Xfv_s ) argerr("Cannot specify both '-1' and '-x'");
   if( Yiv_s && Yfv_s ) argerr("Cannot specify both '-2' and '-y'");
   if( Ziv_s && Zfv_s ) argerr("Cannot specify both '-3' and '-z'");

   if( Errflg ) goto Usage;

   /* Check that input file exists and that output file does not ... */
   if( argc == (noptargs + 3) ) {
      Infilename = strdup(argv[noptargs+1]);
      Outfilename = strdup(argv[noptargs+2]);
      if( !file_exists(Infilename) ) {
         fprintf(stderr,"%s: Input file '%s' does not exist.\n",P,Infilename);
         Errflg = 1;
      }
      if( file_exists(Outfilename) ) {
         fprintf(stderr,"%s: Output file '%s' already exists.",P,Outfilename);
         fprintf(stderr," Remove explicitly.\n");
         sprintf(B,"/bin/rm -f %s",Outfilename);
         fprintf(stderr,"%s\n",B);
         Errflg = 1;
      }
   } else {
      goto Usage;
   }

   if( Ltrace ) {
      fprintf(stderr,"%s: Infilename='%s'\n",P,Infilename);
      fprintf(stderr,"%s: Outfilename='%s'\n",P,Outfilename);
      fprintf(stderr,"%s: Preliminary index vectors ---\n",P);
      fprintf(stderr,"%s: Tiv_s='%s'\n",P,Tiv_s ? Tiv_s : "NULL");
      fprintf(stderr,"%s: Xiv_s='%s'\n",P,Xiv_s ? Xiv_s : "NULL");
      fprintf(stderr,"%s: Yiv_s='%s'\n",P,Yiv_s ? Yiv_s : "NULL");
      fprintf(stderr,"%s: Ziv_s='%s'\n",P,Ziv_s ? Ziv_s : "NULL");
      fprintf(stderr,"%s: Tfv_s='%s'\n",P,Tfv_s ? Tfv_s : "NULL");
      fprintf(stderr,"%s: Xfv_s='%s'\n",P,Xfv_s ? Xfv_s : "NULL");
      fprintf(stderr,"%s: Yfv_s='%s'\n",P,Yfv_s ? Yfv_s : "NULL");
      fprintf(stderr,"%s: Zfv_s='%s'\n",P,Zfv_s ? Zfv_s : "NULL");
   }

   if( Errflg ) goto Exit; 

   /* Process temporal fvec, if specified ... */
   if( Tfv_s ) {
      rc = fvecstrtodseq(Tfv_s,&Tfv,&Tfv_len);
      if( rc <= 0 ) {
         fprintf(stderr,"%s: Error parsing t fvec '%s'\n",P,Tfv_s);
      }
      if( Ltrace ) {
         dvdump(Tfv,Tfv_len,"Tfv");
      }
   }

   /* Determine (putative) rank of input data, and number of datasets  ... */ 
   if( !(gpin = gft_open_sdf_file(Infilename)) ) {
      fprintf(stderr,"%s: Could not open '%s'\n",P,Infilename);
      goto Exit;
   }
   if( !(rc = gft_read_rank(Infilename,1,&Inrank)) ) {
      fprintf(stderr,"%s: Could not read rank of first data set in '%s'\n",
         P,Infilename);
   }
   Inngft = gft_read_sdf_ntlevs(Infilename);
   if( Ltrace ) {
      fprintf(stderr,"%s: First data set of %d in '%s' is rank %d\n",
         P,Inngft,Infilename,Inrank);
   }
   /* Close file to effect rewind ... */

   gsfd_close(gpin);

   /*  Check for input-rank/argument consistency, and define wildcard
    *  selectors ... */ 
   switch( Inrank ) {
   case 1:
      if( Yiv_s ) argerr("'-2' option is invalid for rank-1 data");
      if( Ziv_s ) argerr("'-3' option is invalid for rank-1 data");
      if( Yfv_s ) argerr("'-y' option is invalid for rank-1 data");
      if( Zfv_s ) argerr("'-z' option is invalid for rank-1 data");
      if( !Tiv_s && !Tfv_s ) Tiv_s = strdup("*");
      if( !Xiv_s && !Xfv_s ) Xiv_s = strdup("*");
      break;
   case 2:
      if( Ziv_s ) argerr("'-3' option is invalid for rank-2 data");
      if( Zfv_s ) argerr("'-z' option is invalid for rank-2 data");
      if( !Tiv_s && !Tfv_s ) Tiv_s = strdup("*");
      if( !Xiv_s && !Xfv_s ) Xiv_s = strdup("*");
      if( !Yiv_s && !Yfv_s ) Yiv_s = strdup("*");
      break;
   case 3:
      if( !Tiv_s && !Tfv_s ) Tiv_s = strdup("*");
      if( !Xiv_s && !Xfv_s ) Xiv_s = strdup("*");
      if( !Yiv_s && !Yfv_s ) Yiv_s = strdup("*");
      if( !Ziv_s && !Zfv_s ) Ziv_s = strdup("*");

      /*  BAIL OUT FOR NOW ... */
      fprintf(stderr,"%s: Processing of rank-3 data is not implemented yet!\n",
         P);
      exit(1);

      break;
   default:
      fprintf(stderr,"%s: Cannot process rank-%d data\n",P,Inrank);
      Errflg = 1;
      break;
   }

   if( Errflg ) goto Exit; 

   if( Ltrace ) {
      fprintf(stderr,"%s: Final index vectors ---\n",P);
      fprintf(stderr,"%s: Tiv_s='%s'\n",P,Tiv_s ? Tiv_s : "NULL");
      fprintf(stderr,"%s: Xiv_s='%s'\n",P,Xiv_s ? Xiv_s : "NULL");
      fprintf(stderr,"%s: Yiv_s='%s'\n",P,Yiv_s ? Yiv_s : "NULL");
      fprintf(stderr,"%s: Ziv_s='%s'\n",P,Ziv_s ? Ziv_s : "NULL");
      fprintf(stderr,"%s: Tfv_s='%s'\n",P,Tfv_s ? Tfv_s : "NULL");
      fprintf(stderr,"%s: Xfv_s='%s'\n",P,Xfv_s ? Xfv_s : "NULL");
      fprintf(stderr,"%s: Yfv_s='%s'\n",P,Yfv_s ? Yfv_s : "NULL");
      fprintf(stderr,"%s: Zfv_s='%s'\n",P,Zfv_s ? Zfv_s : "NULL");
   }

   /* Process temporal ivec, if specified ... */

   if( Tiv_s ) {
      rc = ivecstrtoiseq(Tiv_s,1,Inngft,&Tiv,&Tiv_len);
      if( rc <= 0 ) {
         fprintf(stderr,"%s: Error parsing t ivec '%s'\n",P,Tiv_s);
      }
      if( Ltrace ) {
         ivdump(Tiv,Tiv_len,"Tiv");
      }
   }

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
      if( (Tiv && (ivlookup_linear(Tiv,Tiv_len,i)) >= 0) ||
          (Tfv && (dvlookup_linear(Tfv,Tfv_len,D.time,Fuzz)) >= 0) ) {
         if( Ltrace ) {
            fprintf(stderr,"%s: Slicing data set %d, time=%g\n",P,i,D.time);
         }
         if( D.rank == Inrank ) {
            pDsliced = slice_GFT(&D,Xiv_s,Yiv_s,Ziv_s,Xfv_s,Yfv_s,Zfv_s,Fuzz);
         } else {
            fprintf(stderr,
               "%s: Warning! Ignoring rank-%d data set in input.\n",P,D.rank);
            pDsliced = (GFT *) NULL;
         }
         if( pDsliced ) {
            if( Ltrace ) {
               fprintf(stderr,
                  "%s: Writing rank-%d data. Data size=%d. Coord size=%d\n",
                     P,pDsliced->rank,pDsliced->dsize,pDsliced->csize);
            }
            low_write_sdf_stream(gpout->fp,
               pDsliced->pname,pDsliced->time,pDsliced->rank,pDsliced->dsize,
               pDsliced->csize,pDsliced->cnames,pDsliced->tag,pDsliced->shape,
               pDsliced->coords,pDsliced->data);
            free_GFT_data(pDsliced);
            pDsliced = (GFT *) NULL;
         }
      }
      free_GFT_data(&D);
   }
Exit:
   return 0;
Usage:
   fprintf(stderr,"usage: %s ...\n",P);
   fprintf(stderr,"          [-h]               Print this message\n");
   fprintf(stderr,"          [-v]               Enable verbose mode\n");
   fprintf(stderr,"          [-V]               Enable very verbose mode\n");
   fprintf(stderr,"          [-0 time-ivec]     time index vector (ivec)\n");
   fprintf(stderr,"          [-1 x-ivec]        x ivec\n");
   fprintf(stderr,"          [-2 y-ivec]        y ivec\n");
   fprintf(stderr,"          [-3 z-ivec]        z ivec\n");
   fprintf(stderr,"          [-t time-fvec]     time float index vector (fvec)\n");
   fprintf(stderr,"          [-x x-fvec]        x fvec\n");
   fprintf(stderr,"          [-y y-fvec]        y fvec \n");
   fprintf(stderr,"          [-z z-fvec]        z fvec\n");
   fprintf(stderr,"          infile             Input .sdf file\n");
   fprintf(stderr,"          outfile            Output .sdf file\n");
   fprintf(stderr,"\n");
   fprintf(stderr,"   Notes:\n");
   fprintf(stderr,"      - infile data must be of uniform rank (1, 2 or 3)\n");
   fprintf(stderr,"      - Options -0 and -t are mutually exclusive\n");
   fprintf(stderr,"      - Options -1 and -x are mutually exclusive\n");
   fprintf(stderr,"      - Options -2 and -y are mutually exclusive\n");
   fprintf(stderr,"      - Options -3 and -z are mutually exclusive\n");
   fprintf(stderr,"      - ivec's are 1-based (*not* 0-based)\n");
   fprintf(stderr,"      - Options -2 and -y are valid only for rank-2 and rank-3 data\n");
   fprintf(stderr,"      - Options -3 and -z are valid only for rank-3 data\n");
   fprintf(stderr,"      - Out of range values in ivec/fvec specifications are generally silently\n");
   fprintf(stderr,"        ignored\n");
   fprintf(stderr,"      - ivec/fvec specifications that contain single values result in \n");
   fprintf(stderr,"        reduction of rank of data\n");
   fprintf(stderr,"\n");
   fprintf(stderr,"   Examples:\n");
   fprintf(stderr,"\n");
   fprintf(stderr,"      sdfslice -x 0.0 data_2d.sdf data2_x_0.0.sdf\n");
   fprintf(stderr,"      sdfslice -x 0.0 -y 0.0 data_3d.sdf data3_xy_0.0.sdf\n");
   fprintf(stderr,"      sdfslice -x '0.0,1.0' data_2d.sdf data_x_0.0_1.0.sdf\n");

   return 1;
}
