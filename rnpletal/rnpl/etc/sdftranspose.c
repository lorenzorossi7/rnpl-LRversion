#include <stdlib.h>
#include <math.h>
#include <string.h>
#include <unistd.h>
#include <stdio.h>
#include <libgen.h>
#include <sdf_priv.h>
#include <sdf.h>
#include "sdftranspose.h"

int main(int argc, char **argv) {
   static char P[] = "sdftranspose";

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
   double          *datain;
   double          *dataout;

   gft_sdf_file_data *gpin  = (gft_sdf_file_data *) NULL;
   gft_sdf_file_data *gpout = (gft_sdf_file_data *) NULL;

   if( argc != 3 ) usage();
   if( !strcmp(argv[1],argv[2]) ) {
      fprintf(stderr,"%s: Input and output files must be distinct.\n",P);
      goto ErrExit;
   }

   if( !(gpin = gft_open_sdf_file(argv[1])) ) {
      fprintf(stderr,"%s: Could not open '%s'\n",P,argv[1]);
      goto ErrExit;
   }

   if( gpout = gft_open_sdf_file(argv[2]) ) {
      fprintf(stderr,"%s: File '%s' already exists. Remove explicitly and rerun.\n\n",
         P,argv[2]);
      fprintf(stderr,"/bin/rm %s; %s %s %s\n",argv[2],P,argv[1],argv[2]);
      goto ErrExit;
   } else {
      gft_close(argv[2]);
   }

   if( !(gpout = gft_create_sdf_stream(argv[2])) ) {
      fprintf(stderr,"%s: Could not create '%s'\n",P,argv[2]);
      goto ErrExit;
   }

   while ( low_read_sdf_stream(1,gpin->fp,&time,&version,&rank,&dsize,&csize,
              &pname,&cnames,&tag,&shape,&bbox,&coords,&datain) ) {

      if( !(dataout = (double *) malloc(dsize * sizeof(double))) ) {
         fprintf(stderr,"%s: malloc() of %d doubles failed\n",P,dsize);
         goto ErrExit;
      }

      switch( rank ) {
      case 1:
         d1d_transpose(datain,dataout,shape[0]);
         break;
      case 2:
         d2d_transpose(datain,dataout,shape[0],shape[1]);
         break;
      case 3:
         d3d_transpose(datain,dataout,shape[0],shape[1],shape[2]);
         break;
      default:
         fprintf(stderr,"%s: Transposition of data with rank > 3 not supported.\n",P);
      }

      low_write_sdf_stream(gpout->fp,pname,time,rank,dsize,csize,
         cnames,tag,shape,coords,dataout);

      if( pname )   free(pname);
      if( cnames )  free(cnames);
      if( tag )     free(tag);
      if( shape )   free(shape);
      if( bbox )    free(bbox);
      if( coords )  free(coords);
      if( datain )  free(datain);
      if( dataout ) free(dataout);
   }

Exit:
   fprintf(stderr,"%s: %s -> %s\n",P,argv[1],argv[2]);
   return 0;

ErrExit:
   return 1;
}

void usage(void) {
   fprintf(stderr,"usage: sdftranspose infile outfile\n\n");
   fprintf(stderr,"       Converts an .sdf file stored in row-major order to column-major (Fortran) order.\n");
   exit(1);
}

void d1d_transpose(double *d, double *dt, int n1) {
   int i1;

   for( i1 = 1; i1 <= n1; i1++ ) {
      dt[i1-1] = d[i1-1];
   }
}

void d2d_transpose(double *d, double *dt, int n1, int n2) {
   int i1, i2;

   for( i1 = 1; i1 <= n1; i1++ ) {
      for( i2 = 1; i2 <= n2; i2++ ) {
         *(dt + (i2 - 1)*n1 + (i1 - 1)) = *(d + (i1 - 1)*n2 + (i2 - 1));
      }
   }
}

void d3d_transpose(double *d, double *dt, int n1, int n2, int n3) {
   int i1, i2, i3;

   for( i1 = 1; i1 <= n1; i1++ ) {
      for( i2 = 1; i2 <= n2; i2++ ) {
         for( i3 = 1; i3 <= n3; i3++ ) {
            *(dt + (i3 - 1)*n1*n2 + (i2 - 1)*n1 + (i1 - 1)) = 
            *(d  + (i1 - 1)*n2*n3 + (i2 - 1)*n3 + (i3 - 1));
         }
      }
   }
}
