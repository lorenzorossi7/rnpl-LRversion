#include <stdlib.h>
#include "cvtestsdf.h"

/*====================================================================

 Copyright October 1996, Matthew W. Choptuik
 The University of Texas at Austin

 Basic convergence testing program for RNPL generated data.

 Program expects three RNPL-style .sdf files, each containing 
 a single grid function output at the same times and with 
 2:1 refinement relationships between the 1st and 2nd, and 
 2nd and 3rd files.  Program dumps convergence factors as 
 a function of time to standard output and, optionally, outputs 
 point-wise grid-function differences in RNPL-style .sdf files.

 Minimal error checking at this stage: In particular, all files 
 assumed to contain data sets of same rank and shape.

====================================================================*/

int main(int argc,char **argv) {

   const int      Nlev = 3;
   GFT_type      *D;
   char         **F;
   IVEC           N,        Nx,        Ny,       Nz;
   DVEC           d01,      d12;
   double         nrmd01,   nrmd12;
   int            i,        step,      rc;

   int            Out_diffs;
   char          *O_tag;
   int            O_tag_lbase = 0;
   char           Nd01[BUFLEN],        Nd12[BUFLEN];
   double        *Bbox;

   int            ok          = ON;
   int            ltrace      = OFF;
   int            full_ltrace = OFF;

   int            Rank,     Ntlevs;

   DVEC           Times,    Cvfact;

   if( argc < 4 ) goto Usage;

/* Check for point-wise difference option. */

   if( argc > 4 ) {
      Out_diffs = ON;
      O_tag = argv[4];
      if( argc > 5 ) {
         if( sscanf(argv[5],"%d",&O_tag_lbase) != 1 ) {
            O_tag_lbase = 0;
         }
      } 
      sprintf(Nd01,"%s%d%d",O_tag,O_tag_lbase,O_tag_lbase+1);
      sprintf(Nd12,"%s%d%d",O_tag,O_tag_lbase+1,O_tag_lbase+2);
      if( ltrace ) {
         fprintf(stderr,"Will output pointwise differences to ");
         fprintf(stderr,"'%s' and '%s'\n",Nd01,Nd12);
      }
   }
   argv++;

/* Allocate storage for GFTs and filenames (which will be canonicalized 
   via addition of .sdf extension if necessary. */

   D = (GFT_type *) malloc( Nlev * sizeof(GFT_type) );
   F = (char **) malloc( Nlev * sizeof(char *) );

/* Initial check: Attempt to extract step 1 data from all files. */

   for( i = 0; i < Nlev; i++ ) {
      F[i] = sdf_canon(argv[i]);
      if( (rc = extract_GFT(F[i],1,D + i)) ) {
         if( ltrace ) {
            fprintf(stderr,"'%s' contains %d time levels of grid fcn '%s'\n",
                    F[i],D[i].ntlevs,D[i].gfunc_name);
            if( ltrace ) {
               dvfdump(stderr,D[i].time_vec,D[i].ntlevs,"Stored times");
            }
         }
      } else {
         fprintf(stderr,"Could not extract information from '%s'\n",F[i]);
         ok = OFF;
      }
   }

   if( !ok ) {
      fprintf(stderr,"\nError extracting information from one or more\n");
      fprintf(stderr,"specified input files.  Do all the files exist\n");
      fprintf(stderr,"and are they all RNPL-styl sdf files?\n");
      exit(1);
   }

/* Check that lattices and data appear suitable for convergence testing. */

   ok = check_GFT_cvt3(D);
   if( !ok ) {
      fprintf(stderr,"\nData is not suitable for convergence testing.\n");
      goto Usage;
   }

/* Allocate storage for convergence factors, pointwise differences,
   array bounds and free step-1 data. */

   Rank   = D[0].rank;
   Ntlevs = D[0].ntlevs;
   Cvfact = make_DVEC(Ntlevs);
   Times  = Dvcopy(D[0].time_vec,Ntlevs);
   N  = make_IVEC(Nlev); 
   Nx = make_IVEC(Nlev); Ny = make_IVEC(Nlev); Nz = make_IVEC(Nlev);
   Bbox = make_DVEC(2 * Rank);
   for( i = 0; i < Rank; i++ ) {
      Bbox[2*i]   = D[0].coordinate_vals[i][0];
      Bbox[2*i+1] = D[0].coordinate_vals[i][D[0].shape[i]-1];
   }
   for( i = 0; i < Nlev; i++ ) {
      N[i]  = ivprod(D[i].shape,D[i].rank);
      Nx[i] = D[i].shape[0];
      Ny[i] = D[i].rank > 1 ? D[i].shape[1] : 0;
      Nz[i] = D[i].rank > 2 ? D[i].shape[2] : 0;
      rc = extract_GFT(F[i],-1,D + i);
   }
   d01  = make_DVEC(N[0]);
   d12  = make_DVEC(N[1]);
   if( ltrace ) {
      ivfdump(stderr,N,Nlev,"Lattice sizes"); ivfdump(stderr,Nx,Nlev,"Nx"); 
      ivfdump(stderr,Ny,Nlev,"Ny"); ivfdump(stderr,Nz,Nlev,"Nz");
   }

/* Step through files and compute convergence factors. */

   for( step = 1; step <= Ntlevs; step++ ) {
      int     f77_two = 2;

      for( i = 0; i < Nlev; i++ ) {
         if( !(rc = extract_GFT(F[i],step,D + i)) ) {
            fprintf(stderr,"Error reading step %d from '%s'\n",step,F[i]);
            exit(1);
         }
      }
      if( full_ltrace ) {
         fprintf(stderr,"Step: %d\n",step);
         for( i = 0; i < Nlev; i++ ) {
            int    size = ivprod(D[i].shape,D[i].rank);
            fprintf(stderr,"'%s' size: %d",F[i],size);
            fprintf(stderr," limits: %g -- %g\n",
                    dvmin(D[i].data,size),dvmax(D[i].data,size));
         }
      }
      switch( Rank ) {
      case 1:
         dvinj_(D[1].data,d01,&f77_two,&N[1]);
         dvinj_(D[2].data,d12,&f77_two,&N[2]);
         break;
      case 2:
         d2inj_(D[1].data,d01,&f77_two,&f77_two,&Nx[0],&Ny[0]);
         d2inj_(D[2].data,d12,&f77_two,&f77_two,&Nx[1],&Ny[1]);
         break;
      case 3:
         d3inj_(D[1].data,d01,&f77_two,&f77_two,&f77_two,&Nx[0],&Ny[0],&Nz[0]);
         d3inj_(D[2].data,d12,&f77_two,&f77_two,&f77_two,&Nx[1],&Ny[1],&Nz[1]);
         break;
      default:
         fprintf(stderr,"Unexpected rank: %d\n",Rank);
         exit(1);
      }

      dvvs_(D[0].data,d01,d01,&N[0]);
      nrmd01 = dvnrm2_(d01,&N[0]);
      dvvs_(D[1].data,d12,d12,&N[1]);
      nrmd12 = dvnrm2_(d12,&N[1]);
      Cvfact[step-1] = nrmd12 ? nrmd01 / nrmd12 : 0.0;

      if( Out_diffs ) {
         gft_out_bbox(Nd01,D[0].time,D[0].shape,D[0].rank,Bbox,d01);
         gft_out_bbox(Nd12,D[1].time,D[1].shape,D[1].rank,Bbox,d12);
      }

      for( i = 0; i < Nlev; i++ ) {
         rc = extract_GFT(F[i],-step,D + i);
      }
   }

   for( i = 0; i < Ntlevs; i++ ) {
      printf("%g %g\n",Times[i],Cvfact[i]);
   }

	gft_close_all();

   exit(0);

Usage:
   fprintf(stderr,"usage: cvtestsdf <file 1> <file 2> <file 3> ");
   fprintf(stderr,"[<dstem> <base level>]\n\n");
   fprintf(stderr,"       Computes and outputs to standard output the time, t, and\n");
   fprintf(stderr,"       the three-level convergence factor, Q(t), defined by\n\n");
   fprintf(stderr,"              ||u^4h - u^2h||_2 (t)\n");
   fprintf(stderr,"       Q(t) = ---------------------\n");
   fprintf(stderr,"              ||u^2h - u^h||_2 (t)\n\n");
   fprintf(stderr,"       As h-> 0, Q(t) should asymptote to 2^p for a pth order accurate scheme.\n\n");
   fprintf(stderr,"       Specify .sdf files containing single grid function (u) from coarsest to\n");
	fprintf(stderr,"       finest resolution. Grid resolutions must be in the ratio 4:2:1.\n");
   fprintf(stderr,"       All files must contain grid functions defined at\n");
   fprintf(stderr,"       the same output times.\n\n");
   fprintf(stderr,"       Supply <dstem> for pointwise-difference output to\n");
   fprintf(stderr,"       2 RNPL-style .sdf files with names of the form\n\n");
   fprintf(stderr,"          <dstem><l><l+1>.sdf\n");
   fprintf(stderr,"          <dstem><l+1><l+2>.sdf\n\n");
   fprintf(stderr,"       where <l> defaults to 0, but will be set to <base level>\n");
   fprintf(stderr,"       if that argument is supplied.\n");
   exit(1);
}

int extract_GFT(char *sdfname,int step,GFT_type *D) {
   int           GFT_rc;

   GFT_rc = GFT_extract2(sdfname,step,&(D->gfunc_name),&(D->ntlevs),
               &(D->time_vec),&(D->time),&(D->rank),&(D->shape),
               &(D->coord_names),&(D->coordinate_vals),&(D->data));
   return(GFT_rc);

}

char *sdf_canon(char *fname){
   char   *new;
   int     l,   addext = 1;

   int     ltrace = OFF;

   if( (l = strlen(fname)) >= 5 ) {
      if( !strncmp(fname + (l - 4),".sdf",4) ) addext = 0;
   }
   if( addext ) {
      new = (char *) malloc((l + 5) * sizeof(char));
      sprintf(new,"%s.sdf",fname);
   } else {
      new = strdup(fname);
   }
   if( ltrace ) fprintf(stderr,"sdf_canon: Returning <%s>\n",new);
   return( new );
}

int check_GFT_cvt3(GFT_type *D) {
   int    ltrace = OFF;
   int    rc = 1,   i;

   if( (D[0].rank != D[1].rank) || (D[0].rank != D[1].rank) ) {
      fprintf(stderr,"Files do not contain data set of the same rank: ");
      fprintf(stderr,"%d %d %d\n",D[0].rank,D[1].rank,D[2].rank);
      rc = 0;
   } else {
      if( ltrace ) {
         fprintf(stderr,"All files contain rank %d data\n",D[0].rank);
      }
   }
   if( (D[0].ntlevs != D[1].ntlevs) || (D[0].ntlevs != D[2].ntlevs) ) {
      fprintf(stderr,"Files do not contain the same number of time levels: ");
      fprintf(stderr,"%d %d %d\n",D[0].ntlevs,D[1].ntlevs,D[2].ntlevs);
      rc = 0;
   } else {
      if( ltrace ) {
         fprintf(stderr,"All files contain %d time levels\n",D[0].ntlevs);
      }
   }
   for( i = 0; i < D[0].rank; i++ ) {
      if( (2 * (D[0].shape[i] - 1) != (D[1].shape[i] - 1)) ||
          (2 * (D[1].shape[i] - 1) != (D[2].shape[i] - 1)) ) {
         fprintf(stderr,"Files not 2:1 in dimension %d\n",i+1);
         rc = 0;
      }
   }
   if( rc ) {
      if( ltrace ) {
         fprintf(stderr,"Data set dimensions consistent with 4:2:1 ");
         fprintf(stderr,"refinement ratio\n");
      }
   }
   return( rc );
}
