/*
  sdftosvx
  Sends a 1d rnpl sdf file to scivis server

  (c) 2001 by Matthew W. Choptuik
*/

#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <bbhutil.h>
#include <sdf_priv.h>
#include <sdf.h>

#define IVLEN  1024
#define BUFLEN 1024
#define NTMAX  1024*1024

char *P;

int rjava_mser(char *nm, int nt, double *time, double *x, double *data, int *size)
{
#ifdef HAVE_LIBSV
  return java_mser(nm,nt,time,x,data,size);
#else
  return 1;
#endif
}

void die(char *mess) {
   fprintf(stderr,"%s: %s\n",P,mess);
   exit(1);
}

void usage() {
   fprintf(stderr,"Synopsis: \n");
   fprintf(stderr,"  Sends .sdf files to the Scivis visualization server\n\n");
   fprintf(stderr,"Usage: \n");
   fprintf(stderr,"  %s [ -c ]\n",P);
   fprintf(stderr,"          [ -i ivec ]\n");
   fprintf(stderr,"          [ -n oname ]\n");
   fprintf(stderr,"          [ -x xfile ]\n");
   fprintf(stderr,"          input_file [ input_file [ ... ] ] \n\n");
   fprintf(stderr,"  -c       -- append coordinate name when naming\n");
   fprintf(stderr,"              function for output\n");
   fprintf(stderr,"  -i ivec  -- use ivec (1 based) for output control\n");
   fprintf(stderr,"  -n oname -- name all data sets oname\n");
   fprintf(stderr,"  -x xfile -- use y data from xfile for x coordinates\n\n");
   fprintf(stderr,"  input_file and xfile must be 1D .sdf files\n\n");
   exit(0);
}

void l_dvcopy(double *vin,double *vout,int n) {
   for( ; n > 0; n-- ) vout[n-1] = vin[n-1];
}

void l_dvmesh(double *vout,double vmin,double vmax,int n) {
   int    i;
   double dv;
   if( !n )  return;
   vout[0] = vmin;
   if( n > 1 ) {
      dv = (vmax - vmin) / (n - 1);
      for( i = 1; i < n; i++ ) {
         vout[i] = vout[i-1] + dv;
      }
      vout[n-1] = vmax;
   }
}

int main(int argc, char **argv) {
   int     ltrace =  0;
   int     opt, argerr = 0, 
           addcoords = 0, rename = 0,  xremap = 0, defsiv = 0;
   char    siv[BUFLEN], oname[BUFLEN], buf[BUFLEN];
   char   *fname, *xfname = (char *) NULL;
   int     iv[IVLEN];
   int     ifile, nfile, nt, it, pass, noerr, vsrc;
 
   int     version, rank, dsize, csize;
   double  time; 

   int    *shape  = (int *) NULL;
   double *bbox   = (double *) NULL, *coords = (double *) NULL,
          *data   = (double *) NULL;
   char   *pname  = (char *) NULL, *cnames = (char *) NULL, 
          *tag    = (char *) NULL;

   int     versionx, rankx, dsizex, csizex, ntlevsx, xonestep;
   double  timex; 

   int    *shapex = (int *) NULL;
   double *bboxx  = (double *) NULL, *coordsx = (double *) NULL,
          *datax  = (double *) NULL, *tvecx   = (double *) NULL;
   char   *pnamex = (char *) NULL, *cnamesx = (char *) NULL, 
          *tagx   = (char *) NULL;

   int     itout, ntout, ndataout, datap;
   int    *nxout  = (int *) NULL;
   double *xout   = (double *) NULL, *yout = (double *) NULL, 
          *tout   = (double *) NULL;

   gft_sdf_file_data *gp, *gpx; 

   P = argv[0];
   while( (opt = getopt(argc,argv,"ci:n:x:")) != EOF) {
      switch( opt ){
      case 'c' :
         addcoords=1;
         break;
      case 'i' :
         sprintf(siv,"output:=%s",optarg);
         defsiv = 1;
         break;
      case 'n' :
         rename=1;
         strcpy(oname,optarg);
         break;
      case 'x' :
         xremap=1;
         xfname = optarg;
         break;
      default :
         argerr=1;
         break;
      }
   }

   if( ltrace ) {
      fprintf(stderr,"addcoords: %d\n",addcoords);
      fprintf(stderr,"rename: %d\n",rename);
      fprintf(stderr,"oname: %s\n",oname);
      fprintf(stderr,"xremap: %d\n",xremap);
      fprintf(stderr,"xfname: %s\n",xfname ? xfname : "NULL");
   }

   nfile = argc - optind;
   if( argerr || nfile < 1 ) usage();
   if( rename && (nfile > 1) ) 
      die("-n argument only makes sense for a single file.");

   if( !defsiv ) sprintf(siv,"output:=1-*");
   if( !sget_ivec_param(siv,"output",iv,IVLEN) ) 
      die("Inusfficient memory to allocate index vector.");
   fixup_ivec(1,NTMAX,0,iv);
 
   gft_set_multi();

   if( xremap ) {
      if( gft_extract_sdf(xfname,1,&pnamex,&ntlevsx,&tvecx,&timex,
             &rankx,&shapex,&csizex,&cnamesx,&coordsx,&datax) ) {
         xonestep = (ntlevsx == 1);
         if( !xonestep ) {
            gft_extract_sdf(xfname,-1,&pnamex,&ntlevsx,&tvecx,&timex,
               &rankx,&shapex,&csizex,&cnamesx,&coordsx,&datax);
            gft_close_sdf_stream(xfname);
         }
      } else {
         sprintf(buf,"Error opening x-remap file '%s'",xfname); die(buf);
      }
   }

#define ABORT noerr = 0; \
        fprintf(stderr,"%s: Processing of '%s' aborted.\n",P,fname)

   for( ifile = optind; ifile < argc; ifile++ ) {
      fname = argv[ifile];
      if( ltrace ) fprintf(stderr,"Processing '%s'\n",fname);
      if( !(gp = gft_open_sdf_stream(fname)) ) {
         fprintf(stderr,"%s: Could not open '%s'\n",P,fname);
         continue;
      }
      if( xremap && !(gpx = gft_open_sdf_stream(xfname)) ) {
         fprintf(stderr,"%s: Could not open '%s'\n",P,xfname);
         continue;
      }
      for( noerr = 1, pass = 1; pass <=2 && noerr; pass++ ) {
         if( ltrace ) fprintf(stderr,"Processing '%s': pass %d\n",fname,pass);
         it = 0;
         if( pass == 1 ) {
            ntout = 0;
            ndataout = 0;
         } else {
            if( !(gp = gft_open_sdf_stream(fname)) ) {
               fprintf(stderr,"%s: Unexpected error reopening '%s'.\n",P,fname);
               ABORT;
            }
            if( xremap && !(gpx = gft_open_sdf_stream(xfname)) ) {
               fprintf(stderr,"%s: Unexpected error reopening '%s'.\n",P,fname);
               ABORT;
            }
            if( ltrace ) fprintf(stderr,"   ntout: %d  ndataout: %d\n",
                         ntout,ndataout);
            if( !(xout  = (double *) malloc(ndataout * sizeof(double))) ) {
               fprintf(stderr,"%s: Error allocating %d doubles.\n",P,ndataout);
               ABORT;
            }
            if( !(yout  = (double *) malloc(ndataout * sizeof(double))) ) {
               fprintf(stderr,"%s: Error allocating %d doubles.\n",P,ndataout);
               ABORT;
            }
            if( !(tout  = (double *) malloc((ntout) * sizeof(double))) ) {
               fprintf(stderr,"%s: Error allocating %d doubles.\n",P,ntout);
               ABORT;
            }
            if( !(nxout = (int *) malloc((ntout) * sizeof(int))) ) {
               fprintf(stderr,"%s: Error allocating %d ints.\n",P,ntout);
               ABORT;
            }
            itout = 0;
            ndataout = 0;
/*          Reset index vector ... */
            sget_ivec_param(siv,"output",iv,IVLEN); fixup_ivec(1,NTMAX,0,iv);
         }
         while( noerr && low_read_sdf_stream(1,gp->fp,&time,&version,&rank,
               &dsize,&csize,&pname,&cnames,&tag,&shape,&bbox,&coords,&data) 
              ) {
            it = it + 1;
            if( rank != 1 ) {
               fprintf(stderr,
                       "%s: Only rank-1 data can be sent to server.\n",P);
               ABORT;
            }
            if( xremap && !xonestep ) {
               if( !low_read_sdf_stream(1,gpx->fp,&timex,&versionx,&rankx,
                     &dsizex,&csizex,&pnamex,&cnamesx,&tagx,&shapex,&bboxx,
                     &coordsx,&datax) ) {
                  fprintf(stderr,"%s: Error reading step %d of '%s'\n",
                          P,it,xfname);
                  pnamex = NULL; cnamesx  = NULL; tagx  = NULL; shapex = NULL;
                  bboxx  = NULL; coordsx  = NULL; datax = NULL;
                  ABORT;
               } 
            }
            if( noerr && do_ivec(it,IVLEN,iv) ) {
               if( ltrace ) 
                  fprintf(stderr,"  Time step: %d  nx: %d  csize %d\n",
                          it,shape[0],csize);
               if( xremap && (shapex[0] != shape[0]) ) {
                  fprintf(stderr,
                     "%s: Inconsistent x-remap data length at step %d\n",P,it);
                  ABORT;     
               }
               if(         noerr && pass == 1 ) {
                  ntout++;
                  ndataout += shape[0];
               } else if ( noerr ) {
                  if( itout == 0  &&  !rename ) {
                     if( addcoords ) 
                        sprintf(oname,"%s(%s)",pname,cnames);
                     else 
                        sprintf(oname,"%s",pname);
                  } 
                  nxout[itout]   = shape[0];
                  tout[itout] = time;
                  l_dvcopy(data,yout+ndataout,shape[0]);
                  if(        xremap ) {
                     l_dvcopy(datax,xout+ndataout,shape[0]);
                  } else if( csize == shape[0] ) {
                     l_dvcopy(coords,xout+ndataout,shape[0]);
                  } else if( csize == 2 ) {
                     l_dvmesh(xout+ndataout,coords[0],coords[1],shape[0]);
                  } else {
                     fprintf(stderr,
                             "%s: Unexpected coordinate size: %d (nx: %d)\n",
                             P,csize,shape[0]);
                     ABORT;     
                  }
                  ndataout += shape[0];
                  itout++;
               }
            }
            if( pname )  free(pname);
            if( cnames ) free(cnames);
            if( tag )    free(tag);
            if( shape )  free(shape);
            if( bbox )   free(bbox);
            if( coords ) free(coords);
            if( data )   free(data);
            if( xremap && !xonestep ) {
               if( pnamex )  free(pnamex);
               if( cnamesx ) free(cnamesx);
               if( tagx )    free(tagx);
               if( shapex )  free(shapex);
               if( bboxx )   free(bboxx);
               if( coordsx ) free(coordsx);
               if( datax )   free(datax);
            }
         }
         gft_close_sdf_stream(fname);
         if( xremap ) gft_close_sdf_stream(xfname);
      }
      if( noerr ) {
         if( ltrace ) {
            fprintf(stderr,"Sending %d double pairs (%.2g MB) as '%s'\n",
                    ndataout,ndataout/(64.*1024),oname);
         }
         rjava_mser(oname,ntout,tout,xout,yout,nxout);
      }
      if( xout )  free(xout);
      if( yout )  free(yout);
      if( tout )  free(tout);
      if( nxout ) free(nxout);
   }
   exit(0);
}
