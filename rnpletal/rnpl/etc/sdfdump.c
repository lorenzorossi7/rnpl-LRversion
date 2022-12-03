/* 
   sdfdump
   Dumps an .sdf file to standard output in ASCII format

   Copyright 2003- Matthew W. Choptuik, (main/1d) Frank Loeffler (2d)
*/

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <bbhutil.h>
#include <sdf_priv.h>
#include <sdf.h> 

#define IVLEN  1024
#define BUFLEN 1024
#define NTMAX  1024*1024
 
char *P;
 
double zt(double x,int zerotiny) {
   return (zerotiny && (abs(x) < 1.0e-99)) ? 0.0 : x;
}

void die(char *mess) {
   fprintf(stderr,"%s: %s\n",P,mess);
   exit(1);
}
 
void usage() {
   fprintf(stderr,"Synopsis (modified by HB): \n");
   fprintf(stderr,"  Dumps .sdf file to standard output in ASCII format.\n\n");
   fprintf(stderr,"Usage: \n");
   fprintf(stderr,"  %s [ -i ivec ]\n",P);
   fprintf(stderr,"          [ -f format ]\n");
   fprintf(stderr,"          [ -s stride-vector ]\n");
   fprintf(stderr,"          [ -t ]\n");
   fprintf(stderr,"          [ -T ]\n");
   fprintf(stderr,"          [ -N ]\n");
   fprintf(stderr,"          [ -B ]\n");
   fprintf(stderr,"          [ -z ]\n");
   fprintf(stderr,"          file\n\n");
   fprintf(stderr,"  -i ivec  -- use ivec (1 based) for output control.\n");
   fprintf(stderr,"  -f format -- floating point format.\n");
   fprintf(stderr,"  -s stride-vector -- controls dumping increments in each\n");
   fprintf(stderr,"                      of the coordinate directions, x_i\n");
   fprintf(stderr,"     Examples:  -s 5       # Every 5th in x_1\n");
   fprintf(stderr,"                -s 5,10    # Every 5th in x_1, 10th in x_2\n");
   fprintf(stderr,"  -t -- do not include time as column in dump.\n");
   fprintf(stderr,"  -T -- dump time only.\n");
   fprintf(stderr,"  -B -- no blank line between datasets.\n");
   fprintf(stderr,"  -z -- replace values with mag < 1.0e-99 with 0.\n");
   fprintf(stderr,"  -y -- use ygraph output format. Implies -t\n");
   fprintf(stderr,"\n");
   fprintf(stderr,"Output:\n\n");
   fprintf(stderr,"   t   x_1  ...   x_n   f(t,x_1,..,x_n)\n");
   fprintf(stderr,"   t   x_1  ...   x_n   f(t,x_1,..,x_n)\n");
   fprintf(stderr,"                     .                    \n");
   fprintf(stderr,"                     .                    \n");
   fprintf(stderr,"                     .                    \n");
   fprintf(stderr,"\n");
   fprintf(stderr,"or\n\n");        
   fprintf(stderr,"   x_1  ...   x_n   f(t,x_1,..,x_n)\n");
   fprintf(stderr,"   x_1  ...   x_n   f(t,x_1,..,x_n)\n");
   fprintf(stderr,"                  .                    \n");
   fprintf(stderr,"                  .                    \n");
   fprintf(stderr,"                  .                    \n");
   exit(0);
} 

int main(int argc, char **argv) {
//   int     ltrace =  0;
//HB
   int     ltrace =  1;
   int     ltrace_full =  0;

   static char default_fmt[] = "%24.16e";
   static char default_stride[] = "1,1,1,1";

   int     opt, argerr = 0,
           defsiv = 0, dumptime = 1, dumptimeonly = 0, dumpntonly = 0,
           blankline = 1, zerotiny = 0, ygraph=0;
   char    siv[BUFLEN], outfmt[BUFLEN], wd[BUFLEN];
   int     istride[4] = {1,1,1,1};
   char   *fname, *fmt = (char *) NULL, *stride = (char *) NULL;
   int     iv[IVLEN];
   int     nfile, it, ix, iy, iz, ndump;
 
   FILE   *stream = (FILE *) NULL;
 
   int     version, rank, dsize, csize;
   double  time;
 
   int    *shape  = (int *) NULL;
   double *bbox   = (double *) NULL, *coords = (double *) NULL,
          *data   = (double *) NULL;
   char   *pname  = (char *) NULL, *cnames = (char *) NULL,
          *tag    = (char *) NULL;
 
   gft_sdf_file_data *gp;
 
   P = argv[0];
   while( (opt = getopt(argc,argv,"i:f:s:tTNBzy")) != EOF) {
      switch( opt ){
      case 'i' :
         sprintf(siv,"output:=%s",optarg);
         defsiv = 1;
         break;
      case 'f' :
         fmt = strdup(optarg);
         break;
      case 's' :
         stride = strdup(optarg);
         break;
      case 't' :
         dumptime = 0;
         break;
      case 'T' :
         dumptimeonly = 1;
         break;
      case 'N' :
         dumpntonly = 1;
         break;
      case 'B' :
         blankline = 0;
         break;
      case 'y' :
         ygraph = 1;
         dumptime = 0;
         break;
      case 'z' :
         zerotiny = 0;
         break;
      default :
         argerr=1;
         break;
      }
   }  

   nfile = argc - optind;
   if( argerr || nfile != 1 ) usage();
   fname = argv[optind];
   if( !defsiv ) sprintf(siv,"output:=1-*");
   if( !sget_ivec_param(siv,"output",iv,IVLEN) )  
      die("Inusfficient memory to allocate index vector."); 
   fixup_ivec(1,NTMAX,0,iv);
   if( !fmt ) fmt = strdup(default_fmt);
   if( !stride ) stride = strdup(default_stride);
   sscanf(stride,"%d,%d,%d,%d",istride,istride+1,istride+2,istride+3);
   
   if( ltrace ) {
      fprintf(stderr,"%s: file='%s'\n",P,fname);
      fprintf(stderr,"%s: ivec='%s'\n",P,siv);
      fprintf(stderr,"%s: fmt='%s'\n",P,fmt);
      fprintf(stderr,"%s: stride='%s'\n",P,stride);
      fprintf(stderr,"%s: istride=%d,%d,%d,%d\n",P,
         istride[0],istride[1],istride[2],istride[3]);
      fprintf(stderr,"%s: dumptime=%s\n",P,dumptime ? "on" : "off");
      fprintf(stderr,"%s: dumptimeonly=%s\n",P,dumptimeonly ? "on" : "off");
   }

   gft_set_multi();

   if( !(gp = gft_open_sdf_stream(fname)) ) {
      fprintf(stderr,"%s: Could not open '%s'\n",P,fname);
      exit(1);
   }  
   
   it = 0;
   ndump = 0;
   while( low_read_sdf_stream(1,gp->fp,&time,&version,&rank,
             &dsize,&csize,&pname,&cnames,&tag,&shape,&bbox,&coords,&data) ) {
      it++;
      if( do_ivec(it,IVLEN,iv) ) {
         if( ltrace ) fprintf(stderr,"%s: Would dump data set %d\n",P,it);
         if( dumptimeonly ) {
            sprintf(outfmt,"%s\n",fmt);
            printf(outfmt,time);
         } else if( dumpntonly ) {
         } else {
            if( ndump > 0 && blankline ) printf("\n");
            if (ygraph){
               sprintf(outfmt,"\"Time = %s\n","%g");
               printf(outfmt, time);
            }
            switch( rank ) {
            case 1:
               if( dumptime ) {
                  sprintf(outfmt,"%s %s %s\n",fmt,fmt,fmt);
                  for( ix = 0; ix < shape[0]; ix += istride[0] ) {
//                     if( csize == dsize ) { //HB
                        printf(outfmt,time,zt(coords[ix],zerotiny),
                           zt(data[ix],zerotiny));
//                     } else {
//                        fprintf(stderr,"Bounding box dumping is not implemented yet.\n");
//                        exit(1);
//                     }
                  }
               } else {
                  sprintf(outfmt,"%s %s\n",fmt,fmt);
                  for( ix = 0; ix < shape[0]; ix += istride[0] ) {
//                     if( csize == dsize ) { //HB
                        printf(outfmt,coords[ix],data[ix]);
//                     } else {
//                        fprintf(stderr,"Bounding box dumping is not implemented yet.\n");
//                        exit(1);
//                     }
                  }
               }
               break;
            case 2:
               if( dumptime ) {
                  sprintf(outfmt,"%s %s %s %s\n",fmt,fmt,fmt,fmt);
                  for( ix = 0; ix < shape[0]; ix += istride[0] )
                   for( iy = 0; iy < shape[1]; iy += istride[1] ) {
//                     if( csize == shape[0]+shape[1] ) { //HB
                        printf(outfmt,time,zt(coords[ix],zerotiny),
                                           zt(coords[iy],zerotiny),
                                           zt(data[ix+iy*shape[0]],zerotiny));
//                     } else {
//                        fprintf(stderr,"Bounding box dumping is not implemented yet.\n");
//                        exit(1);
//                     }
                  }
               } else {
//                  sprintf(outfmt,"%s %s %s\n",fmt,fmt,fmt);
//                  for( ix = 0; ix < shape[0]; ix += istride[0] )
//                   for( iy = 0; iy < shape[1]; iy += istride[1] ) {
//                     if( csize == shape[0]+shape[1] ) {
//                        printf(outfmt,coords[ix],
//                                      coords[iy],
//                                      data[ix+iy*shape[0]]);
//                     } else {
//                        fprintf(stderr,"Bounding box dumping is not implemented yet.\n");
//                        exit(1);
//                     }
//                        printf(outfmt,coords[ix],
//                                      coords[iy],
//                                      data[ix+iy*shape[0]]);
//                  }
//               }
//HB
                  fprintf(stderr,"TESTHB,shape[0]=%i,shape[1]=%i,csize=%i,dsize=%i\n"
                         ,shape[0],shape[1],csize,dsize);
                  sprintf(outfmt,"%s %s %s\n","%i","%i",fmt);
                  for( ix = 0; ix < shape[0]; ix += 1 ) 
                   for( iy = 0; iy < shape[1]; iy += 1 ) {
                        printf(outfmt,ix,iy,data[ix+iy*shape[0]]);
                  }
               }
               break;
            case 3:
               if( dumptime ) {
                  sprintf(outfmt,"%s %s %s %s %s\n",fmt,fmt,fmt,fmt,fmt);
                  for( ix = 0; ix < shape[0]; ix += istride[0] )
                   for( iy = 0; iy < shape[1]; iy += istride[1] )
                    for( iz = 0; iz < shape[2]; iz += istride[2] ){
                          printf(outfmt,time,zt(coords[ix],zerotiny),
                                           zt(coords[iy],zerotiny),
                                           zt(coords[iz],zerotiny),
                                           zt(data[ix+shape[0]*(iy+shape[1]*iz)],zerotiny));
                  }
               } else {
                  fprintf(stderr,"TESTLR,shape[0]=%i,shape[1]=%i,shape[2]=%i,csize=%i,dsize=%i\n"
                         ,shape[0],shape[1],shape[2],csize,dsize);
                  sprintf(outfmt,"%s %s %s %s\n","%i","%i","%i",fmt);
                  for( ix = 0; ix < shape[0]; ix += 1 )
                   for( iy = 0; iy < shape[1]; iy += 1 )
                    for( iz = 0; iz < shape[2]; iz += 1 ) {
                        printf(outfmt,ix,iy,iz,data[ix+shape[0]*(iy+shape[1]*iz)]);
                  }
               }
//               fprintf(stderr,"%s: Rank-3 dumping is not implemented yet.\n",P);
//LR
               break;
//               exit(1);
//LR
            default:
               fprintf(stderr,"%s: Rank-%d dumping is not implemente.\n",P,rank);
               break;
            }
         }
         ndump++;
      }
   }
   if( dumpntonly ) { 
      printf("%d\n",ndump);
   }
}
