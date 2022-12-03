/*

sdfrank.c 

Tries to determine rank of single .sdf file.

History: sdftemplate.c

*/

#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <bbhutil.h>
#include <sdf.h>
#include <math.h>

#define IVLEN  1024
#define BUFLEN 1024
#define NTMAX  1024*1024

char *P;
int   default_n = 10;

void die(char *mess) {
   fprintf(stderr,"%s: %s\n",P,mess);
   exit(1);
}

void usage() {
   fprintf(stderr,"Synopsis: \n");
   fprintf(stderr,"  Attempts to determine rank of single .sdf file.\n");
   fprintf(stderr,"  Returns 0 if argument does not specify .sdf file\n");
   fprintf(stderr,"  or if file contains datasets of more than one rank\n");
   fprintf(stderr,"  or if there are problems processing file.\n");
   fprintf(stderr,"\nUsage: \n");
   fprintf(stderr,"  sdfrank [ -h ]\n");
   fprintf(stderr,"          [ -n | %d ]\n",default_n);
   fprintf(stderr,"          sdf_file\n\n");
   fprintf(stderr,"  -n       -- look at first n datasets\n");
   fprintf(stderr,"  -h       -- print this message\n");
   exit(1);
}

int main(int argc, char **argv) {
   int      ltrace =  0;

   char     *P;
   char     *fname;
   int       n = default_n;

   int       opt, argerr = 0;

   /* For GFT_extract2 call ... */
   int       tlev; 
   char     *func_name; 
   int       ntlev; 
   double   *tvec;
   double    time; 
   int       drank; 
   int      *shape; 
   char    **cnames;
   double  **coords;
   double   *data;

   int       drank_1; 

   int       nfile, it, rc;

   P = argv[0];

   n = default_n;

/* Argument processing ... */
   while( (opt = getopt(argc,argv,"hn:")) != EOF) {
      switch( opt ){
      case 'h' :
         usage();
         break;
      case 'n' :
         if( sscanf(optarg,"%d",&n) != 1 ) usage();
         break;
      default :
         argerr=1;
         break;
      }
   }

   nfile = argc - optind;
   if( argerr || nfile != 1 ) usage();

   fname = argv[optind];
   if( ltrace ) {
      fprintf(stderr,"\n%s: fname=<%s> n=%d\n",P,fname,n);
   }

   if( ! gft_is_sdf_file(fname) ) {
      printf("0\n");
      exit(1);
   }

   it = 1;
   if( !GFT_extract2(fname,it,&func_name,&ntlev,&tvec,&time,
        &drank_1,&shape,&cnames,&coords,&data) ) goto Error;
   if( !GFT_extract2(fname,-it,&func_name,&ntlev,&tvec,&time,
       &drank_1,&shape,&cnames,&coords,&data) ) goto Error;

   n = ntlev < n ? ntlev : n;

   for( it = 1; it <= n; it ++ ) {
      if( !(rc = GFT_extract2(fname,it,&func_name,&ntlev,&tvec,&time,
              &drank,&shape,&cnames,&coords,&data)) ) goto Error;
      if( ltrace ) {
         fprintf(stderr," Data set: %d  Time: %g  Rank: %d  rc: %d\n",
            it,time,drank,rc);
      }
      if( !GFT_extract2(fname,-it,&func_name,&ntlev,&tvec,&time,
          &drank,&shape,&cnames,&coords,&data) ) goto Error;
      if( drank != drank_1 ) goto Error;
   }
   printf("%d\n",drank_1);

   exit(0);
Error:
   printf("0\n");
   exit(1);
}
