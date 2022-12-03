/*

sdftemplate.c 

Template program for processing .sdf file(s).

Uses GFT_extract2(...) to extract data from file(s).

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

void die(char *mess) {
   fprintf(stderr,"%s: %s\n",P,mess);
   exit(1);
}

void usage() {
   fprintf(stderr,"Synopsis: \n");
   fprintf(stderr,"  Does something with one or more .sdf files.\n");
   fprintf(stderr,"\nUsage: \n");
   fprintf(stderr,"  sdftemplate [ -i ivec]\n");
   fprintf(stderr,"              [ -h ]\n");
   fprintf(stderr,"              [ -o optval ]\n");
   fprintf(stderr,"               sdf_file [ sdf_file [ ... ] ] \n\n");
   fprintf(stderr,"  -i ivec   -- use ivec (1 based) for output control\n");
   fprintf(stderr,"  -o optval -- Sets optval to some integer value\n");
   fprintf(stderr,"  -h        -- print this message\n");
   exit(1);
}

int main(int argc, char **argv) {
   int      ltrace =  1;

   char     *P;
   char      siv[BUFLEN];
   char     *fname;
   int       defsiv = 0;
   int       optval = -1;

   int       opt, argerr = 0;

   int       iv[IVLEN];

   /* For GFT_extract2 call ... */
   int       tlev; /* in: time level to retrieve, < 0 frees storage */
   char     *func_name; /* name of this grid function */
   int       ntlev; /* number of time levels available in file */
   double   *tvec; /* vector of time values */
   double    time; /* time of this slice */
   int       drank; /* rank of data set */
   int      *shape; /* shape of data set */
   char    **cnames; /* names of coordinates */
   double  **coords; /* values of coordinates */
   double   *data;   /* actual data */

   int       ifile, nfile, it, rc;

   P = argv[0];

/* Argument processing ... */
   while( (opt = getopt(argc,argv,"hi:o:")) != EOF) {
      switch( opt ){
      case 'i' :
         sprintf(siv,"output:=%s",optarg);
         defsiv = 1;
         break;
      case 'h' :
         usage();
         break;
      case 'o' :
         if( sscanf(optarg,"%d",&optval) != 1 ) usage();
         break;
      default :
         argerr=1;
         break;
      }
   }

   nfile = argc - optind;
   if( argerr || nfile < 1 ) usage();

   if( !defsiv ) sprintf(siv,"output:=1-*");
   if( !sget_ivec_param(siv,"output",iv,IVLEN) ) 
      die("Inusfficient memory to allocate index vector.");

   if( ltrace ) {
      fprintf(stderr,"%s: siv=<%s>\n",P,siv ? siv : "NULL");
   }
   fixup_ivec(1,NTMAX,0,iv);

/* Process each file ... */
   for( ifile = optind; ifile < argc; ifile++ ) {
      fname = argv[ifile];
      /* Check to see that argument actually specifies an .sdf file ... */
      if( ! gft_is_sdf_file(fname) ) {
         fprintf(stderr,"%s: fname=<%s> is not an .sdf file ... skipping.\n",
            P,fname);
         continue;
      }

      /* Reprocess index vector ... */
      sget_ivec_param(siv,"output",iv,IVLEN); 
      fixup_ivec(1,NTMAX,0,iv); 
      if( ltrace ) {
         fprintf(stderr,"\n%s: fname=<%s>\n",P,fname);
      }
      /* Initial call to GFT_extract2 to determine number of time levels ... */
      it = 1;
      rc = GFT_extract2(fname,it,&func_name,&ntlev,&tvec,&time,
              &drank,&shape,&cnames,&coords,&data);
      /* Check return code and exit if 0 (optional, but caveat emptor) */
      if( !rc ) exit(1);

      if( ltrace ) 
         fprintf(stderr,"%s: file contains %d datasets.\n",P,ntlev);
      /* Free storage allocated by initial call ... */
      rc = GFT_extract2(fname,-it,&func_name,&ntlev,&tvec,&time,
          &drank,&shape,&cnames,&coords,&data);
      if( !rc ) exit(1);

      /* Loop over datasets (possibly selected via index vector) ... */
      for( it = 1; it <= ntlev; it ++ ) {
         if( do_ivec(it,IVLEN,iv) ) {
           /* Retrieve dataset ... */
            rc = GFT_extract2(fname,it,&func_name,&ntlev,&tvec,&time,
                    &drank,&shape,&cnames,&coords,&data);
            if( !rc ) exit(1);
            if( ltrace ) {
               fprintf(stderr," Data set: %d  Time: %g  Rank: %d\n",
                  it,time,drank);
            }
				/* Process data here ... */

            /* Free storage ... */
            rc = GFT_extract2(fname,-it,&func_name,&ntlev,&tvec,&time,
                &drank,&shape,&cnames,&coords,&data);
            if( !rc ) exit(1);
         }
      }
   }
   exit(0);
}
