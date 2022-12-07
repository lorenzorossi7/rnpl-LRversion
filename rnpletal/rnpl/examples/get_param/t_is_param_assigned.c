#include <stdio.h>
#include <bbhutil.h>
#include <stdlib.h> //LR: this is needed by modern compilers

/* 
 *   Test RNPL parameter-fetching utility routines
 *      is_param_assigned_in_file
 *      is_param_assigned_in_file_nc
 */
int main(int argc, char **argv) {
   char   *P = argv[0];
   char   *pfile;

   FILE   *fp;

   double  realpar;

   int     slen = 128;
   int     iveclen = 128;
   int     i, rc;

   if( argc < 2 ) goto Usage;
   if( ! (fp = fopen(argv[1],"r")) ) {
      fprintf(stderr,"%s: Error opening file '%s' for read.\n",P,argv[1]);
      exit(1);
   }
   fclose(fp);
   pfile = argv[1];

   rc = is_param_assigned_in_file(pfile,"eps_dis");
   fprintf(stderr,"%s: is_param_assigned_in_file(pfile,'eps_dis') returns %d\n",
      P,rc);

   rc = is_param_assigned_in_file_nc(pfile,"eps_dis");
   fprintf(stderr,"%s: is_param_assigned_in_file_nc(pfile,'eps_dis') returns %d\n",
      P,rc);

   exit(1);

Usage:
   fprintf(stderr,"usage: %s parameter-file\n",P);
   exit(1);
}

