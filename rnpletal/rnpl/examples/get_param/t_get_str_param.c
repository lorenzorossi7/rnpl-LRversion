#include <stdio.h>
#include <bbhutil.h>
#include <stdlib.h> //LR: this is needed by modern compilers

int main(int argc, char **argv) {
   FILE   *fp;
   char  **svec;

   int     slen = 128;
   int     i, rc;

   if( argc < 2 ) goto Usage;
   if( ! (fp = fopen(argv[1],"r")) ) {
      fprintf(stderr,"%s: Error opening file '%s' for read.\n",argv[0],argv[1]);
      exit(1);
   }
   fclose(fp);

   svec = (char **) malloc(3 * sizeof(char *));
   for( i = 0; i < 3; i++ ) svec[i] = (char *) malloc(slen * sizeof(char));

   rc = get_str_param(argv[1],"vstr",svec,3);
   fprintf(stderr,"%s: sget_str_param(...) returns %d\n",argv[0],rc);
   for( i = 0; i < 3; i++ ) fprintf(stderr,"   vstr[%d]='%s'\n",i,svec[i]);

   exit(0);

Usage:
   fprintf(stderr,"usage: %s parameter-file\n",argv[0]);
   exit(1);
}
