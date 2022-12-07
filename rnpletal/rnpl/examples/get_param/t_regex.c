#include <stdio.h>
#include <sys/types.h>
#include <regex.h>
#include <stdlib.h> //LR: this is needed by modern compilers

/* 
 *   Simple example that uses regex library ...
 */
int main(int argc, char **argv) {
   char    *P = argv[0];

   char    *regex,  *string;

   regex_t    reg;
   int        cflags = 0;
   int        rc;
   int        nmatch = 1;
   regmatch_t match[1];
   int        eflags = 0;

   if( argc < 3 ) goto Usage;
   regex = argv[1];
   string = argv[2];
   fprintf(stderr,"%s: regex='%s' string='%s'\n",P,regex,string);

   rc = regcomp(&reg,regex,cflags);
   fprintf(stderr,"%s: regcomp(...) returns %d\n",P,rc);
   if( !rc ) {
      rc = regexec(&reg,string,nmatch,match,eflags);
      fprintf(stderr,"%s: regexec(...) returns %d\n",P,rc);
   } else {
      fprintf(stderr,"%s: Compilation of regex='%s' failed.\n",P,regex);
   }

   exit(1);

Usage:
   fprintf(stderr,"usage: %s <regex> <string>\n",P);
   exit(1);
}
