#include <stdio.h>
#include <stdlib.h>
#include <malloc/malloc.h> //LR: this, instead of the original #include <malloc.h>, seems to be needed by modern compilers
#include <bbhutil.h>

/* 
 *   Test RNPL parameter-fetching routines
 */
int main(int argc, char **argv) {
   char   *P = argv[0];
   char   *pfile;

   FILE   *fp;

   char  **svec = (char **) NULL;
   int    *ivec = (int *) NULL;

   int     intpar;
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

   svec = (char **) malloc(3 * sizeof(char *));
   for( i = 0; i < 3; i++ ) svec[i] = (char *) malloc(slen * sizeof(char));
   ivec = (int *) malloc(iveclen * sizeof(int));

   fprintf(stderr,"\n+++ %s Testing 'get_int_param'\n",P);

   intpar = -999;
   rc = get_int_param(pfile,"intpar1",&intpar,1);
   fprintf(stdout,"%s: get_int_param(%s,%s,...) returns %d\n",P,pfile,"intpar1",rc);
   fprintf(stdout,"   intpar1=%d\n",intpar);

   intpar = -999;
   rc = get_int_param(pfile,"intpar2",&intpar,1);
   fprintf(stderr,"%s: get_int_param(%s,%s,...) returns %d\n",P,pfile,"intpar2",rc);
   fprintf(stderr,"   intpar2=%d\n",intpar);

   intpar = -999;
   rc = get_int_param(pfile,"intpar3",&intpar,1);
   fprintf(stderr,"%s: get_int_param(%s,%s,...) returns %d\n",P,pfile,"intpar3",rc);
   fprintf(stderr,"   intpar3=%d\n",intpar);

   fprintf(stderr,"\n+++ %s Testing 'get_real_param'\n",P);

   realpar = -999.0;
   rc = get_real_param(pfile,"realpar1",&realpar,1);
   fprintf(stdout,"%s: get_real_param(%s,%s,...) returns %d\n",P,pfile,"realpar1",rc);
   fprintf(stdout,"   realpar1=%g\n",realpar);

   realpar = -999.0;
   rc = get_real_param(pfile,"realpar2",&realpar,1);
   fprintf(stderr,"%s: get_real_param(%s,%s,...) returns %d\n",P,pfile,"realpar2",rc);
   fprintf(stderr,"   realpar2=%g\n",realpar);

   realpar = -999.0;
   rc = get_real_param(pfile,"realpar3",&realpar,1);
   fprintf(stderr,"%s: get_real_param(%s,%s,...) returns %d\n",P,pfile,"realpar3",rc);
   fprintf(stderr,"   realpar3=%g\n",realpar);

   fprintf(stderr,"\n+++ %s Testing 'get_str_param'\n",P);

   for( i = 0; i < 3; i++ ) strcpy(svec[i],"");
   rc = get_str_param(pfile,"vstr1",svec,3);
   fprintf(stderr,"%s: get_str_param(%s,%s,...) returns %d\n",P,pfile,"vstr1",rc);
   for( i = 0; i < 3; i++ ) fprintf(stderr,"   vstr1[%d]='%s'\n",i,svec[i]);

   for( i = 0; i < 3; i++ ) strcpy(svec[i],"");
   rc = get_str_param(pfile,"vstr2",svec,3);
   fprintf(stderr,"%s: get_str_param(%s,%s,...) returns %d\n",P,pfile,"vstr2",rc);
   for( i = 0; i < 3; i++ ) fprintf(stderr,"   vstr2[%d]='%s'\n",i,svec[i]);

   for( i = 0; i < 3; i++ ) strcpy(svec[i],"");
   rc = get_str_param(pfile,"vstr3",svec,3);
   fprintf(stderr,"%s: get_str_param(%s,%s,...) returns %d\n",P,pfile,"vstr3",rc);
   for( i = 0; i < 3; i++ ) fprintf(stderr,"   vstr3[%d]='%s'\n",i,svec[i]);

   fprintf(stderr,"\n+++ %s Testing 'get_ivec_param'\n",P);

   for( i = 0; i < 3; i++ ) ivec[i] = -999;
   rc = get_ivec_param(pfile,"ivec1",ivec,iveclen);
   fprintf(stderr,"%s: get_ivec_param(%s,%s,...} returns %d\n",P,pfile,"ivec1",rc);
   for( i = 0; i < 3; i++ ) fprintf(stderr,"   ivec1[%d]='%d'\n",i,ivec[i]);

   for( i = 0; i < 3; i++ ) ivec[i] = -999;
   rc = get_ivec_param(pfile,"ivec2",ivec,iveclen);
   fprintf(stderr,"%s: get_ivec_param(%s,%s,...} returns %d\n",P,pfile,"ivec2",rc);
   for( i = 0; i < 3; i++ ) fprintf(stderr,"   ivec2[%d]='%d'\n",i,ivec[i]);

   for( i = 0; i < 3; i++ ) ivec[i] = -999;
   rc = get_ivec_param(pfile,"ivec3",ivec,iveclen);
   fprintf(stderr,"%s: get_ivec_param(%s,%s,...} returns %d\n",P,pfile,"ivec3",rc);
   for( i = 0; i < 3; i++ ) fprintf(stderr,"   ivec3[%d]='%d'\n",i,ivec[i]);

   exit(0);

Usage:
   fprintf(stderr,"usage: %s parameter-file\n",P);
   exit(1);
}
