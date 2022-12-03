#include <stdio.h>
#include <stdlib.h>
#include <malloc/malloc.h> //Modified by Lorenzo Rossi to be able to compile on Mac OS High Sierra. Originally it used to be #include <malloc.h>
#include <bbhutil.h>

/* ----------------------------------------------------
Tests RNPL vector parameter-fetching routines.

Sample build and execution ...

% make
cc -O -I/usr/local/include -c t_get_param_v.c
cc -O -L/usr/local/lib t_get_param_v.o -lbbhutil -lm -o t_get_param_v

% cat param 

vstr1 := [ "a" "bc" "def" "ghij" "klmno" "pqrstu"]
vstr3 := q
vint1 := [ 1 1 2 3 5 8 13 ]
vint3 := [ 1.0 2.0 ]
vreal1 := [ 3.1 4.1 5.9 2.6 5.3 5.8 9.7 9.3 ]
vreal3 := ["a" "b" ]

% t_get_param_v param

+++ t_get_param_v: Testing 'get_int_param_v'
get_int_param_v(...,"vint1",...) returns 1
   Number of ints read: 7
   vint[0]=1
   vint[1]=1
   vint[2]=2
   vint[3]=3
   vint[4]=5
   vint[5]=8
   vint[6]=13
get_int_param_v(...,"vint2",...) returns 0
get_int_param_v(...,"vint3",...) returns -1

+++ t_get_param_v: Testing 'get_real_param_v'
get_int_param_v(...,"vreal1",...) returns 1
   Number of reals read: 8
   vreal[0]=3.1
   vreal[1]=4.1
   vreal[2]=5.9
   vreal[3]=2.6
   vreal[4]=5.3
   vreal[5]=5.8
   vreal[6]=9.7
   vreal[7]=9.3
get_real_param_v(...,"vreal2",...) returns 0
get_real_param_v(...,"vreal3",...) returns -1

+++ t_get_param_v: Testing 'get_str_param_v'
get_str_param_v(...,"vstr1",...) returns 1
   Number of strings read: 6
   vstr[0]='a'
   vstr[1]='bc'
   vstr[2]='def'
   vstr[3]='ghij'
   vstr[4]='klmno'
   vstr[5]='pqrstu'
get_str_param_v(...,"vstr2",...) returns 0
get_str_param_v(...,"vstr3",...) returns -1

-----------------------------------------------------*/

int main(int argc, char **argv) {
   char   *P = argv[0];
   char   *pfile;

   FILE   *fp;

   char  **vstr = (char **) NULL; 
   int     nvstr;
   int    *vint = (int *) NULL;
   int     nvint;
   double *vreal = (double *) NULL;
   int     nvreal;

   int     i,  rc;

   if( argc < 2 ) goto Usage;
   if( ! (fp = fopen(argv[1],"r")) ) {
      fprintf(stderr,"%s: Error opening file '%s' for read.\n",P,argv[1]);
      exit(1);
   }
   fclose(fp);
   pfile = argv[1];

   fprintf(stderr,"\n+++ %s: Testing 'get_int_param_v'\n",P);

   rc = get_int_param_v(pfile,"vint1",&vint,&nvint);
   fprintf(stderr,"get_int_param_v(...,\"vint1\",...) returns %d\n",rc);
   if( rc == 1 ) {
      fprintf(stderr,"   Number of ints read: %d\n",nvint);
      for( i = 0; i < nvint; i++ ) {
         fprintf(stderr,"   vint[%d]=%d\n",i,vint[i]);
      }
      if( vint ) free(vint);
   }

   rc = get_int_param_v(pfile,"vint2",&vint,&nvint);
   fprintf(stderr,"get_int_param_v(...,\"vint2\",...) returns %d\n",rc);

   rc = get_int_param_v(pfile,"vint3",&vint,&nvint);
   fprintf(stderr,"get_int_param_v(...,\"vint3\",...) returns %d\n",rc);

   fprintf(stderr,"\n+++ %s: Testing 'get_real_param_v'\n",P);

   rc = get_real_param_v(pfile,"vreal1",&vreal,&nvreal);
   fprintf(stderr,"get_int_param_v(...,\"vreal1\",...) returns %d\n",rc);
   if( rc == 1 ) {
      fprintf(stderr,"   Number of reals read: %d\n",nvreal);
      for( i = 0; i < nvreal; i++ ) {
         fprintf(stderr,"   vreal[%d]=%g\n",i,vreal[i]);
      }
      if( vreal ) free(vreal);
   }

   rc = get_real_param_v(pfile,"vreal2",&vreal,&nvreal);
   fprintf(stderr,"get_real_param_v(...,\"vreal2\",...) returns %d\n",rc);

   rc = get_real_param_v(pfile,"vreal3",&vreal,&nvreal);
   fprintf(stderr,"get_real_param_v(...,\"vreal3\",...) returns %d\n",rc);

   fprintf(stderr,"\n+++ %s: Testing 'get_str_param_v'\n",P);

   rc = get_str_param_v(pfile,"vstr1",&vstr,&nvstr);
   fprintf(stderr,"get_str_param_v(...,\"vstr1\",...) returns %d\n",rc);
   if( rc == 1 ) {
      fprintf(stderr,"   Number of strings read: %d\n",nvstr);
      for( i = 0; i < nvstr; i++ ) {
         fprintf(stderr,"   vstr[%d]='%s'\n",i,vstr[i]);
         if( vstr[i] ) free(vstr[i]);
      }
      if( vstr ) free(vstr);
   }

   rc = get_str_param_v(pfile,"vstr2",&vstr,&nvstr);
   fprintf(stderr,"get_str_param_v(...,\"vstr2\",...) returns %d\n",rc);

   rc = get_str_param_v(pfile,"vstr3",&vstr,&nvstr);
   fprintf(stderr,"get_str_param_v(...,\"vstr3\",...) returns %d\n",rc);

   exit(0);

Usage:
   fprintf(stderr,"usage: %s parameter-file\n",P);
   exit(1);
}
