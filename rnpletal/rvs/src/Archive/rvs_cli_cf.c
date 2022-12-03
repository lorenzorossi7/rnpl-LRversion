#include <stdio.h>    
#include <string.h>    
#include "rvs_cli_headers.h"

char *_STRING_cvt(char *in,int n) {
   char *out;
   int   i;
   out = (char *) malloc((n + 1) * sizeof(char));
   strncpy(out,in,n);
   out[n] = '\0';

   for(i = n - 1;i >= 0 && out[i] == ' ';i--);
   if(i >= 0 && i < n) out[i+1] = '\0';

   return out;
}

char **_VSTRING_cvt(char *in,int elemlen,int ns) {
   char **out;
   int  i,  j;
   out = (char **) malloc(ns * sizeof(char *));
   for( i = 0; i < ns; i++ ) {
      out[i] = (char *)  malloc((elemlen + 1) * sizeof(char));
      strncpy(out[i],in + i * elemlen,elemlen);
      out[i][elemlen] = '\0';
      for(j = elemlen - 1; j >= 0 && out[i][j] == ' '; j--);
      if( j >= 0 && j < elemlen ) out[i][j+1] = '\0';
   }
   return out;
}

void _STRCPY_cvt(char *out,char *in,int n) {
   strncpy(out,in,strlen(in));
}

void _VSTRCPY_cvt(char *out,char **in,int elemlen,int ns) {
   int  i;
   for( i = 0; i < ns; i++ ) {
      strncpy(out + i * elemlen,in[i],strlen(in[i]));
   }
}

void _free_VSTRING_cvt(char **s,int ns) {
   int    i;
   for( i = 0; i < ns; i++ ) {
      if( s[i] ) free(s[i]);
   }
   if( s ) free(s);
}

int vsconnect_( void  ) {

   int _mfi_rval;
   _mfi_rval = vsconnect(
      );
   return _mfi_rval;
}

int vsopen_(
   char * name,
   int len_name ) {

   int _mfi_rval;
   char * l_name;

   l_name = _STRING_cvt(name,len_name);

   _mfi_rval = vsopen(
      l_name      );

   _STRCPY_cvt(name,l_name,len_name);


   free(l_name);

   return _mfi_rval;
}

int vsxy1_(
   int * p_wt,
   double * p_time,
   DVEC x,
   DVEC y,
   int * p_n ) {

   int _mfi_rval;
   _mfi_rval = vsxy1(
      *p_wt,
      *p_time,
      x,
      y,
      *p_n      );
   return _mfi_rval;
}

int vsxy2_(
   int * p_wt,
   double * p_time,
   int * p_tag,
   DVEC x,
   DVEC y,
   int * p_n ) {

   int _mfi_rval;
   _mfi_rval = vsxy2(
      *p_wt,
      *p_time,
      *p_tag,
      x,
      y,
      *p_n      );
   return _mfi_rval;
}

int vsclose_(
   int * p_wt ) {

   int _mfi_rval;
   _mfi_rval = vsclose(
      *p_wt      );
   return _mfi_rval;
}

int vshang_( void  ) {

   int _mfi_rval;
   _mfi_rval = vshang(
      );
   return _mfi_rval;
}

int vsstatus_(
   int * p_wt ) {

   int _mfi_rval;
   _mfi_rval = vsstatus(
      *p_wt      );
   return _mfi_rval;
}

int vsgetvspid_( void  ) {

   int _mfi_rval;
   _mfi_rval = vsgetvspid(
      );
   return _mfi_rval;
}

int vsreset_( void  ) {

   int _mfi_rval;
   _mfi_rval = vsreset(
      );
   return _mfi_rval;
}

int vskillall_( void  ) {

   int _mfi_rval;
   _mfi_rval = vskillall(
      );
   return _mfi_rval;
}

int vssetmaxlxy_(
   int * p_wt,
   int * p_lxy ) {

   int _mfi_rval;
   _mfi_rval = vssetmaxlxy(
      *p_wt,
      *p_lxy      );
   return _mfi_rval;
}

int vsgetparameter_(
   int * p_wt,
   int * p_code ) {

   int _mfi_rval;
   _mfi_rval = vsgetparameter(
      *p_wt,
      *p_code      );
   return _mfi_rval;
}

int vssetparameter_(
   int * p_wt,
   int * p_code,
   int * p_value ) {

   int _mfi_rval;
   _mfi_rval = vssetparameter(
      *p_wt,
      *p_code,
      *p_value      );
   return _mfi_rval;
}

int vssetaf_(
   int * p_wt,
   int * p_af ) {

   int _mfi_rval;
   _mfi_rval = vssetaf(
      *p_wt,
      *p_af      );
   return _mfi_rval;
}

int vssetthin_(
   int * p_wt ) {

   int _mfi_rval;
   _mfi_rval = vssetthin(
      *p_wt      );
   return _mfi_rval;
}

int vsnameq_(
   char * name,
   int len_name ) {

   int _mfi_rval;
   char * l_name;

   l_name = _STRING_cvt(name,len_name);

   _mfi_rval = vsnameq(
      l_name      );

   _STRCPY_cvt(name,l_name,len_name);


   free(l_name);

   return _mfi_rval;
}

int vsglt_(
   int * p_wt ) {

   int _mfi_rval;
   _mfi_rval = vsglt(
      *p_wt      );
   return _mfi_rval;
}

int vsxyn_(
   char * name,
   DVEC x,
   DVEC y,
   int * p_n,
   int len_name ) {

   int _mfi_rval;
   char * l_name;

   l_name = _STRING_cvt(name,len_name);

   _mfi_rval = vsxyn(
      l_name,
      x,
      y,
      *p_n      );

   _STRCPY_cvt(name,l_name,len_name);


   free(l_name);

   return _mfi_rval;
}

int vsxynt_(
   char * name,
   double * p_time,
   DVEC x,
   DVEC y,
   int * p_n,
   int len_name ) {

   int _mfi_rval;
   char * l_name;

   l_name = _STRING_cvt(name,len_name);

   _mfi_rval = vsxynt(
      l_name,
      *p_time,
      x,
      y,
      *p_n      );

   _STRCPY_cvt(name,l_name,len_name);


   free(l_name);

   return _mfi_rval;
}

int vsxytagnt_(
   char * name,
   double * p_time,
   DVEC x,
   DVEC y,
   IVEC tag,
   int * p_n,
   int len_name ) {

   int _mfi_rval;
   char * l_name;

   l_name = _STRING_cvt(name,len_name);

   _mfi_rval = vsxytagnt(
      l_name,
      *p_time,
      x,
      y,
      tag,
      *p_n      );

   _STRCPY_cvt(name,l_name,len_name);


   free(l_name);

   return _mfi_rval;
}

int vssxynt_(
   char * name,
   float time,
   SVEC x,
   SVEC y,
   int * p_n,
   int len_name ) {

   int _mfi_rval;
   char * l_name;

   l_name = _STRING_cvt(name,len_name);

   _mfi_rval = vssxynt(
      l_name,
      time,
      x,
      y,
      *p_n      );

   _STRCPY_cvt(name,l_name,len_name);


   free(l_name);

   return _mfi_rval;
}

int vsxn_(
   char * name,
   DVEC x,
   int * p_n,
   int len_name ) {

   int _mfi_rval;
   char * l_name;

   l_name = _STRING_cvt(name,len_name);

   _mfi_rval = vsxn(
      l_name,
      x,
      *p_n      );

   _STRCPY_cvt(name,l_name,len_name);


   free(l_name);

   return _mfi_rval;
}

int vsxnt_(
   char * name,
   double * p_time,
   DVEC x,
   int * p_n,
   int len_name ) {

   int _mfi_rval;
   char * l_name;

   l_name = _STRING_cvt(name,len_name);

   _mfi_rval = vsxnt(
      l_name,
      *p_time,
      x,
      *p_n      );

   _STRCPY_cvt(name,l_name,len_name);


   free(l_name);

   return _mfi_rval;
}

int vsmxynt_(
   char * name,
   IVEC start,
   DVEC time,
   int * p_nvec,
   DVEC x,
   DVEC y,
   int len_name ) {

   int _mfi_rval;
   char * l_name;

   l_name = _STRING_cvt(name,len_name);

   _mfi_rval = vsmxynt(
      l_name,
      start,
      time,
      *p_nvec,
      x,
      y      );

   _STRCPY_cvt(name,l_name,len_name);


   free(l_name);

   return _mfi_rval;
}

int vsgnxyni_(
   char * name,
   int * p_index,
   int len_name ) {

   int _mfi_rval;
   char * l_name;

   l_name = _STRING_cvt(name,len_name);

   _mfi_rval = vsgnxyni(
      l_name,
      *p_index      );

   _STRCPY_cvt(name,l_name,len_name);


   free(l_name);

   return _mfi_rval;
}

int vsgxyni_(
   char * name,
   int * p_index,
   DVEC x,
   DVEC y,
   Pint pn,
   Pdouble ptime,
   int len_name ) {

   int _mfi_rval;
   char * l_name;

   l_name = _STRING_cvt(name,len_name);

   _mfi_rval = vsgxyni(
      l_name,
      *p_index,
      x,
      y,
      pn,
      ptime      );

   _STRCPY_cvt(name,l_name,len_name);


   free(l_name);

   return _mfi_rval;
}

int vsnsct_(
   char * name,
   double * p_sc,
   double * p_time,
   int len_name ) {

   int _mfi_rval;
   char * l_name;

   l_name = _STRING_cvt(name,len_name);

   _mfi_rval = vsnsct(
      l_name,
      *p_sc,
      *p_time      );

   _STRCPY_cvt(name,l_name,len_name);


   free(l_name);

   return _mfi_rval;
}

int vsgxynall_(
   char * name,
   DVEC x,
   DVEC y,
   IVEC N,
   DVEC Time,
   int len_name ) {

   int _mfi_rval;
   char * l_name;

   l_name = _STRING_cvt(name,len_name);

   _mfi_rval = vsgxynall(
      l_name,
      x,
      y,
      N,
      Time      );

   _STRCPY_cvt(name,l_name,len_name);


   free(l_name);

   return _mfi_rval;
}

int vsgetparametern_(
   char * name,
   int * p_which_parameter,
   int len_name ) {

   int _mfi_rval;
   char * l_name;

   l_name = _STRING_cvt(name,len_name);

   _mfi_rval = vsgetparametern(
      l_name,
      *p_which_parameter      );

   _STRCPY_cvt(name,l_name,len_name);


   free(l_name);

   return _mfi_rval;
}

int vsgetlxyn_(
   char * name,
   int len_name ) {

   int _mfi_rval;
   char * l_name;

   l_name = _STRING_cvt(name,len_name);

   _mfi_rval = vsgetlxyn(
      l_name      );

   _STRCPY_cvt(name,l_name,len_name);


   free(l_name);

   return _mfi_rval;
}

