/*
  hdfinfo file_name [-v]
  prints [verbose] information about an hdf file
  Copyright (c) 1995,1996,1997 by 
  Robert L. Marsa and Matthew W. Choptuik
*/

#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <string.h>
#include <time.h>
#include <signal.h>
#include <mfhdf.h>

#define NV(v,n) if( !(v = make_DVEC(n)) ) goto Malloc

typedef double * DVEC;

int32 ivprod(int32 *v,int32 n) {
  int32   rval = 0, i;

  for( rval = 1, i = 0; i < n; i++ ) rval *= v[i];

  return rval;
}

DVEC make_DVEC(int32  n) {
   return (DVEC) malloc(n * sizeof(double));
}

void free_DVEC(DVEC v) {
   if( v != (DVEC) NULL ) {
      free(v);
   }
}

double dvmin(double *v,int32 n) {
  double   rval = 0.0;
  int32    i;

  if( n > 0 ) {
    rval = v[0];
    for( i = 1; i < n; i++ ) {
      if( v[i] < rval ) rval = v[i];
    }
  } 
  return rval;
}

double dvmax(double *v,int32 n) {
  double   rval = 0.0;
  int32    i;

  if( n > 0 ) {
    rval = v[0];
    for( i = 1; i < n; i++ ) {
      if( v[i] > rval ) rval = v[i];
    }
  } 
  return rval;

}

void fprintf_extrema(FILE *fp,char *s,double *v,int32 n) {
  fprintf(fp,"%s: Minimum: %g  Maximum: %g\n",s,dvmin(v,n),dvmax(v,n));
}

main(int32 argc, char **argv)
{
  int32 fid,sds_id;
  int32 start[4] = {0,0,0,0}, size[4];
  int32 rank,i;
  int32 nslabs,ind;
  int32 type,gattrs,attrs,sz,dim_id,dattrs;
  char name[60];
  double *data;
  int32 verbose;
  char buffer[1024];
  int32 ndata;
  
  if(argc < 2) goto Usage;

  verbose = 0;
  if( !strcmp(argv[1],"-v") ) {
    if( argc < 3 ) goto Usage;
    argv++;
    verbose = 1;
  } 

  fid=SDstart(argv[1],DFACC_RDONLY);
  if(fid==-1){
    fprintf(stderr,"Unable to open %s.\n",argv[1]);
    exit(0);
  }
  if(SDfileinfo(fid,(int32 *)&nslabs,(int32 *)&gattrs)==-1){
    fprintf(stderr,"Can't get file info.\n");
    exit(0);
  }
  printf("%s contains %d data sets and %d attributes.\n",argv[1],nslabs,gattrs);
  printf("Scientific Data Sets:\n");
  for(ind=0;ind<nslabs;ind++){
    if((sds_id=SDselect(fid,ind))==-1){
      fprintf(stderr,"Can't select sds number %d.\n",ind);
      exit(0);
    }
    if(SDgetinfo(sds_id,name,(int32 *)&rank,(int32 *)size,
                              (int32 *)&type,(int32 *)&attrs)==-1){
      fprintf(stderr,"Can't get info about 1st sds\n");
      exit(0);
    }
    printf("%s has rank %d size (",name,rank);
    for(i=0;i<rank;i++){
      printf("%d ",size[i]);
    }
    printf(")\n");
    if( verbose && (type == DFNT_FLOAT64) ) {
    ndata = ivprod(size,rank);
      NV(data,ndata);
      if( SDreaddata(sds_id,(int32 *)start,NULL,(int32 *)size,data) == -1 ) {
      fprintf(stderr,"Can't read data for sds\n");
    } else {
      sprintf(buffer,"Data info for sds <%s>",name);
         fprintf_extrema(stdout,buffer,data,ndata);
     }
    free_DVEC(data);
     }
    for(i=0;i<rank;i++){
      dim_id=SDgetdimid(sds_id,i);
      if(dim_id!=-1){
        if(SDdiminfo(dim_id,name,(int32 *)&sz,
                                  (int32 *)&type,(int32 *)&dattrs)==-1){
          fprintf(stderr,"Can't get dimension info.\n");
          exit(0);
        }
        if(type!=0) {
          printf("  has dim %s id: %d\n",name,dim_id);
/*         If verbose extract scale information and dump extremities ... */
        if( verbose ) {
             NV(data,sz);
             if( SDgetdimscale(dim_id,data) == -1 ) {
          fprintf(stderr,"Can't extract scale info for dimension %d\n",i);
         } else {
          sprintf(buffer,"Scale info for dimension %d",i);
          fprintf_extrema(stdout,buffer,data,sz);
             }
             free_DVEC(data);
           }
         }
      }
    }
    printf("  type: %d  attributes: %d\n",type,attrs);
    for(i=0;i<attrs;i++){
      SDattrinfo(sds_id,i,name,(int32 *)&type,(int32 *)&sz);
      printf("    name: %s  type: %d  count: %d\n",name,type,sz);
    }
    SDendaccess(sds_id);
  }
  printf("Global Attributes:\n");
  for(i=0;i<gattrs;i++){
    SDattrinfo(fid,i,name,(int32 *)&type,(int32 *)&sz);
    printf("%s  type: %d  count: %d\n",name,type,sz);
  }
  SDend(fid);
  exit(0);
Usage:
    fprintf(stderr,"Usage: hdfinfo [-v] file_name\n");
    exit(1);
Malloc:
  fprintf(stderr,"hdfinfo: Unexpected malloc() failure\n");
   exit(1);
}
