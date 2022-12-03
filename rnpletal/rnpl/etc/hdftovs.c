/*
  hdftovs fname_sten [-n func_name]
  Sends a 1d rnpl hdf file to vs
  Copyright (c) 1995,1996,1997 by Robert L. Marsa
*/

#include <stdlib.h>
#include <stdio.h>
#include <librnpl.h>

#define CHUNK_SIZE (32 * (1024*1024))

main(int argc, char **argv)
{
  int rank,shape[1],lev,*start,nlvls,nv,res,unam=0;
  double time,*data,*coords,*tvec,*x,*y,*tv;
  char cnames[30],nm[128],fname[128],oname[128];
  
  if(argc != 2 && argc != 4){
    fprintf(stderr,"Usage: hdftovs fname_stem [-n func_name]\n");
    exit(1);
  }
  if(argc==4 && strcmp(argv[2],"-n")){
    fprintf(stderr,"hdftovs: illegal option <%s>\n",argv[2]);
    fprintf(stderr,"Usage: hdftovs fname_stem [-n func_name]\n");
    exit(1);
  }else if(argc==4){
    unam=1;
    strcpy(oname,argv[3]);
  }
  gft_set_single_hdf(argv[1]);
  if(!gft_read_hdf_name(argv[1],1,fname)){
    fprintf(stderr,"hdftovs: Can't read name from <%s>\n",argv[1]);
    exit(1);
  }
  if(!unam){
    strcpy(oname,fname);
  }
  if(!gft_read_hdf_rank(fname,1,&rank)){
    fprintf(stderr,"hdftovs: Can't read rank for <%s>\n",fname);
    exit(1);
  }
  if(rank>1){
    fprintf(stderr,"hdftovs: Treatment of rank-%d data sets is not yet implemented.\n",rank);
    exit(1);
  }    
  if(!gft_read_hdf_shape(fname,1,shape)){
    fprintf(stderr,"hdftovs: Can't read shape for <%s>\n",fname);
    exit(1);
  }
  nv=CHUNK_SIZE/shape[0];
  if(nv==0){
    fprintf(stderr,"hdftovs: vector too long.\n");
    exit(1);
  }
  x=vec_alloc(CHUNK_SIZE);
  y=vec_alloc(CHUNK_SIZE);
  start=ivec_alloc(nv+1);
  tvec=vec_alloc(nv);
  for(lev=0;lev<nv;lev++)
    start[lev]=lev*shape[0]+1;
  start[lev]=1;
  data=y;
  coords=x;
  if(!gft_read_full(fname,1,shape,cnames,1,&time,coords,data)){
    fprintf(stderr,"hdftovs: Can't read first time level\n");
    exit(1);
  }
  sprintf(nm,"%s(%s)",oname,cnames);
  lev=1;
  do{
    data=y;
    coords=x;
    tv=tvec;
    nlvls=0;
    do{
      if(res=gft_read_full(fname,lev,shape,cnames,1,tv,coords,data)){
        tv++;
        data+=shape[0];
        coords+=shape[0];
        lev++,nlvls++;
      }
    }while(nlvls<nv-1 && res);
#ifdef HAVE_LIBVS
    vsmxynt(nm,start,tvec,nlvls,x,y);
#endif
  }while(res);
  gft_close_all();
  free(nm);
  free(x);
  free(y);
  free(start);
  free(tvec);
}
