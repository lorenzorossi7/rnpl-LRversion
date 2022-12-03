#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <bbhutil.h>
#include <sdf_priv.h>

double dmin(int size, double *data)
{
  double dm=0.0;
  int i;
  
  if(size>0)
    for(dm=data[0],i=1;i<size;i++)
      if(data[i]<dm)
        dm=data[i];
  return dm;
}

double dmax(int size, double *data)
{
  double dm=0.0;
  int i;
  
  if(size>0)
    for(dm=data[0],i=1;i<size;i++)
      if(data[i]>dm)
        dm=data[i];
  return dm;
}

main(int argc, char **argv)
{
  int rank,csize,dsize,version,*shape=NULL;
  int i;
  double time,*data=NULL,*coords=NULL,*bbox=NULL;
  char *name=NULL,*cnames=NULL,*tag=NULL;
  char fname[256];
  gft_sdf_file_data *gp;
  int ltrace = 0;
  
  if(argc!=2){
    printf("usage: sdfinfo file_name\n");
    exit(0);
  }
  strcpy(fname,argv[1]);

  if( !gft_is_sdf_file(fname) ) {
     fprintf(stderr,"sdfinfo: '%s' does not specify an .sdf file.\n",fname);
     exit(1);
  }

  gft_set_single(fname);
  gp=gft_open_sdf_stream(fname);
  if(gp==NULL){
    gft_set_multi();
    gft_set_single(gft_make_sdf_name(fname));
    if( ltrace ) {
      fprintf(stderr,"gft_make_sdf_name(%s) returns '%s'\n",fname,
              gft_make_sdf_name(fname));
    }
  }
  i=1;
  while(mid_read_sdf_stream(1,fname,&time,&version,&rank,&dsize,&csize,&name,
                          &cnames,&tag,&shape,&bbox,&coords,&data)){
    printf("Data set %d\n",i);
    printf("name=<%s>\n",name);
    printf("version=%d\n",version);
    printf("time=%g\n",time);
    printf("rank=%d\n",rank);
      switch(rank){
        case 1:
          printf("shape=(%d)\n",shape[0]);
          if( bbox ) {
            printf("bbox=(%g,%g)\n",bbox[0],bbox[1]);
          } else {
            printf("bbox=NULL\n");
          }
          break;
        case 2:
          printf("shape=(%d,%d)\n",shape[0],shape[1]);
          if( bbox ) {
            printf("bbox=(%g,%g,%g,%g)\n",bbox[0],bbox[1],bbox[2],bbox[3]);
          } else {
            printf("bbox=NULL\n");
          }
          break;
        case 3:
          printf("shape=(%d,%d,%d)\n",shape[0],shape[1],shape[2]);
          if( bbox ) {
            printf("bbox=(%g,%g,%g,%g,%g,%g)\n",bbox[0],bbox[1],bbox[2],bbox[3],
                    bbox[4],bbox[5]);
          } else {
            printf("bbox=NULL\n");
          }
          break;
        default:
          break;
      }
    printf("csize=%d\n",csize);
    printf("dsize=%d\n",dsize);
    printf("cnames=(%s)\n",cnames ? cnames : "NULL");
    printf("tag=<%s>\n",tag ? tag : "NULL");
    printf("|data|=%g, min(data)=%g, max(data)=%g\n",l2norm(dsize,data),
    dmin(dsize,data),dmax(dsize,data));
    i++;
    if(shape)free(shape);
    if(data)free(data);
    if(coords)free(coords);
    if(bbox)free(bbox);
    if(name)free(name);
    if(cnames)free(cnames);
    if(tag)free(tag);
  }
  gft_close_all_sdf();
}
