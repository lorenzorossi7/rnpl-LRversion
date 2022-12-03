/*
  sdftosv 
  Sends an rnpl sdf file to scivis
  Copyright (c) 1997 by Robert L. Marsa
*/

#include <stdlib.h>
#include <stdio.h>
#include <bbhutil.h>
#include <sdf_priv.h>

#define VERSION "1.0"
#define CHUNK_SIZE (32 * (1024*1024))

int rjava_ser(char *nm, double time, double *cds, double *data, int size)
{
#ifdef HAVE_LIBSV
  return java_ser(nm,time,cds,data,size);
#else
  return 1;
#endif
}

int rjava_mser(char *nm, int nt, double *time, double *x, double *data, int *size)
{
#ifdef HAVE_LIBSV
  return java_mser(nm,nt,time,x,data,size);
#else
  return 1;
#endif
}

int rjava_bbser(char *nm, double time, double *bb, int size, double *data)
{
#ifdef HAVE_LIBSV
  return java_bbser(nm,time,bb,size,data);
#else
  return 1;
#endif
}

int rjava_bser2d(char *nm, double time, double *x, int xs, double *y, int ys,
                double *data)
{
#ifdef HAVE_LIBSV
  return java_bser2d(nm,time,x,xs,y,ys,data);
#else
  return 1;
#endif
}

int rjava_bmser2d(char *nm, int nt, double *time, double *x, int *nx, double *y,
                  int *ny, double *data)
{
#ifdef HAVE_LIBSV
  return java_bmser2d(nm,nt,time,x,nx,y,ny,data);
#else
  return 1;
#endif
}

int rjava_bbser2d(char *nm, double time, double *bb, int xs, int ys, double *data)
{
#ifdef HAVE_LIBSV
  return java_bbser2d(nm,time,bb,xs,ys,data);
#else
  return 1;
#endif
}

int rjava_ser3d(char *nm, double time, int xs, int ys, int zs, double th, 
               double *data)
{
#ifdef HAVE_LIBSV
  return java_ser3d(nm,time,xs,ys,zs,th,data);
#else
  return 1;
#endif
}

int rjava_mser3d(char *nm, int nt, double *time, int *xs, int *ys, int *zs, 
                 double th, double *data)
{
#ifdef HAVE_LIBSV
  return java_mser3d(nm,nt,time,xs,ys,zs,th,data);
#else
  return 1;
#endif
}

double mean(const double *data, const int len)
{
  int i;
  double s,l;
  
  for(i=0,s=0,l=len;i<len;i++)
    s+=data[i]/l;
  return s;
}

main(int argc, char **argv)
{
  int rank,*shape,nlvls,ntlevs,res,n,unam=0,single=0;
  double time,*data,*coords;
  double *x=NULL,*y=NULL,*z=NULL,*dt=NULL,*tv=NULL,*xp,*yp,*zp,*tp,*dp,*de;
  char *cnames,nm[512],fname[128],oname[128],*name;
  char iv[1024];
  int argerr=0,opt,frank=-1,addcoords=0;
  extern char *optarg;
  extern int optind,opterr,optopt;
  int *output=NULL,*nx=NULL,*ny=NULL,*nz=NULL;
  gft_sdf_file_data *gp;
  int version,csize,dsize,ds,i;
  double *bbox,thresh;
  char *tag;
  int ltrace=0;

  iv[0]=0;
  while((opt=getopt(argc,argv,"ci:n:s"))!=EOF){
    switch(opt){
      case 'c' :
        addcoords=1;
        break;
      case 'i' :
        sprintf(iv,"output:=%s",optarg);
        break;
      case 'n' :
        unam=1;
        strcpy(oname,optarg);
        break;
      case 's' :
        single=1;
        break;
      default :
        argerr=1;
        break;
    }
  }
  if(argerr || argc<2){
    fprintf(stderr,"sdftosv version: %s\n",VERSION);
    fprintf(stderr,"  Copyright (c) 1997 by Robert L. Marsa\n");
    fprintf(stderr,"  sends .sdf files to the scivis visualization server\n");
    fprintf(stderr,"Usage: \n");
    fprintf(stderr,"  sdftosv [ -c ]\n");
    fprintf(stderr,"          [ -i ivec ]\n");
    fprintf(stderr,"          [ -n oname ]\n");
    fprintf(stderr,"          [ -s ]\n");
    fprintf(stderr,"          input_file [ input_file [ ... ] ]\n\n");
    fprintf(stderr,"  -c       -- append coordinate names when naming\n");
    fprintf(stderr,"              function for output\n");
    fprintf(stderr,"  -i ivec  -- use ivec (0 based) for output control\n");
    fprintf(stderr,"  -n oname -- name all data sets oname\n");
    fprintf(stderr,"  -s       -- send data sets one at a time\n");
    fprintf(stderr,"              useful for large or nonuniform data\n");
    fprintf(stderr,"  input_file is an .sdf file\n\n");
    exit(0);
  }
  for(;optind<argc;optind++){
    if(ltrace){
      fprintf(stderr,"sdftosv: argc=%d optind=%d\n",argc,optind);
      fprintf(stderr,"sdftosv: argv[%d]=<%s>\n",optind,argv[optind]);
    }
    strcpy(fname,argv[optind]);
    if(ltrace)
      fprintf(stderr,"sdftosv: fname=<%s>\n",fname);
    gft_set_single(fname);
    gp=gft_open_sdf_stream(fname);
    if(gp==NULL){
      gft_set_multi();
      gft_set_single(gft_make_sdf_name(fname));
      gp=gft_open_sdf_stream(fname);
      if(gp==NULL){
        fprintf(stderr,"sdftosv: Can't open file <%s>\n",fname);
        exit(0);
      }
    }
    gsfd_make_index(gp);
    ntlevs=dynarray_len(gp->tarray);
    if(ntlevs<0){
      fprintf(stderr,"sdftosv: Problem reading file <%s>\n",fname);
      exit(0);
    }
    if(ltrace)
      fprintf(stderr,"sdftosv: ntlevs=%d\n",ntlevs);
      
    if(output)free(output);
    output=ivec_alloc(5);
    output[0]=1;
    output[1]=-1;
    output[2]=-1;
    output[3]=1;
    output[4]=0;
    if(ltrace){
      fprintf(stderr,"sdftosv: ivec: <%s>\n",iv);
      fprintf(stderr,"sdftosv: output=%p iv=%d\n",output,output[0]);
    }
    res=sget_param(iv,"output","ivec",5,&output,1);
    if(res==0){
      fprintf(stderr,"sdftosv: problem parsing ivec <%s>\n",iv);
      exit(0);
    }
    if(ltrace){
      fprintf(stderr,"sdftosv: output=%p iv=%d\n",output,output[0]);
    }
    fixup_ivec(0,ntlevs,0,output);
    
    if(!single){
      if(tv){ free(tv); tv=NULL; }
      if(dt){ free(dt); dt=NULL; }
      if(nx){ free(nx); nx=NULL; }
      if(ny){ free(ny); ny=NULL; }
      if(nz){ free(nz); nz=NULL; }
      if(x){ free(x); x=NULL; }
      if(y){ free(y); y=NULL; }
      if(z){ free(z); z=NULL; }
      tv=vec_alloc(ntlevs);
      dt=vec_alloc(CHUNK_SIZE);
      if(!gft_read_rank(fname,1,&rank)){
        fprintf(stderr,"sdftosv: can't read rank from <%s>\n",fname);
        exit(0);
      }
      rewind(gp->fp);
      if(ltrace)
        fprintf(stderr,"sdftosv: rank=%d\n",rank);
      switch(rank){
        case 1:
          nx=ivec_alloc(ntlevs);
          x=vec_alloc(CHUNK_SIZE);
          break;
        case 2:
          nx=ivec_alloc(ntlevs);
          ny=ivec_alloc(ntlevs);
          x=vec_alloc(CHUNK_SIZE);
          y=vec_alloc(CHUNK_SIZE);
          break;
        case 3:
          nx=ivec_alloc(ntlevs);
          ny=ivec_alloc(ntlevs);
          nz=ivec_alloc(ntlevs);
          x=vec_alloc(CHUNK_SIZE);
          y=vec_alloc(CHUNK_SIZE);
          z=vec_alloc(CHUNK_SIZE);
          break;
        default: fprintf(stderr,"sdftosv: rank=%d is not supported\n",rank);
                 exit(0);
      }
      frank=rank;
      nlvls=0;
      xp=x;
      yp=y;
      zp=z;
      dp=dt;
      tp=tv;
      de=dt+CHUNK_SIZE;
    }
    n=0;
    while(low_read_sdf_stream(1,gp->fp,&time,&version,&rank,&dsize,&csize,&name,
                              &cnames,&tag,&shape,&bbox,&coords,&data)){
      if(do_ivec(n,ntlevs,output)){
        if(single){
          if(!unam){
            strcpy(oname,name);
          }
          if(addcoords)
            sprintf(nm,"%s(%s)",oname,cnames);
          else sprintf(nm,"%s",oname);
          switch(rank){
            case 1:
              if(csize!=2*rank)
                rjava_ser(nm,time,coords,data,dsize);
              else
                rjava_bbser(nm,time,bbox,dsize,data);
              break;
            case 2:
              if(csize!=2*rank)
                rjava_bser2d(nm,time,coords,shape[0],coords+shape[0],shape[1],data);
              else
                rjava_bbser2d(nm,time,bbox,shape[0],shape[1],data);
              break;
            case 3:
              thresh=mean(data,dsize);
              if(csize!=2*rank)
                rjava_ser3d(nm,time,shape[0],shape[1],shape[2],thresh,data);
              else
                fprintf(stderr,"sdftosv: unfortunately, coordinates aren't supported by scivis\n");
              break;
            default:
              break;
          }
        }else{
          if(nlvls==0 && dp+dsize>de){
            fprintf(stderr,"sdftosv: data set is too large.  Try sdftosv -s\n");
            exit(0);
          }else if(dp+dsize>de){
            switch(frank){
              case 1:
                rjava_mser(nm,nlvls,tv,x,dt,nx);
                break;
              case 2:
                rjava_bmser2d(nm,nlvls,tv,x,nx,y,ny,dt);
                break;
              case 3:
                for(i=0,ds=0;i<nlvls;i++)
                  ds+=(nx[i]*ny[i]*nz[i]);
                thresh=mean(dt,ds);
                rjava_mser3d(nm,nlvls,tv,nx,ny,nz,thresh,dt);
                break;
            }
            nlvls=0;
            xp=x;
            yp=y;
            zp=z;
            dp=dt;
            tp=tv;
          }
          if(!unam){
            strcpy(oname,name);
          }
          if(addcoords)
            sprintf(nm,"%s(%s)",oname,cnames);
          else sprintf(nm,"%s",oname);
          if(rank!=frank){
            fprintf(stderr,"sdftosv: the grid functions in this file do not all have the\n");
            fprintf(stderr,"         same rank (%d).  Use sdftosv -s to send the data.\n",frank);
            exit(0);
          }
          rdvcpy(dp,data,dsize);
          *tp=time;
          dp+=dsize;
          tp++;
          switch(frank){
            case 1:
              nx[nlvls]=shape[0];
              if(csize==2*frank)
                rdvramp(xp,shape[0],bbox[0],(bbox[1]-bbox[0])/(shape[0]-1));
              else
                rdvcpy(xp,coords,shape[0]);
              xp+=shape[0];
              break;
            case 2:
              nx[nlvls]=shape[0];
              ny[nlvls]=shape[1];
              if(csize==2*frank){
                rdvramp(xp,shape[0],bbox[0],(bbox[1]-bbox[0])/(shape[0]-1));
                rdvramp(yp,shape[1],bbox[2],(bbox[3]-bbox[2])/(shape[1]-1));
              }else{
                rdvcpy(xp,coords,shape[0]);
                rdvcpy(yp,coords+shape[0],shape[1]);
              }
              xp+=shape[0];
              yp+=shape[1];
              break;
            case 3:
              nx[nlvls]=shape[0];
              ny[nlvls]=shape[1];
              nz[nlvls]=shape[2];
              if(csize==2*frank){
                rdvramp(xp,shape[0],bbox[0],(bbox[1]-bbox[0])/(shape[0]-1));
                rdvramp(yp,shape[1],bbox[2],(bbox[3]-bbox[2])/(shape[1]-1));
                rdvramp(zp,shape[2],bbox[4],(bbox[5]-bbox[4])/(shape[2]-1));
              }else{
                rdvcpy(xp,coords,shape[0]);
                rdvcpy(yp,coords+shape[0],shape[1]);
                rdvcpy(zp,coords+shape[0]+shape[1],shape[2]);
              }
              xp+=shape[0];
              yp+=shape[1];
              zp+=shape[2];
              break;
            default: fprintf(stderr,"sdftosv: rank=%d is not supported\n",rank);
                     exit(0);
          }
          nlvls++;
        }
      }
      n++;
      if(name) free(name);
      if(cnames) free(cnames);
      if(tag) free(tag);
      if(shape) free(shape);
      if(bbox) free(bbox);
      if(coords) free(coords);
      if(data) free(data);
    }
    if(!single && nlvls>0){
      switch(frank){
        case 1:
          if( ltrace ) dump_mser(nm,nlvls,tv,x,dt,nx);
          rjava_mser(nm,nlvls,tv,x,dt,nx);
          break;
        case 2:
          rjava_bmser2d(nm,nlvls,tv,x,nx,y,ny,dt);
          break;
        case 3:
          for(i=0,ds=0;i<nlvls;i++)
            ds+=(nx[i]*ny[i]*nz[i]);
          thresh=mean(dt,ds);
          rjava_mser3d(nm,nlvls,tv,nx,ny,nz,thresh,dt);
          break;
      }
    }
  }
  gft_close_all();

}
int dump_mser(char *nm,int nlvls,double *tv,double *x,double *y,int *nx) {
   int i; 
   double *xp, *yp;
   fprintf(stderr,"dump_ser: nm=<%s>\n",nm);
   fprintf(stderr,"dump_ser: nlvls=<%d>\n",nlvls);
   xp = x; yp = y;
   for( i = 0; i < nlvls; i++ ) {
      fprintf(stderr,"dump_ser: t[%d]=%g nx[%d]=%d\n",i,tv[i],i,nx[i]);
   }
   return 1;
}
