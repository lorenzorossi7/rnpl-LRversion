/* Implementation of remote vs procedures ... */

#include <mwcstandards.h>
#include "rvs.h"
#include "vs_constants.h"

int *rvsconnect_1(void)
{
   static  int  rc;

   rc = vsconnect();
   return(&rc);
}

int *rvsopen_1(RVSSTRING_args *args)
{
   static  int  rc;

   rc = vsopen(args->vs_string);
   return(&rc);
}

int *rvsxy1_1(RVSXY1_args *args)
{
   static  int  rc;

   rc = vsxy1(args->wt,args->time,args->x.x_val,args->y.y_val,args->n);
   return(&rc);
}

int *rvsxy2_1(RVSXY2_args *args)
{
   static  int  rc;

   rc = vsxy2(args->wt,args->time,args->tag,
              args->x.x_val,args->y.y_val,args->n);
   return(&rc);
}

int *rvsclose_1(RVS1INT_args *args)
{
   static  int  rc;

   rc = vsclose(args->arg1);
   return(&rc);
}

int *rvshang_1(void)
{
   static  int  rc;

   rc = vshang();   
   return(&rc);
}

int *rvsstatus_1(RVS1INT_args *args)
{
   static  int  rc;

   rc = vsstatus(args->arg1);
   return(&rc);
}

int *rvsgetvspid_1(void)
{
   static  int  rc;

   rc = vsgetvspid();
   return(&rc);
}

int *rvsreset_1(void)
{
   static  int  rc;

   rc = vsreset();
   return(&rc);
}

int *rvskillall_1(void)
{
   static  int  rc;

   rc = vskillall();
   return(&rc);
}

int *rvssetmaxlxy_1(RVS2INT_args *args)
{
   static  int  rc;

   rc = vssetmaxlxy(args->arg1,args->arg2);
   return(&rc);
}

int *rvsgetparameter_1(RVS2INT_args *args)
{
   static  int  rc;

   rc = vsgetparameter(args->arg1,args->arg2);
   return(&rc);
}

int *rvssetparameter_1(RVS3INT_args *args)
{
   static  int  rc;

   rc = vssetparameter(args->arg1,args->arg2,args->arg3);
   return(&rc);
}

int *rvssetaf_1(RVS2INT_args *args)
{
   static  int  rc;

   rc = vssetaf(args->arg1,args->arg2);
   return(&rc);
}

int *rvssetthin_1(RVS1INT_args *args)
{
   static  int  rc;

   rc = vssetthin(args->arg1);
   return(&rc);
}

int *rvsnameq_1(RVSSTRING_args *args)
{
   static  int  rc;

   rc = vsnameq(args->vs_string);
   return(&rc);
}

int *rvsglt_1(RVS1INT_args *args)
{
   static  int  rc;

   rc = vsglt(args->arg1);
   return(&rc);
}

int *rvsxyn_1(RVSXYN_args *args)
{
   static  int  rc;

   rc = vsxyn(args->name,args->x.x_val,args->y.y_val,args->n);
   return(&rc);
}

int *rvsxynt_1(RVSXYNT_args *args)
{
   static  int  rc;

   rc = vsxynt(args->name,args->time,args->x.x_val,args->y.y_val,args->n);
   return(&rc);
}

int *rvsxytagnt_1(RVSXYTAGNT_args *args)
{
   static  int  rc;

   rc = vsxytagnt(args->name,args->time,args->x.x_val,args->y.y_val,
                  args->tag.tag_val,args->n);
   return(&rc);
}

int *rvssxynt_1(RVSSXYNT_args *args)
{
   static  int  rc;

   rc = vssxynt(args->name,args->time,args->x.x_val,args->y.y_val,args->n);
   return(&rc);
}

int *rvsxn_1(RVSXN_args *args)
{
   static  int  rc;

   rc = vsxn(args->name,args->x.x_val,args->n);
   return(&rc);
}

int *rvsxnt_1(RVSXNT_args *args)
{
   static  int  rc;

   rc = vsxnt(args->name,args->time,args->x.x_val,args->n);
   return(&rc);
}

int *rvsmxynt_1(RVSMXYNT_args *args)
{
   static  int  rc;

   rc = vsmxynt(args->name,args->start.start_val,args->time.time_val,args->nvec,
                args->x.x_val,args->y.y_val);
   return(&rc);
}

int *rvsmxy1_1(RVSMXY1_args *args)
{
   static  int  rc;

   rc = vsmxy1(args->wt,args->start.start_val,args->time.time_val,args->nvec,
               args->x.x_val,args->y.y_val);
   return(&rc);
}

int *rvsgnxyni_1(RVSGNXYNI_args *args)
{
   static   int   rc;

   rc = vsgnxyni(args->name,args->index);
   return(&rc);
   
}

int *rvsnsct_1(RVSNSCT_args *args)
{
   static   int   rc;

   rc = vsnsct(args->name,args->sc,args->time);
   return(&rc);
   
}

rget_xyvec0_res *rvsgxyni_1(RVSGXYNI_args *args)
{
   static    rget_xyvec0_res     res;

   int       n,                  rc;

   xdr_free(xdr_rget_xyvec0_res, &res);

   n = vsgnxyni(args->name,args->index);
   if( n > 0 ) {
#define   P res.rget_xyvec0_res_u.the_rxyvec0
      P.x.x_val = (double *) malloc(n * sizeof(double));
      P.x.x_len = n;
      P.y.y_val = (double *) malloc(n * sizeof(double));
      P.y.y_len = n;
      rc = vsgxyni(args->name,args->index,P.x.x_val,P.y.y_val,&P.n,&P.time);
   } else {
      P.x.x_val = NULL;
      P.x.x_len = 0;
      P.y.y_val = NULL;
      P.y.y_len = 0;
      P.n = 0;
   }
#undef    P
   res.errno = 0;
   return(&res);
}

int *rvsgetparametern_1(RVSGETPARAMETERN_args *args)
{
   static   int   rc;

   rc = vsgetparametern(args->name,args->which_parameter);
   return(&rc);

}

int *rvsgetlxyn_1(RVSGETLXYN_args *args)
{
   static   int   rc;

   rc = vsgetlxyn(args->name);
   return(&rc);

}

rget_xyalldata0_res *rvsgxynall_1(RVSGXYNALL_args *args) {
   static    rget_xyalldata0_res res;

   int       nEvents,            nIndex;

   xdr_free(xdr_rget_xyalldata0_res, &res);

   nEvents = vsgetparametern(args->name,VS_GET_NEVENTS);
   nIndex = vsgetparametern(args->name,VS_GET_LXY);
   if( nEvents > 0 ) {
#define   P res.rget_xyalldata0_res_u.the_rxyalldata0
      P.name = strdup(args->name);
      P.nIndex  = nIndex;
      P.nEvents = nEvents;
      P.N.N_len = nIndex;
      P.N.N_val = (int *) malloc(nIndex * sizeof(int));
      P.Time.Time_len = nIndex;
      P.Time.Time_val = (double *) malloc(nIndex * sizeof(double));
      P.x.x_len = nEvents;
      P.x.x_val = (double *) malloc(nEvents * sizeof(double));
      P.y.y_len = nEvents;
      P.y.y_val = (double *) malloc(nEvents * sizeof(double));
      vsgxynall(args->name,P.x.x_val,P.y.y_val,P.N.N_val,P.Time.Time_val);
   } else {
      P.name = strdup(args->name);
      P.nIndex  = nIndex;
      P.nEvents = nEvents;
      P.N.N_len = 0;
      P.N.N_val = NULL;
      P.Time.Time_len = 0;
      P.Time.Time_val = NULL;
      P.x.x_len = 0;
      P.x.x_val = NULL;
      P.y.y_len = 0;
      P.y.y_val = NULL;
   }
#undef    P
   res.errno = 0;
   return(&res);
}
