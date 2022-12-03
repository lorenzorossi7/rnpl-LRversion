/* Remote versions of vs routines ... */

#include "rvs_cli_includes.h"

/* Server name database ... */

#include "vs_server_names.h"

char *  Server = (char *) NULL;

#define STD_DECL      CLIENT *cl; int *rc;

#define CHECK1        if( !assign_Server() ) return -1; cl = clnt_create(Server,RVS,RVSVERS,"tcp"); if( cl == NULL ) { clnt_pcreateerror(Server); return -1; }

#define CHECK2        if(args) free(args); if( rc == NULL ) { clnt_perror(cl,Server); clnt_destroy(cl); return -1; } else { clnt_destroy(cl); return *rc; }

#define ARGS_ALLOC(TYPE) args = (TYPE *) malloc(sizeof(TYPE))

int assign_Server() {
   CLIENT *cl;
   int      ltrace = OFF;
   if( !Server ) {
      if( (Server = getenv("VSHOST")) )  {
         if( ltrace ) {
            fprintf(stderr,"assign_Server: VSHOST variable set.\n");
            fprintf(stderr,"assign_Server: Attempting to connect to %s.\n",
                    Server);
         }
         cl = clnt_create(Server,RVS,RVSVERS,"tcp");
         if( cl ) {
            clnt_destroy(cl);
            goto Break;
         } else {
            fprintf(stderr,"assign_Server: Could not communicate with %s.\n",
                    Server);
            fprintf(stderr,
               "assign_Server: Ensure that server is running on %s and/or\n",
                    Server);
            fprintf(stderr,"assign_Server: check/reset value of environment ");
            fprintf(stderr,"variable VSHOST.\n");
            return 0;
         }
      } else {
			fprintf(stderr,"assign_Server: environment variable VSHOST not set\n");
         return 0;
      }
Break:
      if( ltrace ) {
         fprintf(stderr,"assign_Server: Will communicate with vs on <%s>.\n",
                 Server);
      }
   }
   return 1;
}

/* vsconnect does *not* leave (remote) server state as it found it ... */

int vsconnect(void)
{
   STD_DECL

   void          *args = (void *) NULL;

   CHECK1
   rc = rvsconnect_1(args,cl);
   CHECK2
}

int vsopen(const char *name)
{
   int             ltrace = OFF;

   STD_DECL

   RVSSTRING_args *args;

   if( ltrace ) printf("vsopen: (rvs_cli.c) name: <%s>\n",name);

   CHECK1
   ARGS_ALLOC(RVSSTRING_args);
   args->vs_string = (char *) name;
   rc = rvsopen_1(args,cl);
   CHECK2
}

int vsxy1(int wt, double time, DVEC x, DVEC y, int n)
{
   STD_DECL

   RVSXY1_args    *args;

   CHECK1
   ARGS_ALLOC(RVSXY1_args);
   args->wt = wt;     args->time = time;
   args->x.x_val = x; args->x.x_len = n;
   args->y.y_val = y; args->y.y_len = n;
   args->n = n;
   rc = rvsxy1_1(args,cl);
   CHECK2
}

int vsxy2(int wt, double time, int tag, DVEC x, DVEC y, int n)
{
   STD_DECL

   RVSXY2_args    *args;

   CHECK1
   ARGS_ALLOC(RVSXY2_args);
   args->wt = wt;     args->time = time;
   args->tag = tag;
   args->x.x_val = x; args->x.x_len = n;
   args->y.y_val = y; args->y.y_len = n;
   args->n = n;
   rc = rvsxy2_1(args,cl);
   CHECK2
}

int vsclose(int wt)
{
   STD_DECL

   RVS1INT_args   *args;

   CHECK1
   ARGS_ALLOC(RVS1INT_args);
   args->arg1 = wt;
   rc = rvsclose_1(args,cl);
   CHECK2
}

int vshang(void)
{
   STD_DECL

   void          *args = (void *) NULL;

   CHECK1
   rc = rvshang_1(args,cl);
   CHECK2
}

int vsstatus(int wt)
{
   STD_DECL

   RVS1INT_args   *args;

   CHECK1
   ARGS_ALLOC(RVS1INT_args);
   args->arg1 = wt;
   rc = rvsstatus_1(args,cl);
   CHECK2
}

int vsgetvspid(void)
{
   STD_DECL

   void          *args = (void *) NULL;

   CHECK1
   rc = rvsgetvspid_1(args,cl);
   CHECK2
}

int vsreset(void)
{
   STD_DECL

   void          *args = (void *) NULL;

   CHECK1
   rc = rvsreset_1(args,cl);
   CHECK2
}

int vskillall(void)
{
   STD_DECL

   void          *args = (void *) NULL;

   CHECK1
   rc = rvskillall_1(args,cl);
   CHECK2
}

int vssetmaxlxy(int wt, int lxy)
{
   STD_DECL

   RVS2INT_args   *args;

   CHECK1
   ARGS_ALLOC(RVS2INT_args);
   args->arg1 = wt;
   args->arg2 = lxy;
   rc = rvssetmaxlxy_1(args,cl);
   CHECK2
}

int vsgetparameter(int wt, int code)
{
   STD_DECL

   RVS2INT_args   *args;

   CHECK1
   ARGS_ALLOC(RVS2INT_args);
   args->arg1 = wt;
   args->arg2 = code;
   rc = rvsgetparameter_1(args,cl);
   CHECK2
}

int vssetparameter(int wt, int code, int value)
{
   STD_DECL

   RVS3INT_args   *args;

   CHECK1
   ARGS_ALLOC(RVS3INT_args);
   args->arg1 = wt;
   args->arg2 = code;
   args->arg3 = value;
   rc = rvssetparameter_1(args,cl);
   CHECK2
}

int vssetaf(int wt, int af)
{
   STD_DECL

   RVS2INT_args   *args;

   CHECK1
   ARGS_ALLOC(RVS2INT_args);
   args->arg1 = wt;
   args->arg2 = af;
   rc = rvssetaf_1(args,cl);
   CHECK2
}

int vssetthin(int wt)
{
   STD_DECL

   RVS1INT_args   *args;

   CHECK1
   ARGS_ALLOC(RVS1INT_args);
   args->arg1 = wt;
   rc = rvssetthin_1(args,cl);
   CHECK2
}

int vsnameq(const char *name)
{
   STD_DECL

   RVSSTRING_args *args;

   CHECK1
   ARGS_ALLOC(RVSSTRING_args);
   args->vs_string = (char *) name;
   rc = rvsnameq_1(args,cl);
   CHECK2
}

int vsglt(int wt)
{
   STD_DECL

   RVS1INT_args   *args;

   CHECK1
   ARGS_ALLOC(RVS1INT_args);
   args->arg1 = wt;
   rc = rvsglt_1(args,cl);
   CHECK2
}

int vsxyn(const char *name, DVEC x, DVEC y, int n)
{
   STD_DECL

   RVSXYN_args    *args;

   CHECK1
   ARGS_ALLOC(RVSXYN_args);
   args->name = (char *) name;
   args->x.x_val = x;  args->x.x_len = n;
   args->y.y_val = y;  args->y.y_len = n;
   args->n = n;
   rc = rvsxyn_1(args,cl);
   CHECK2
}

int vsxynt(const char *name, double time, DVEC x, DVEC y, int n)
{
   STD_DECL 

   RVSXYNT_args   *args;

   CHECK1
   ARGS_ALLOC(RVSXYNT_args); 
   args->name = (char *) name; args->time = time;
   args->x.x_val = x; args->x.x_len = n;
   args->y.y_val = y; args->y.y_len = n;
   args->n = n;
   rc = rvsxynt_1(args,cl);
   CHECK2
}

int vsxytagnt(const char *name, double time, DVEC x, DVEC y, IVEC tag, int n)
{
   STD_DECL 

   RVSXYTAGNT_args   *args;

   CHECK1
   ARGS_ALLOC(RVSXYTAGNT_args); 
   args->name = (char *) name;       args->time = time;
   args->x.x_val = x;       args->x.x_len = n;
   args->y.y_val = y;       args->y.y_len = n;
   args->tag.tag_val = tag; args->tag.tag_len = n;
   args->n = n;
   rc = rvsxytagnt_1(args,cl);
   CHECK2
}

int vssxynt(const char *name, float time, SVEC x, SVEC y, int n)
{
   STD_DECL 

   RVSSXYNT_args   *args;

   CHECK1
   ARGS_ALLOC(RVSSXYNT_args); 
   args->name = (char *) name; args->time = time;
   args->x.x_val = x; args->x.x_len = n;
   args->y.y_val = y; args->y.y_len = n;
   args->n = n;
   rc = rvssxynt_1(args,cl);
   CHECK2
}

int vsxn(const char *name, DVEC x, int n)
{
   STD_DECL

   RVSXN_args     *args;

   CHECK1
   ARGS_ALLOC(RVSXN_args);
   args->name = (char *) name;
   args->x.x_val = x; args->x.x_len = n;
   args->n = n;
   rc = rvsxn_1(args,cl);
   CHECK2
}

int vsxnt(const char *name, double time, DVEC x, int n)
{
   STD_DECL

   RVSXNT_args    *args;

   CHECK1
   ARGS_ALLOC(RVSXNT_args);
   args->name = (char *) name;
   args->time = time;
   args->x.x_val = x; args->x.x_len = n;
   args->n = n;
   rc = rvsxnt_1(args,cl);
   CHECK2
}

int vsmxynt(const char *name, IVEC start, DVEC time, int nvec, DVEC x, DVEC y)
{
   STD_DECL

   RVSMXYNT_args  *args;

   CHECK1
   ARGS_ALLOC(RVSMXYNT_args);
   args->name = (char *) name;
   args->start.start_val = start;  args->start.start_len = nvec + 1;
   args->time.time_val   = time;   args->time.time_len   = nvec;
   args->nvec = nvec;
   args->x.x_val = x;              args->x.x_len = start[nvec] - 1;
   args->y.y_val = y;              args->y.y_len = start[nvec] - 1;
   rc = rvsmxynt_1(args,cl);
   CHECK2
}

int vsmxy1(int wt, IVEC start, DVEC time, int nvec, DVEC x, DVEC y)
{
   STD_DECL

   RVSMXY1_args   *args;

   CHECK1
   ARGS_ALLOC(RVSMXY1_args);

   args->wt = wt; 
   args->start.start_val = start;  args->start.start_len = nvec + 1;
   args->time.time_val   = time;   args->time.time_len   = nvec;
   args->nvec = nvec;
   args->x.x_val = x;              args->x.x_len = start[nvec] - 1;
   args->y.y_val = y;              args->y.y_len = start[nvec] - 1;
   rc = rvsmxy1_1(args,cl);
   CHECK2
}

int vsgnxyni(const char *name,int index) 
{
   STD_DECL
   
   RVSGNXYNI_args    *args;

   CHECK1
   ARGS_ALLOC(RVSGNXYNI_args);

   args->name = (char *) name;
   args->index = index;
   rc = rvsgnxyni_1(args,cl);
   CHECK2
}

int vsnsct(const char *name,double sc,double time)
{
   STD_DECL
   
   RVSNSCT_args    *args;

   CHECK1
   ARGS_ALLOC(RVSNSCT_args);

   args->name = (char *) name;
   args->sc = sc;
   args->time = time;
   rc = rvsnsct_1(args,cl);
   CHECK2
}

int vsgxyni(const char *name, int index, DVEC x, DVEC y, Pint pn, Pdouble ptime)
{

   CLIENT           *cl; 
   rget_xyvec0_res  *res;

   RVSGXYNI_args    *args;

   CHECK1
   ARGS_ALLOC(RVSGXYNI_args);

   args->name = (char *) name;
   args->index = index;
   res = rvsgxyni_1(args,cl);

   free(args); 
   if( res == NULL ) { 
      clnt_perror(cl,Server); 
      clnt_destroy(cl); 
      return -1; 
   } else { 
#define   P res->rget_xyvec0_res_u.the_rxyvec0
      lDVcopy(P.x.x_val,x,P.n);
      lDVcopy(P.y.y_val,y,P.n);
      *pn = P.n;
      *ptime = P.time;
      clnt_destroy(cl); 
      return P.n; 
#undef    P
   }
}


int vsgxynall(const char *name,DVEC x,DVEC y,IVEC N,DVEC Time) {

   CLIENT                *cl;
   rget_xyalldata0_res   *res;
   RVSGXYNALL_args       *args;

   CHECK1 
   ARGS_ALLOC(RVSGXYNALL_args);

   args->name = (char *) name;
   res = rvsgxynall_1(args,cl);

   free(args);
      if( res == NULL ) {
      clnt_perror(cl,Server);
      clnt_destroy(cl);
      return -1;
   } else {
#define   P res->rget_xyalldata0_res_u.the_rxyalldata0
      clnt_destroy(cl);
      lIVcopy(P.N.N_val,N,P.nIndex);
      lDVcopy(P.Time.Time_val,Time,P.nIndex);
      lDVcopy(P.x.x_val,x,P.nEvents);
      lDVcopy(P.y.y_val,y,P.nEvents);
      return P.nIndex;
#undef    P
   }
}

/* Local version of DVcopy ... */

DVEC lDVcopy(DVEC v1,DVEC v2,int n) {
   int   i;

   if( (v1 != (DVEC) NULL) && (v2 != (DVEC) NULL) ) {
      for( i = 0; i < n; i++ ) {
         v2[i] = v1[i];
      }
   }
   return v2;
}

/* Local version of IVcopy ... */

IVEC lIVcopy(IVEC v1,IVEC v2,int n) {
   int   i;

   if( (v1 != (IVEC) NULL) && (v2 != (IVEC) NULL) ) {
      for( i = 0; i < n; i++ ) {
         v2[i] = v1[i];
      }
   }
   return v2;
}

int vsgetparametern(const char *name,int which_parameter)
{
   STD_DECL

   RVSGETPARAMETERN_args    *args;

   CHECK1
   ARGS_ALLOC(RVSGETPARAMETERN_args);

   args->name = (char *) name;
   args->which_parameter = which_parameter;
   rc = rvsgetparametern_1(args,cl);
   CHECK2
}

int vsgetlxyn(const char *name)
{
   STD_DECL

   RVSGETLXYN_args           *args;

   CHECK1
   ARGS_ALLOC(RVSGETLXYN_args);

   args->name = (char *) name;
   rc = rvsgetlxyn_1(args,cl);
   CHECK2
}
