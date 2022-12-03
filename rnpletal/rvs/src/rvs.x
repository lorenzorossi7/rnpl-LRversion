/* Implementation of remote vs ... */
%#include <stdlib.h>
%#include <string.h> //added by LR - new compilers seem to require this

struct RVS1INT_args {
   int        arg1;
};

struct RVS2INT_args {
   int        arg1;
   int        arg2;
};

struct RVS3INT_args {
   int        arg1;
   int        arg2;
   int        arg3;
};

struct RVSMXY1_args {
   int        wt;
   int        start<>;
   double     time<>;
   int        nvec;
   double     x<>;
   double     y<>;
};

struct RVSMXYNT_args {
   string     name<>;
   int        start<>;
   double     time<>;
   int        nvec;
   double     x<>;
   double     y<>;
};

struct RVSSTRING_args {
   string     vs_string<>;
};
 
struct RVSXN_args {
   string     name<>;
   double     x<>;
   int        n;
};

struct RVSXNT_args {
   string     name<>;
   double     time;
   double     x<>;
   int        n;
};

struct RVSXY1_args {
   int        wt;
   double     time;
   double     x<>;
   double     y<>;
   int        n;
};

struct RVSXY2_args {
   int        wt;
   double     time;
   int        tag;
   double     x<>;
   double     y<>;
   int        n;
};

struct RVSXYNT_args {
   string     name<>;
   double     time;
   double     x<>;
   double     y<>;
   int        n;
};

struct RVSXYTAGNT_args{
   string     name<>;
   double     time;
   double     x<>;
   double     y<>;
	int        tag<>;
   int        n;
};

struct RVSSXYNT_args {
   string     name<>;
   float      time;
   float      x<>;
   float      y<>;
   int        n;
};

struct RVSXYN_args {
   string     name<>;
   double     x<>;
   double     y<>;
   int        n;
};

struct RVSGNXYNI_args {
	string     name<>;
	int        index;
};

struct RVSGXYNI_args {
	string     name<>;
	int        index;
};

struct RVSNSCT_args {
	string     name<>;
	double     sc;
	double     time;
};

struct rxyvec0 {
	int        index;
	double     time;
	int        tag;
	int        n;
	double     x<>;
	double     y<>;
};

union rget_xyvec0_res switch(int errno) {
case 0:
	rxyvec0 the_rxyvec0;
default:
	void;
};

struct RVSGETPARAMETERN_args {
	string      name<>;
	int         which_parameter;
};

struct RVSGETLXYN_args {
	string      name<>;
};

struct RVSGXYNALL_args {
	string      name<>;
};

struct rxyalldata0 {
	string    name<>;
	int       nIndex;
	int       nEvents;
	int       N<>;
	double    Time<>;
	double    x<>;
	double    y<>;
};

union rget_xyalldata0_res switch(int errno) {
case 0:
	rxyalldata0 the_rxyalldata0;
default:
	void;
};

program RVS {
   version RVSVERS {
      int RVSCONNECT(void)                   =   1;
      int RVSOPEN(RVSSTRING_args)            =   2;
      int RVSXY1(RVSXY1_args)                =   3;
      int RVSXY2(RVSXY2_args)                =   4;
      int RVSCLOSE(RVS1INT_args)             =   5;
      int RVSHANG(void)                      =   6;
      int RVSSTATUS(RVS1INT_args)            =   7;
      int RVSGETVSPID(void)                  =   8;
      int RVSRESET(void)                     =   9;
      int RVSSETMAXLXY(RVS2INT_args)         =  10;
      int RVSGETPARAMETER(RVS2INT_args)      =  11;
      int RVSSETPARAMETER(RVS3INT_args)      =  12;
      int RVSSETAF(RVS2INT_args)             =  13;
      int RVSSETTHIN(RVS1INT_args)           =  14;
      int RVSNAMEQ(RVSSTRING_args)           =  15;
      int RVSGLT(RVS1INT_args)               =  16;
      int RVSXYN(RVSXYN_args)                =  17;
      int RVSXYNT(RVSXYNT_args)              =  18;
      int RVSXN(RVSXN_args)                  =  19;
      int RVSXNT(RVSXNT_args)                =  20;
      int RVSSXYNT(RVSSXYNT_args)            =  21;
      int RVSMXYNT(RVSMXYNT_args)            =  22;
      int RVSMXY1(RVSMXY1_args)              =  23;
      int RVSGNXYNI(RVSGNXYNI_args)          =  24;
      int RVSNSCT(RVSNSCT_args)              =  25;
      rget_xyvec0_res RVSGXYNI(RVSGXYNI_args) 
                                             =  26;
      int RVSGETPARAMETERN(RVSGETPARAMETERN_args) 
                                             =  27;
      int RVSGETLXYN(RVSGETLXYN_args)        =  28;
      rget_xyalldata0_res RVSGXYNALL(RVSGETLXYN_args)
                                             =  29;
	   int RVSXYTAGNT(RVSXYTAGNT_args)        =  30;
      int RVSKILLALL(void)                   =  31;
   } = 1;
} = 100;
