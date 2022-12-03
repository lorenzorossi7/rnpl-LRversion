#ifndef V_TYPESDEF
#define V_TYPESDEF

typedef      void            *VEC;

typedef      double         *DVEC;
typedef      double        **DMAT;

typedef      float          *SVEC;
typedef      float         **SMAT;

typedef      int            *IVEC;
typedef      int           **IMAT;

typedef      long           *LVEC;
typedef      int           **LMAT;

typedef      unsigned int  *UIVEC;
typedef      unsigned int **UIMAT;

typedef      char         *STRING;
typedef      char     **STRINGVEC;
typedef      char          *CHVEC;
typedef      char         **CHMAT;

typedef      struct        IVECN {
   IVEC      v;
   int       n;
}                          IVECN;

typedef      struct        SVECN {
   SVEC      v;
   int       n;
}                          SVECN;

typedef      struct        DVECN {
   DVEC      v;
   int       n;
}                          DVECN;

typedef      struct        DI {
   double    x;
   int       ix,  spacer;
}                          DI, *DIVEC;

typedef      struct        D2 {
   double    x1;
   double    x2;
}                          D2, *PD2;

typedef      struct        SI {
   float     x;
   int       ix,  spacer;
}                          SI, *SIVEC;

typedef      struct        S2 {
   float     x1;
   float     x2;
}                          S2, *PS2;

typedef      struct        PDVEC {
   DVEC      v1;
   DVEC      v2;
}                          PDVEC,  *PPDVEC;

typedef      struct        PDVECN {
   DVEC      v1;
   DVEC      v2;
   int       n;
}                          PDVECN, *PPDVECN;

typedef      struct        PSVEC {
   SVEC      v1;
   SVEC      v2;
}                          PSVEC,  *PPSVEC;

#define      r_DVEC        register DVEC
#define      r_IVEC        register IVEC
#define      r_SVEC        register SVEC
#define      r_DMAT        register DMAT

#define      r_int         register int
#define      r_float       register float
#define      r_double      register double

typedef      double       *(* DVEC_FCN)();
typedef      float        *(* SVEC_FCN)();

typedef      void          (* PFV_GENERIC)();
typedef      int           (* PFI_GENERIC)();
typedef      float         (* PFF_GENERIC)();
typedef      double        (* PFD_GENERIC)();

typedef      void          (* PFV)(void);
typedef      int           (* PFI)(int);
typedef      float         (* PFF)(float);
typedef      double        (* PFD)(double);
typedef      int           (* COMPAR)(const void *,const void *);

typedef      void          (* PFV_I)       (int);
typedef      void          (* PFV_II)      (int,int);
typedef      int           (* PFI_II)      (int,int);
typedef      int           (* PFI_D)       (double);
typedef      int           (* PFI_F)       (float);
typedef      int           (* PFI_FFF)     (float,float,float);
typedef      void          (* PFV_ID)      (int,double);
typedef      void          (* PFV_IDD)     (int,double,double);
typedef      STRING        (* PFS_SS)      (STRING,STRING);

#endif 
