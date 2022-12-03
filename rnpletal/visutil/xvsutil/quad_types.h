#ifndef QUAD_TYPESDEF
#define QUAD_TYPESDEF

typedef      struct        DPAIR {
   double                     x;
   double                     y;
}                          DPAIR,    *PDPAIR;

typedef      struct        DQUAD {
   double                     x1;
   double                     x2;
   double                     y1;
   double                     y2;
}                          DQUAD,    *PDQUAD;


typedef      struct        IPAIR {
   int                        x;
   int                        y;
}                          IPAIR,    *PIPAIR;

typedef      struct        IQUAD {
   int                        x1;
   int                        x2;
   int                        y1;
   int                        y2;
}                          IQUAD,    *PIQUAD;

typedef      struct        igeom {
   int                        x;
   int                        y;
   int                        w;
   int                        h;
}                          igeom,    *pigeom;


#endif 
