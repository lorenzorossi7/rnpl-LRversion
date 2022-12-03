#ifndef    IV_DEF
#define    IV_DEF

#include    "v_types.h"

extern      LVEC        make_LVEC(int n);
extern      void        free_LVEC(LVEC v);

extern      LVEC        Lvcopy(LVEC v1,int n);
extern      LVEC        LVcopy(LVEC v1,LVEC v2,int n);

extern      void        lvdump(LVEC v,int n,char *s);

extern      long        lvsum(LVEC v,int n);
extern      long        lvprod(LVEC v,int n);

#endif
