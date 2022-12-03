#include <math.h>
#include <stdlib.h>
#include "utilmath_c.h"
#include "utilmathc.h"

double drand48_(void) {
   return drand48();
}

int countnans_(double *v,int *pn) {
   return countnans(v,*pn);
}

void replacenans_(double *v,int *pn,double *pvalue) {
   replacenans(v,*pn,*pvalue);
}
