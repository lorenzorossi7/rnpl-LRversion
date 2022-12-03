#ifndef _VDEF 
#define _VDEF

#define  MAKEVEC(TYPE,N) (TYPE *) malloc(N * sizeof(TYPE))
#define  FREEVEC(TYPE,X) if( X ) free(X); X = (TYPE *) NULL

#include "sv.h"
#include "dv.h"
#include "iv.h"
#include "lv.h"

#include "imatlib.h"
#include "dmatlib.h"

#include "d3lib.h"

#endif
