#ifndef RNPL_AUX_TYPES_DEF
#define RNPL_AUX_TYPES_DEF

#include <v.h>
/* Robert needs to #ifndef some of his header routines ... */
/* #include <bbhutil.h> */

typedef struct     GFT_type {
   STRING           gfunc_name;
   int              ntlevs;
   DVEC             time_vec;
   double           time;
   int              rank;
   IVEC             shape;
   STRINGVEC        coord_names;
   DMAT             coordinate_vals;
   DVEC             data;
}                  GFT_type,   *PGFT_type;

#endif
