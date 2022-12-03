#ifndef DEF_SDF_TYPES
#define DEF_SDF_TYPES

/* SDF data set type */

typedef struct SDFDS {
   double   time;
   int      version;
   int      rank;
   int      dsize;
   int      csize;
   char    *pname;
   char    *cnames;
   char    *tag;
   int     *shape;
   double  *bbox;
   double  *coords;
   double  *data;
}              SDFDS,  *PSDFDS;

#endif
