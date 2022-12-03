#ifndef CVTESTSDF_DEF
#define CVTESTSDF_DEF

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <bbhutil.h>
#include <v.h>

#define ON     1
#define OFF    0
#define BUFLEN 1024

#include "rnpl_aux_types.h"

int     extract_GFT(char *sdfname,int step,GFT_type *D);
char   *sdf_canon(char *fname);
int     check_GFT_cvt3(GFT_type *D);

#endif
