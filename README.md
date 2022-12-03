# rnpl-LRversion
Upgraded versions of RNPL to compile on LR's Mac

Note of changes made

Hans Bantilan (HB) has modified sdfdump.c to enable dumping of rank 2 .sdf files into ascii. I, Lorenzo Rossi (LR) modified it further to enable dumping of rank 3 .sdf files into ascii.


LR

I, Lorenzo Rossi, have edited the following files in RNPL changing #include <malloc.h> into #include <malloc/malloc.h> to eliminate an error during the compilation on Mac Os High Sierra.
The modified lines have a comment to explain what I did also inside the files themselves.

rnpl/examples/get_param/t_get_param_v.c: #include <malloc.h>
rnpl/examples/get_param/t_get_param.c: #include <malloc.h>
rvs/src/Archive/rvs_xdr.c: #include <malloc.h>
rvs/src/Archive/rvs_svc.c: #include <malloc.h>
rvs/src/Archive/rvs_clnt.c: #include <malloc.h>

I have also added the following lines in certain files in order to, again, avoid errors from modern compilers.

rvs/src/rvs.x: %#include <string.h>
rvs/src/v1.c: #include <stdlib.h>
rvs/src/vn.c: #include <stdlib.h>
rnpl/src/librnpl.c: #include "svs.h"
rnpl/src/sdf.c: #include <ctype.h>