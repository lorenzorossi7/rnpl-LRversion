/* This version removes references to mygl/device.h etc. which inhibits 
	compilation of xv[1n]
*/
#ifndef _DEF_SYSINCLUDES
#define _DEF_SYSINCLUDES
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <limits.h>
#ifndef DARWIN
#include <values.h>
#endif
#include <libgen.h>
#ifdef SGI
#include <sys/bsd_types.h>
#endif
#include <sys/types.h>
#include <errno.h>
#include <assert.h>
#ifdef DARWIN
#include <termios.h>
#else
#include <termio.h>
#endif
#include <unistd.h>
#include <fcntl.h>
#include <string.h>
#include <setjmp.h>
#include <signal.h>
#include <time.h>
#include <sys/dir.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#ifndef NO_SGI_GL
#ifdef SGI
#include <device.h>
#include <gl.h>
#include <gl/sphere.h>
#include <fmclient.h>
#else
#endif
#endif

#ifdef DARWIN
#include <sys/ttycom.h>
#endif

#ifdef SGI
#include <sys/dirent.h>
#else
#include <dirent.h>
#endif

#ifndef MYCONSTANTS_DEF
#define MYCONSTANTS_DEF

#define                  ON          1
#define                  OFF         0
#define                  UNKNOWN     2
#define                  YES         1
#define                  NO          0
#ifndef                  TRUE
#define                  TRUE        1
#endif
#ifndef                  FALSE
#define                  FALSE       0
#endif
#define                  BUFLEN      1024

#endif

#ifndef MPI
#define MPI
#endif

#ifndef ME
#define ME
#endif

#endif
