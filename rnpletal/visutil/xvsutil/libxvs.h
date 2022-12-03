#ifndef LIBXVS_DEF
#define LIBXVS_DEF

#include "sdf_types.h"
#include "sdf_util.h"

#define   OFF       0
#define   ON        1
#define   BUFLEN    1024

#define   UNDEF      -1

enum {
   XVS_OPEN,
   XVS_CLOSE
};

extern    char *Xvshost;
extern    int   Xvsrc;
extern    int   l_XVSPORT;
extern    int   l_DEFAULT_XVSPORT;
extern    int   Ltrace_l;

extern    char  BL[];

extern    int xvs_recv_buf(FILE *stream, char *buf);
extern    int xvs_send_buf(FILE *stream, char *buf);
extern    int xvs_recv_status(FILE *stream, char *buf);
extern    int xvs_send_status(FILE *stream, int status, char *buf);
extern    int xvs_control_send_message(FILE *stream,char *msg,char *failmsg,char *P);
extern    FILE *xvs_stream(int op);
extern    FILE *xvs_io(int op);

extern    int l_int_env(char *env, int defval);

#include "libxvs_f.h"

#endif
