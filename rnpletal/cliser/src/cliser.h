#ifndef CLISER_DEF
#define CLISER_DEF

#include <stdlib.h>
#include <sys/types.h>
#include <errno.h>
#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>

#define                        ON          1
#define                        OFF         0
#define                        BUFLEN      1024

typedef  int                   (* PFI_II_L) (int,int);

typedef enum {
  COMM_CONNECT,
  COMM_PING
} Comm_mode;


extern   int                   ser0_start(int port);
extern   int                   ser0_serve_block(int ss,PFI_II_L service);
extern   int                   ser0_stop(int ss);
extern   int                   ser0_comm(char *sername,int port,
                                         Comm_mode mode);
extern   int                   ser0_ping(char *sername,int port);
extern   int                   ser0_connect(char *sername,int port);

extern   void                  dump_hostent(char *s, struct hostent *hp);

extern   void                  l_herror(char *s);

#endif
