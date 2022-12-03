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

enum {
   COMM_PING_SUCCEEDED = 0,
   COMM_PING_FAILED = 1,
   COMM_HOSTNAME_FAILED = -1,
   COMM_SOCKET_CREATE_FAILED = -2,
   COMM_CONNECT_FAILED = -3,
   COMM_INVALID_MODE = -10
};

extern   int                   DEFAULT_XVSPORT;

extern   int                   ser0_start_xvs(int port);
extern   int                   ser0_serve_block_xvs(int ss,PFI_II_L service);
extern   int                   ser0_stop_xvs(int ss);
extern   int                   ser0_comm_xvs(char *sername,int port,
                                         Comm_mode mode);
extern   int                   ser0_ping_xvs(char *sername,int port);
extern   int                   ser0_connect_xvs(char *sername,int port);

extern   void                  dump_hostent_xvs(char *s, struct hostent *hp);

extern   void                  l_herror_xvs(char *s); 

#endif
