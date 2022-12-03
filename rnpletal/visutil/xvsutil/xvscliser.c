#include "xvscliser.h"
/* #include "xvs.h" */

#define  LTRACE 0

int DEFAULT_XVSPORT=5001;

/************************************************************************/
/*                                                                      */
/*  Generic socket based client-server utilities:                       */
/*                                                                      */
/*     ser0_start_xvs(int port)                                          */
/*        Binds a socket to specified port on invoking host and returns */
/*        socket for subsequent accepts() etc.  Returns -1 in service   */
/*        can not be started.                                           */
/*     ser0_serve_block_xvs(int ss,(int *) service(int,int))             */
/*        Waits for connections (blocks) on previously opened socket    */
/*        and invokes service after connection has been established.    */
/*        Loops until service() returns 0.                              */
/*     ser0_stop_xvs(int ss)                                             */
/*        Shuts down socket                                             */
/*     ser0_ping_xvs(char *sername,int port)                             */
/*        Returns > 0 if service running on specified port on sername,  */
/*        0 otherwise.                                                  */
/*     ser0_connect_xvs(char *sername,int port)                          */
/*        Connects to specified port on host sername and returns socket */
/*        Returns (-1,-2,-3) for (Unknown host, socket creation error,  */
/*        connect error) respectively.                                  */
/*                                                                      */
/************************************************************************/

/************************************************************************/
/*                                                                      */
/* Starts server and returns socket for subsequent use in accept() ...  */
/* Environment variable HOSTNAME overrides value returned by            */
/* gethostname() (work-around for host machine operating via PPP)       */
/*                                                                      */
/************************************************************************/

int ser0_start_xvs(int port) {
   struct sockaddr_in  ser;
   char                R[] = "ser0_start_xvs";
   int                 lenser = sizeof(ser);
   struct hostent     *hp = NULL;
   char                hostname[BUFLEN];
   int                 max_l_ss_queue = 2048;

   int     ltrace = LTRACE;
   int     ss,     cs,   l_true = 1,  sockopt,  optlen,  optval;

   if( getenv("HOSTNAME") ) {
      strcpy(hostname,getenv("HOSTNAME"));
   } else {
      if( gethostname(hostname,BUFLEN) < 0 ) {
         l_herror_xvs("ser0_start: gethostname() returned error code="); return -1;
      }
   }
   if( ltrace ) fprintf(stderr, "%s: hostname=%s\n",R, hostname);

   if( (cs = ser0_ping_xvs(hostname,port)) > 0 ) {
      fprintf(stderr,"ser0_start_xvs: Server already running on %s, port %d.\n",
         hostname, port);
      close(cs);
      return -1;
   }

   if( ltrace ) {
      fprintf(stderr,"ser0_start_xvs: Attempting to serve on '%s'.\n",
              hostname);
   }

   if( (hp = gethostbyname(hostname)) == NULL ) {
      l_herror_xvs("ser0_start: gethostname() returned error code="); return -1; 
   }

   if( (ss = socket(AF_INET, SOCK_STREAM, 0)) < 0 ) {
      perror("ser0_start_xvs"); return -1;
   }

/* Allow server to re-use local address ... */

   setsockopt(ss,SOL_SOCKET,SO_REUSEADDR,&l_true,sizeof(l_true));
   if( ltrace ) {
#ifdef __BITS_SOCKET_H
      getsockopt(ss,SOL_SOCKET,SO_REUSEADDR,&optval,(socklen_t *) &optlen);
#elif defined DARWIN
      getsockopt(ss,SOL_SOCKET,SO_REUSEADDR,&optval,(socklen_t *) &optlen);
#else
      getsockopt(ss,SOL_SOCKET,SO_REUSEADDR,&optval,              &optlen);
#endif
      fprintf(stderr,"ser0_start_xvs: SO_REUSEADDR(%d)=%d(%d)\n",ss,optval,optlen);
   }

   bzero((char *) &ser, sizeof(ser));
   ser.sin_family = AF_INET;
   bcopy(hp->h_addr,(char *) &ser.sin_addr,hp->h_length);
   ser.sin_port = htons(port);

   if( bind(ss, (struct sockaddr *) &ser, sizeof(ser)) ) {
      perror("bind"); return -1;
   }

   if( listen(ss, max_l_ss_queue) ) {
      perror("listen"); return -1;
   } else {
      if( ltrace ) {
         fprintf(stderr,"ser0_start_xvs: Listening for connections on %d.\n",ss);
      }
   }

   return ss;
}

/************************************************************************/

int ser0_serve_block_xvs(int ss,PFI_II_L service) {
   int                  ltrace = LTRACE;
   struct sockaddr_in   cli;
   int                  lencli = sizeof(struct sockaddr_in);
   int                  cs;

   while( 1 ) {
      if( ltrace ) {
         fprintf(stderr,"ser0_serve_block_xvs: Waiting for a connection on %d.\n",
                 ss);
      }
#ifdef __BITS_SOCKET_H
      cs = accept(ss, (struct sockaddr *) &cli, (socklen_t *) &lencli);
#elif defined DARWIN
      cs = accept(ss, (struct sockaddr *) &cli, (socklen_t *) &lencli);
#else 
      cs = accept(ss, (struct sockaddr *) &cli,               &lencli);
#endif
      if( ltrace ) {
         fprintf(stderr,"ser0_serve_block_xvs: Connection on %d.\n",cs);
      }
      if( !(*service)(ss,cs) ) return 1;
   }
}

/************************************************************************/

int ser0_stop_xvs(int ss) {

   close(ss);

   return 1;
}

/************************************************************************/

int ser0_comm_xvs(char *sername,int port,Comm_mode mode) {
   static struct sockaddr_in  ser;
   int lenser = sizeof(ser);
   static struct hostent     *hp = NULL;

   int      ltrace = LTRACE;
   int      cs,   l_true = 1,  optlen,  optval,  rc;

   if( hp == NULL ) {
      if( ltrace ) {
         fprintf(stderr,
            "ser0_comm_xvs: Initialization for communication with '%s'.\n",
             sername);
      }
      if( (hp = gethostbyname(sername)) == NULL ) {
         return COMM_HOSTNAME_FAILED;
      }

      bzero((char *) &ser, sizeof(ser));
      ser.sin_family = AF_INET;
      bcopy(hp->h_addr_list[0],(char *) &ser.sin_addr,hp->h_length);    
      ser.sin_port = htons(port);
   }
   if( ltrace ) {
      dump_hostent_xvs("ser0_comm_xvs",hp);
      fprintf(stderr,"port: %d  htons(port): %d\n",port,htons(port));
   }

   if( (cs = socket(AF_INET, SOCK_STREAM, 0)) < 0 ) {
      return COMM_SOCKET_CREATE_FAILED;
   }

   if( setsockopt(cs,SOL_SOCKET,SO_REUSEADDR,&l_true,sizeof(l_true)) ) {
      fprintf(stderr,"ser0_comm_xvs: setsockopt failed\n");
   }
   if( ltrace ) {
#ifdef __BITS_SOCKET_H
      getsockopt(cs,SOL_SOCKET,SO_REUSEADDR,&optval,(socklen_t *) &optlen);
#elif defined DARWIN
      getsockopt(cs,SOL_SOCKET,SO_REUSEADDR,&optval,(socklen_t *) &optlen);
#else
      getsockopt(cs,SOL_SOCKET,SO_REUSEADDR,&optval,              &optlen);
#endif
      fprintf(stderr,"ser0_comm_xvs: SO_REUSEADDR(%d)=%d(%d)\n",cs,optval,optlen);
   }

   rc = connect(cs, (struct sockaddr *) &ser, sizeof(ser));

   switch( mode ) {

   case COMM_CONNECT:
      if( rc ) {
         if( ltrace ) {
            fprintf(stderr,"ser0_comm_xvs: connect(%d,...) returns %d.",
                    cs,rc);
            fprintf(stderr,"   errno: %d\n",errno);
         }
         close(cs);
         return COMM_CONNECT_FAILED;
      } else {
         return cs;
      }
   case COMM_PING:
      if( rc ) {
         if( ltrace ) {
            fprintf(stderr,"ser0_comm_xvs: connect(%d,...) returns %d.",
                    cs,rc);
            fprintf(stderr,"   errno: %d\n",errno);
         }
         return COMM_PING_SUCCEEDED;
      } else {
         close(cs);
         return COMM_PING_FAILED;
      }
   default:
      fprintf(stderr,"ser0_comm_xvs: Mode %d not implemented.\n",mode);
      return COMM_INVALID_MODE;
   }
}

int ser0_ping_xvs(char *sername,int port) {
   return ser0_comm_xvs(sername,port,COMM_PING);
}

int ser0_connect_xvs(char *sername,int port) {
   return ser0_comm_xvs(sername,port,COMM_CONNECT);
}

/************************************************************************/

void dump_hostent_xvs(char *s, struct hostent *hp) {
   int    i = 0;
   fprintf(stderr,"%s: dump_hostent_xvs\n",s);
   fprintf(stderr,"   h_name: %s\n",hp->h_name);
   while( hp->h_aliases[i] ) {
      fprintf(stderr,"  h_aliases[%d]: %s\n",i,hp->h_aliases[i]);
      i++;
   }
   fprintf(stderr,"   h_name: %s\n",hp->h_name);
   fprintf(stderr,"   h_addrtype: %d\n",hp->h_addrtype);
   fprintf(stderr,"   h_length: %d\n",hp->h_addrtype);
   fprintf(stderr,"   h_addr: %p\n",hp->h_addr);
   i = 0;
   while( hp->h_addr_list[i] ) {
      fprintf(stderr,"   h_addr_list[%d]: %p\n",i,hp->h_addr_list[i]);
      i++;
   }
}

/*  herror() is not portable, but h_errno is ... */
void l_herror_xvs(char *s) {
   switch( h_errno ) {
   case HOST_NOT_FOUND:
      fprintf(stderr,"%s%d -- Host not found.\n",s,h_errno);
      break;
   case NO_ADDRESS:
      fprintf(stderr,"%s%d -- No address.\n",s,h_errno);
      break;
   case NO_RECOVERY:
      fprintf(stderr,"%s%d -- No recovery.\n",s,h_errno);
      break;
   case TRY_AGAIN:
      fprintf(stderr,"%s%d -- Try again.\n",s,h_errno);
      break;
   default:
      break;
   }
}
