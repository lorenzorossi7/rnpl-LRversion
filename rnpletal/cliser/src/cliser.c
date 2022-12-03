#include "cliser.h"

#define  LTRACE 0

/************************************************************************/
/*                                                                      */
/*  Generic socket based client-server utilities:                       */
/*                                                                      */
/*     ser0_start(int port)                                             */
/*        Binds a socket to specified port on invoking host and returns */
/*        socket for subsequent accepts() etc.  Returns -1 in service   */
/*        can not be started.                                           */
/*     ser0_serve_block(int ss,(int *) service(int,int))                */
/*        Waits for connections (blocks) on previously opened socket    */
/*        and invokes service after connection has been established.    */
/*        Loops until service() returns 0.                              */
/*     ser0_stop(int ss)                                                */
/*        Shuts down socket                                             */
/*     ser0_ping(char *sername,int port)                                */
/*        Returns > 0 if service running on specified port on sername,  */
/*        0 otherwise.                                                  */
/*     ser0_connect(char *sername,int port)                             */
/*        Connects to specified port on host sername and returns socket */
/*        Returns (-1,-2,-3) for (Unknown host, socket creation error,  */
/*        connect error) respectively.                                  */
/*                                                                      */
/************************************************************************/

/* For checking for connections using select ... */

/* fd_set                  Ser0_fd_set;          */
/* struct timeval          Timeout;              */


/************************************************************************/
/*                                                                      */
/* Starts server and returns socket for subsequent use in accept() ...  */
/* Environment variable HOSTNAME overrides value returned by            */
/* gethostname() (work-around for host machine operating via PPP)       */
/*                                                                      */
/************************************************************************/

int ser0_start(int port) {
   struct sockaddr_in  ser;
   int                 lenser = sizeof(ser);
   struct hostent     *hp = NULL;
   char                hostname[BUFLEN];
   int                 max_l_ss_queue = 5;

   int     ltrace = LTRACE;
   int     ss,     cs,   true = 1,  sockopt,  optlen;

   if( getenv("HOSTNAME") ) {
      strcpy(hostname,getenv("HOSTNAME"));
   } else {
      if( gethostname(hostname,BUFLEN) < 0 ) {
         l_herror("ser0_start: gethostname() returned error code="); return -1;
      }
   }

   if( (cs = ser0_ping(hostname,port)) > 0 ) {
      fprintf(stderr,"ser0_start: Server already running\n");
      close(cs);
      return -1;
   }

   if( ltrace ) {
      fprintf(stderr,"ser0_start: Attempting to serve on '%s'\n",
              hostname);
   }

   if( (hp = gethostbyname(hostname)) == NULL ) {
         l_herror("ser0_start: gethostname() returned error code="); return -1;
   }

   if( (ss = socket(AF_INET, SOCK_STREAM, 0)) < 0 ) {
      perror("ser0_start"); return -1;
   }

/* Allow server to re-use local address ... */

   setsockopt(ss,SOL_SOCKET,SO_REUSEADDR,&true,sizeof(true));

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
         fprintf(stderr,"ser0_start: Listening for connections on %d\n",ss);
      }
   }

   return ss;
}

/************************************************************************/

int ser0_serve_block(int ss,PFI_II_L service) {
   int                  ltrace = LTRACE;
   struct sockaddr_in   cli;
   int                  lencli = sizeof(struct sockaddr_in);
   int                  cs;

   while( 1 ) {
      if( ltrace ) {
         fprintf(stderr,"ser0_serve_block: Waiting for a connection on %d\n",
                 ss);
      }
      cs = accept(ss, (struct sockaddr *) &cli, &lencli);
      if( ltrace ) {
         fprintf(stderr,"ser0_serve_block: Connection on %d \n",cs);
      }
      if( !(*service)(ss,cs) ) return 1;
   }
}

/************************************************************************/

int ser0_stop(int ss) {
   int     ltrace = LTRACE;

   if( shutdown(ss,2) ) {
      perror("ser0_stop"); return -1;
   }
   close(ss);

   return 1;
}

/************************************************************************/

int ser0_comm(char *sername,int port,Comm_mode mode) {
   static struct sockaddr_in  ser;
   static int                 lenser = sizeof(ser);
   static struct hostent     *hp = NULL;

   int      ltrace = LTRACE;
   int      cs,     rc;

   if( hp == NULL ) {
      if( ltrace ) {
         fprintf(stderr,
            "ser0_comm: Initialization for communication with '%s'\n",
             sername);
      }
      if( (hp = gethostbyname(sername)) == NULL ) {
         return -1;
      }

      bzero((char *) &ser, sizeof(ser));
      ser.sin_family = AF_INET;
/*    bcopy(hp->h_addr,(char *) &ser.sin_addr,hp->h_length);  */
      bcopy(hp->h_addr_list[0],(char *) &ser.sin_addr,hp->h_length);    
      if( ltrace ) {
         dump_hostent("ser0_comm",hp);
         fprintf(stderr,"port: %d  htons(port): %d\n",port,htons(port));
      }
      ser.sin_port = htons(port);
   }

   if( (cs = socket(AF_INET, SOCK_STREAM, 0)) < 0 ) {
      return -2;
   }

   rc = connect(cs, (struct sockaddr *) &ser, sizeof(ser));
   switch( mode ) {

   case COMM_CONNECT:
      if( rc ) {
         perror("ser0_comm");
         if( ltrace ) {
            fprintf(stderr,"ser0_comm: connect(%d,...) returns %d",
                    cs,rc);
            fprintf(stderr,"   errno: %d\n",errno);
         }
         close(cs);
         return -3;
      } else {
         return cs;
      }
   case COMM_PING:
      if( rc ) {
         if( ltrace ) {
            fprintf(stderr,"ser0_comm: connect(%d,...) returns %d",
                    cs,rc);
            fprintf(stderr,"   errno: %d\n",errno);
         }
         return 0;
      } else {
         close(cs);
         return 1;
      }
   default:
      fprintf(stderr,"ser0_comm: Mode %d not implemented\n",mode);
      return -10;
   }
}

int ser0_ping(char *sername,int port) {
   return ser0_comm(sername,port,COMM_PING);
}

int ser0_connect(char *sername,int port) {
   return ser0_comm(sername,port,COMM_CONNECT);
}

/************************************************************************/

void dump_hostent(char *s, struct hostent *hp) {
   int    i = 0;
   fprintf(stderr,"%s: dump_hostent\n",s);
   fprintf(stderr,"   h_name: %s\n",hp->h_name);
   while( hp->h_aliases[i] ) {
      fprintf(stderr,"  h_aliases[%d]: %s\n",i,hp->h_aliases[i]);
      i++;
   }
   fprintf(stderr,"   h_name: %s\n",hp->h_name);
   fprintf(stderr,"   h_addrtype: %d\n",hp->h_addrtype);
   fprintf(stderr,"   h_length: %d\n",hp->h_addrtype);
   fprintf(stderr,"   h_addr: %s\n",hp->h_addr);
   i = 0;
   while( hp->h_addr_list[i] ) {
      fprintf(stderr,"   h_addr_list[%d]: %s\n",i,hp->h_addr_list[i]);
      i++;
   }
}

/*  herror() is not portable, but h_errno is ... */
void l_herror(char *s) {
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
