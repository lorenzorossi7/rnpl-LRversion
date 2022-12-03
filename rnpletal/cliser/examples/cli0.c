#include "cliser.h"
#include "stream.h"

/* Client main program ... */

int main(int argc,char **argv) {
   FILE           *stream;
   XDR             xdrs;
   CLISER_STREAM   the;


   int             cs;

   if( argc < 4 ) goto Usage;
   if( (sscanf(argv[1],"%lf",&the.a) != 1) ||
       (sscanf(argv[2],"%lf",&the.b) != 1) ||
       (sscanf(argv[3],"%lf",&the.c) != 1) ) goto Usage;

   if( !getenv("SER0HOST") ) {
      fprintf(stderr,"%s: Environment variable SER0HOST not set\n",argv[0]);
      exit(-1);
   }
   
   if( (cs = ser0_connect(getenv("SER0HOST"),5002)) < 0 ) {
      fprintf(stderr,"%s: Connect to '%s' failed\n",argv[0],getenv("SER0HOST"));
      exit(-1);
   }

   if( !(stream = fdopen(cs,"w")) ) {
      fprintf(stderr,"cli0: fdopen(%d) failed\n",cs);
      exit(-1);
   }
   xdrstdio_create(&xdrs,stream,XDR_ENCODE);
   if( !xdr_CLISER_STREAM(&xdrs,&the) ) {
      fprintf(stderr,"cli0: xdr_CLISER_STREAM(&xdrs,&the) failed\n");
      exit(-1);
   }
   xdr_destroy(&xdrs);
   fclose(stream); 
   close(cs);
   
   exit(0);

Usage:
   fprintf(stderr,"usage: %s <a> <b> <c>\n",argv[0]);
   exit(1);
}
