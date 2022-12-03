#include "cliser.h"
#include "stream.h"

extern    int ser0_service0(int ss,int cs);

/* Server main program ... */

int main(int argc,char **argv) {
   int  ss;
   if( (ss = ser0_start(5002)) > 0 ) {
		fprintf(stderr,"%s: Starting service\n",argv[0]);
      ser0_serve_block(ss,ser0_service0);
   }
   exit(0);
}

/* Server side handler ... */

int ser0_service0(int ss,int cs) {
   FILE                *stream;
   XDR                  xdrs;
   CLISER_STREAM        the = {2.0,3.0,4.0};

   if( !(stream = fdopen(cs,"r")) ) {
      fprintf(stderr,"ser0_serve_block: fdopen(%d) failed\n",cs);
      close(cs); 
      return 1;
   }
   xdrstdio_create(&xdrs,stream,XDR_DECODE);

/* If no data on stream, clean-up and return ... */

   if( !xdr_CLISER_STREAM(&xdrs,&the) )  goto Cleanup;

   if( the.a < 0 ) {
      fprintf(stderr,"ser0_serve_block: Shutting down server\n");
      ser0_stop(ss);
      return 0;
   }

   printf("a: %g  b: %g  c: %g\n",the.a,the.b,the.c);

Cleanup:
   xdr_destroy(&xdrs);
   fclose(stream);
   close(cs);
   return 1;
}
