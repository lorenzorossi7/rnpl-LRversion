/* #include "project.h" */

#include "libxvs.h"
#include "xvscliser.h"

int   Ltrace_l = OFF;

char *Xvshost = (char *) NULL;
int   Xvsrc;
int   l_XVSPORT;
int   l_DEFAULT_XVSPORT = 5001;

char  BL[BUFLEN];

int xvs_recv_buf(FILE *stream, char *buf) {
   static char R[] = "xvs_recv_buf";
   PSDFDS      mess = (PSDFDS) NULL;
   char       *rbuf;
   int         rc;
   int         ltrace = Ltrace_l;

   sprintf(buf,"");

   mess = new_SDFDS();
   rewind(stream);
   rc = read_SDFDS(stream,mess);
   sprintf(buf,"%s",mess->pname);
   if( ltrace ) {
      fprintf(stderr,"%s: Returning rc=%d buf='%s'\n",R,rc,buf);
   }
   if( mess ) free_SDFDS(mess);
   return rc;

}

int xvs_send_buf(FILE *stream, char *buf) {
   static char R[] = "xvs_send_buf";
   PSDFDS      mess = (PSDFDS) NULL;
   int         rc = 0;
   int         ltrace = Ltrace_l;

   mess = message_SDFDS(buf);
   rc = write_SDFDS(stream,mess);
   rewind(stream);
   if( ltrace ) fprintf(stderr,"%s: Sent message='%s'. Returning rc=%d\n",R,buf,rc); 
   if( mess ) free_SDFDS(mess);

   return rc;
}

int xvs_recv_status(FILE *stream, char *buf) {
   static char R[] = "xvs_recv_status";
   PSDFDS      mess = (PSDFDS) NULL;
   char       *rbuf;
   int         rc;
   int         ltrace = Ltrace_l;

   sprintf(buf,"");

   mess = new_SDFDS();
   rewind(stream);
   rc = read_SDFDS(stream,mess);
   if( ltrace ) {
      fprintf(stderr,"%s: read_SDFDS returns %d\n",R,rc);
      fprintf(stderr,"%s:   mess->pname=%s\n",R,mess->pname);
      sscanf(mess->pname,"%s",BL);
      fprintf(stderr,"%s:   BL=%s\n",R,BL);
   }

   if( !rc  ) return rc;

   if( sscanf(mess->pname,"%s",BL) == 1 ) {
      if(       !strcmp("1",BL) ) {
         rc = 1;
         if( strlen(mess->pname) > 2 ) {
            sprintf(buf,"%s",mess->pname+2);
         }
      } else if( !strcmp("0",BL) ) {
         rc = 0;
         if( strlen(mess->pname) > 2 ) {
            sprintf(buf,"%s",mess->pname+2);
         }
      } else {
         fprintf(stderr,"%s: Fatal: Could not parse '1' or '0' from message='%s'\n",
            R,mess->pname);
         exit(1);
      }
   } else {
      fprintf(stderr,"sscanf(%s,...) failed\n",mess->pname);
      rc = 0;
   }
   if( ltrace ) fprintf(stderr,"%s: Returning rc=%d buf='%s'\n",R,rc,buf);
   if( mess ) free_SDFDS(mess);

   return rc;
}

int xvs_send_status(FILE *stream, int status, char *buf) {
   static char R[] = "xvs_send_status";
   PSDFDS      mess = (PSDFDS) NULL;
   int         rc = 0;
   int         ltrace = Ltrace_l;

   if( ltrace ) {
      fprintf(stderr,"%s: status=%d buf='%s'\n",R,status,buf ? buf : "");
   }

   if( status !=0 && status != 1 ) return rc;
   if( buf ) {
      sprintf(BL,"%d %s",status,buf);
   } else {
      sprintf(BL,"%d",status);
   }
   mess = message_SDFDS(BL);
   rc = write_SDFDS(stream,mess);
   rewind(stream);
   if( ltrace ) fprintf(stderr,"%s: Sent message='%s'. Returning rc=%d\n",R,BL,rc); 
   if( mess ) free_SDFDS(mess);

   return rc;
}

int xvs_control_send_message(FILE *stream,char *msg,char *failmsg,char *P) {
   static char R[] = "xvs_control_send_message";
   PSDFDS      message = (PSDFDS) NULL;
   int         rc;
   int         ltrace = Ltrace_l;

   if( !stream ) return 0;
   if( ltrace ) fprintf(stderr,"%s: msg=%s\n",R,msg);
   message = message_SDFDS(msg);
   rc = write_SDFDS(stream,message);
   if( message ) free_SDFDS(message);
   if( !rc )  {
      fprintf(stderr,"%s: %s\n",P,failmsg);
   }

   return rc;
}

FILE *xvs_io(int op) {
   FILE *the = (FILE *) NULL;
   static int xvs_ss = UNDEF;
   static int ncall = 0;
   static FILE *stream = (FILE *) NULL;
   static int msg_xvshost = 1;
   char hostname[BUFLEN];
   int ltrace = Ltrace_l;

   ncall++;
   if( ltrace ) {
      fprintf(stderr,"xvs_io: ncall=%d\n",ncall);
   }
   switch( op ) {
   case XVS_OPEN:
      if( xvs_ss == UNDEF ) {
         if( (Xvshost = getenv("XVSHOST")) || (Xvshost = getenv("VSHOST")) ) {
            l_XVSPORT = l_int_env("XVSPORT",l_DEFAULT_XVSPORT); 
            if( (xvs_ss = ser0_connect_xvs(Xvshost,l_XVSPORT)) > 0 ) {
               the    = fdopen(xvs_ss,"r+");
               stream = the;
            } else {
               fprintf(stderr,"xvs_io: ser0_connect_xvs() returns %d\n",xvs_ss);
               xvs_ss = UNDEF;
            }
         } else { 
            if( msg_xvshost ) {
               fprintf(stderr,"xvs_io: XVSHOST environment variable is not set.\n");
               (void) gethostname(hostname,BUFLEN);
               fprintf(stderr,"setenv XVSHOST %s\n",hostname);
               fprintf(stderr,"export XVSHOST=%s\n",hostname);
            }
            msg_xvshost = 0;
         }
      } else {
         fprintf(stderr,"xvs_io: Attempting to open but xvs_ss already defined.\n");
      }
      break; 
   case XVS_CLOSE:
      if( xvs_ss != UNDEF ) {
         if( stream ) {
            fclose(stream);
            stream = (FILE *) NULL;
            if( ltrace ) fprintf(stderr,"xvs_io: Closed stream.\n");
         } else {
            fprintf(stderr,"xvs_io: Attempting to close but stream is NULL.\n");
         }
         shutdown(xvs_ss,2);
         close(xvs_ss);
         if( ltrace ) fprintf(stderr,"xvs_io: Closed xvs_ss.\n");
         xvs_ss = UNDEF;
      } else {
         fprintf(stderr,"xvs_io: Attempting to close but xvs_ss is undefined.\n");
      }
      break;
   default:
      fprintf(stderr,"xvs_io: Unexpected op=%d\n",op);
      break;
   }
   return the;
}

FILE *xvs_stream(int op) {
   static FILE *stream = (FILE *) NULL;
   static int xvs_ss = UNDEF;
   static int ncall = 0;
   static int msg_xvshost = 1;
   static int nerrmsg = 0;
   char hostname[BUFLEN];
   int ltrace = Ltrace_l;

   ncall++;
   if( ltrace ) {
      fprintf(stderr,"xvs_stream: ncall=%d\n",ncall);
   }
   switch( op ) {
   case XVS_OPEN:
      if( !stream && (xvs_ss == UNDEF) ) {
         if( ltrace ) fprintf(stderr,"xvs_stream: Opening stream\n");
         if( (Xvshost = getenv("XVSHOST")) || (Xvshost = getenv("VSHOST")) ) {
            l_XVSPORT = l_int_env("XVSPORT",l_DEFAULT_XVSPORT);
            if( (xvs_ss = ser0_connect_xvs(Xvshost,l_XVSPORT)) > 0 ) {
               stream = fdopen(xvs_ss,"r+");
               if( !stream ) {
                  fprintf(stderr,"xvs_stream: *** Error opening stream\n");
               }
            } else {
               if( !nerrmsg ) {
                  fprintf(stderr,"xvs_stream: Could not connect to server on %s, port %d.\n\n",
                        Xvshost, l_XVSPORT);
                  fprintf(stderr,"Ensure that 'xvs' is running on %s\n",
                        Xvshost);
                  fprintf(stderr,"and/or that the XVSPORT environment variable is set properly.\n\n");
                  nerrmsg = 1;
               }
               xvs_ss = UNDEF;
            }
         } else { 
            if( msg_xvshost ) {
               fprintf(stderr,"xvs_stream: XVSHOST environment variable is not set.\n");
               (void) gethostname(hostname,BUFLEN);
               fprintf(stderr,"setenv XVSHOST %s\n",hostname);
               fprintf(stderr,"export XVSHOST=%s\n",hostname);
            }
            msg_xvshost = 0;
         }
      }
      break; 
   case XVS_CLOSE:
      if( stream && (xvs_ss != UNDEF) ) {
         if( ltrace ) fprintf(stderr,"xvs_stream: Closing stream\n");
         if( fclose(stream) ) {
            fprintf(stderr,"xvs_stream: *** Error closing stream\n");
         }
         stream = (FILE *) NULL;
         if( ltrace ) fprintf(stderr,"xvs_stream: Closed stream.\n");
         shutdown(xvs_ss,2);
         close(xvs_ss);
         if( ltrace ) fprintf(stderr,"xvs_stream: Closed xvs_ss.\n");
         xvs_ss = UNDEF;
      } else {
         fprintf(stderr,"xvs_stream: Attempting to close but xvs_ss or stream is undefined.\n");
      }
      break;
   default:
      fprintf(stderr,"xvs_stream: Unexpected op=%d\n",op);
      break;
   }
   return stream;
}

FILE *xvs_open(void) {
   return xvs_io(XVS_OPEN);
}

void xvs_close(void) {
   (void) xvs_io(XVS_CLOSE);
}

int vsxynt(const char *name, double time, double *x, double *y, int n) {
   FILE *stream = (FILE *) NULL;
   SDFDS *the = (SDFDS *) NULL;
   char cnames[] = "x";
   int shape[1];
   double bbox[2];
   int rc = -1;
   int ltrace = Ltrace_l;

   if( n < 1 ) return rc;

   if( (stream = xvs_stream(XVS_OPEN)) ) {
      the = new_SDFDS();
      shape[0] = n;
      bbox[0] = x[0];
      bbox[1] = x[n-1];
      the->time = time;
      the->rank = 1;
      the->dsize = n;
      the->csize = n;
      the->pname = (char *) name;
      the->cnames = cnames;
      the->tag = 0;
      the->shape = shape; 
      the->bbox = bbox; 
      the->coords = x;
      the->data = y;
      if( write_SDFDS(stream,the) ) {
         rc = n;
      } 
      if( getenv("XVSI") ) {
         if( ltrace ) {
            fprintf(stderr,"vsxynt: XVSI set.  Closing stream\n");
         }
         stream = xvs_stream(XVS_CLOSE);
      }
   }
   return rc;
}

int xvs(const char *name, double time, double *x, double *y, int n) {
 return vsxynt(name, time, x, y, n);
}

int vsmxynt(const char *name, int *start, double *time, int n, double *x, double *y) {
   int  i, rc;
   FILE *stream = (FILE *) NULL;
   SDFDS *mess  = (SDFDS *) NULL; 
   if( (stream = xvs_stream(XVS_OPEN)) ) {  

		/*
      mess = message_SDFDS("set bulkinput");
      rc = write_SDFDS(stream,mess);
      if( mess ) free_SDFDS(mess);
		*/

      for( i = 0; i < n; i++ ) {
         rc = vsxynt(name,time[i],x+start[i]-1,y+start[i]-1,start[i+1]-start[i]);
      }

		/*
      mess = message_SDFDS("unset bulkinput");
      rc = write_SDFDS(stream,mess);
      if( mess ) free_SDFDS(mess);
		*/
   }
   return rc;
}

int l_int_env(char *env, int defval) {
   int  val;
   if( getenv(env) ) {
      if( !(sscanf(getenv(env),"%d",&val) == 1) ) val = defval;
   } else {
      val = defval;
   }
   return val;
}

