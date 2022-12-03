#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h> 
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "file_c.h"

int file_exists(char *fname) {
   FILE    *fp;

   if( (fp = fopen(fname,"r")) != NULL ) {
      fclose(fp);
      return 1;
   } else {
      return 0;
   }
}

/*  Assumes lines are LBUFLEN characters long or less ... */

int nlines_in_file(char *fname) {
   FILE    *fp;
   char    buffer[LBUFLEN];
   int     nlines = 0;

   if( (fp = fopen(fname,"r")) != NULL ) {
      while( fgets(buffer,LBUFLEN,fp) != NULL ) nlines++;
      fclose(fp);
   }
   return nlines;
}

int file_nlines(char *fname) {
   FILE    *fp;
   char    buffer[LBUFLEN];
   int     nlines = 0;

   if( (fp = fopen(fname,"r")) != NULL ) {
      while( fgets(buffer,LBUFLEN,fp) != NULL ) nlines++;
      fclose(fp);
   }
   return nlines;
}

char *file_lino(char *fname,int lino) {
   FILE    *fp;
   char    buffer[LBUFLEN];
   int     nlines = 0;

   if( (fp = fopen(fname,"r")) != NULL ) {
      while( fgets(buffer,LBUFLEN,fp) != NULL ) { 
         if( ++nlines == lino ) {
            fclose(fp);
            buffer[strlen(buffer)-1] = '\0';
            return(strdup(buffer));
         }
      }
      fclose(fp);
   }
   return((char *) NULL);
}

char *s_extract_tag(char *tag_w_exta,char *extb) {

   int   lsa,     lsb;
   char *sreturn;

   if( !tag_w_exta || !extb ) goto No_match;

   lsa = strlen(tag_w_exta);
   lsb = strlen(extb);
   if( lsa > lsb  &&  lsb > 0 ) {
      if( strcmp(tag_w_exta+(lsa - lsb),extb) ) {
         goto No_match;
      } else {
         sreturn = malloc(lsa - lsb + 1);
         sreturn[lsa - lsb] = '\0';
         return(strncpy(sreturn,tag_w_exta,lsa - lsb));
      }
   } else {
No_match:
      return(tag_w_exta ? strdup(tag_w_exta) : tag_w_exta);
   }
}

double get_double_from_file(char *filename) {
   FILE      *fp;
   double     rc  =  0.0;

   if( (fp = fopen(filename,"r")) ) {
      fscanf(fp,"%lf",&rc);
      fclose(fp);
   }

   return(rc);
}

int get_int_from_file(char *filename) {
   FILE      *fp;
   int        rc  =  0;

   if( (fp = fopen(filename,"r")) ) {
      fscanf(fp,"%d",&rc);
      fclose(fp);
   }

   return(rc);
}

/* Performs naive file name expansion a la csh

   [1] ^~  -->  $HOME 

   Returns new (malloc()'ed) string  ... */

char *fname_expand(char *fname_in) {
	char  *fname_out = (char *) NULL;
	int    len_out;

	if( fname_in ) {
		if( fname_in[0] == '~' ) {
			len_out = (strlen(fname_in) - 1) + strlen(getenv("HOME"));
			fname_out = (char *) malloc(1 + len_out);
			sprintf(fname_out,"%s%s",getenv("HOME"),fname_in + 1);
		} else {
			fname_out = strdup(fname_in);
		}
	} 
	return fname_out;
}

/* Returns 1 if fname is a directory ... */

int is_dir(char *fname) {
	struct stat info;
	stat(fname,&info);
	return S_ISDIR(info.st_mode);
}
