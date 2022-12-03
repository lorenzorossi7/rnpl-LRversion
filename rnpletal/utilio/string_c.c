/* MWC's string manipulation routines ... */

#include <stdio.h>
#include <string.h>
#include "string_c.h"

/* Returns pointer to first non blank character in string ... */

char *sskiplb(char *string) {
   while( string && string[0] == ' ' ) ++string;
   return string;
}

/* Lowercases string ... */

char *str2lc(char *str_in) {
	char   *str_out = (char *) NULL;
	int     pos;
	if( str_in ) {
		str_out = strdup(str_in);
		for( pos = 0; pos < strlen(str_out); pos++ ) {
			if( 'A' <= str_out[pos] && str_out[pos] <= 'Z' ) {
				str_out[pos] += 'a' - 'A';
			}
		}
	}
	return(str_out);
}

/* Converts input string to f77 identifier if possible ... */

char  *str2f77id(char *str_in) {
	char    first_set[] = 
             "_abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUWXYZ";
	char    nth_set[] = 
             "_abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUWXYZ0123456789";
	char   *str_out = (char *) NULL;
	int     pos, out_pos, len_str_out;

	if( str_in ) {
		for( pos = 0; pos < strlen(str_in); pos++ ) {
			if( pos == 0 ) {
				if( strchr(first_set,str_in[0]) ) {
					len_str_out = 1;
				} else {
					goto Exit;
				}
			} else {
				if( strchr(nth_set,str_in[pos]) ) ++len_str_out;
			}
		}
	}
	if( len_str_out ) {
		str_out = (char *) malloc((len_str_out + 1) * sizeof(char));
		str_out[len_str_out] = '\0';
		for( pos = 0, out_pos = 0; pos < strlen(str_in); pos++ ) {
         if( pos == 0 || strchr(nth_set,str_in[pos]) ) {
				str_out[out_pos] = str_in[pos];
				out_pos++;
			}
      }
	}
Exit:
	return( str2lc(str_out) );
}

/* Strips special (non alpha-numeric-underscore anU) characters and return 
   new string ... */

char *str2anu(char *str_in) {
	char     valid[] = 
				"_abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUWXYZ0123456789";
	char    *str_out = (char *) NULL;
	int      pos, out_pos, len_str_out = 0;

	if( str_in && strlen(str_in) ) {
		for( pos = 0; pos < strlen(str_in); pos++ ) {
			if( strchr(valid,str_in[pos]) ) ++len_str_out;
		}
		if( len_str_out ) {
			str_out = (char *) malloc((len_str_out + 1) * sizeof(char));
			str_out[len_str_out] = '\0';
			for( pos = 0, out_pos = 0; pos < strlen(str_in); pos++ ) {
				if( strchr(valid,str_in[pos]) ) {
					str_out[out_pos] = str_in[pos];
					out_pos++;
				}
			}
		}
	} 
	return( str_out );
}
