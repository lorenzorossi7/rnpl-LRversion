#ifndef FILE_C_DEF
#define FILE_C_DEF

#define LBUFLEN   1024

int     file_exists(char *fname);
int     nlines_in_file(char *fname);
int     file_nlines(char *fname);
char   *file_lino(char *fname,int lino);
char   *s_extract_tag(char *tag_w_exta,char *extb);
double  get_double_from_file(char *filename);
int     get_int_from_file(char *filename);
char   *fname_expand(char *fname_in);
int     is_dir(char *fname);

#endif
