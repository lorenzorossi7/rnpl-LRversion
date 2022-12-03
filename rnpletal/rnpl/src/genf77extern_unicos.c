#include <string.h>

void fort_call(char *s)
{
   int l,i;
   
   l=strlen(s);
   for(i=0;i<l;i++)
      s[i]=toupper(s[i]);
}

