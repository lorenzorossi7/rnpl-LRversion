#include <string.h>
#include <ctype.h> //LR- this is needed in modern compilers to recognise the function tolower

void fort_call(char *s)
{
   int l,i;
   
   l=strlen(s);
   for(i=0;i<l;i++)
      s[i]=tolower(s[i]);
   s[l]='_';
   s[l+1]='\0';
}

