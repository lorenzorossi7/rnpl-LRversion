#include <string.h>

void fort_call(char *s)
{
   int i,l,c;
   
   l=strlen(s);
   for(c=0,i=0;i<l;i++){
      s[i]=tolower(s[i]);
      if(s[i]=='_') c++;
   }
   s[l++]='_';
   if(c) s[l++]='_';
   s[l]='\0';
}
