#define GA(n,v,s) if( argc > n ) if( sscanf(argv[n],s,&v) != 1 ) goto Usage
#define GAD(n,v,s) if( argc > n )  if( strcmp(argv[n],".") && sscanf(argv[n],s,&v) != 1 ) goto Usage
 
#define ARG_INC   --argc, ++argv
#define USE_STDIN argc == 1 && strlen(argv[1]) == 1 && argv[1][0] == '-'
