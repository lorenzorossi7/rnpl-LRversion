#ifndef VS_SERVER_NAMES_DEF
#define VS_SERVER_NAMES_DEF

/* Possible machines visualizer could be running on. */

char *server_names[] = {
      "einstein.ph.utexas.edu",
      "infeld.ph.utexas.edu",
      "hoffmann.ph.utexas.edu",
      "128.83.131.87"
};
/*
char *server_names[] = {
      "128.83.131.6",
      "128.83.131.169"
};
*/
#define N_SERVERS sizeof(server_names) / sizeof(char *)

#endif
