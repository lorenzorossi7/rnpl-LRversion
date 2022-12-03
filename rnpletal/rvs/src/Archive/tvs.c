/* Tests vsxynt() ... */

#include      <stdio.h>
#include      <string.h>
#include      "rvs_cli.h"

int    N  = 10000;
int    Nt = 10;

main(int argc,char **argv) {
   double   *x,     *y;
	int       i,      it;

	if( !(x = (double *) malloc(N * sizeof(double))) ||
	    !(y = (double *) malloc(N * sizeof(double))) ) {
		fprintf(stderr,"tvs:  Couldn't allocate %d pairs\n",N);
	}
	for( i = 0; i < N; i++ ) {
		x[i] = i; y[i] = x[i] * x[i];
	}
	for( it = 0; it < Nt; it++ ) {
		vsxynt("tvs",(double) it,x,y,N);
	}
	exit(0);
}
