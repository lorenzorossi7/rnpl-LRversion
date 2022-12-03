#include <stdio.h>
#include <signal.h>
#include "f77sig_c.h"

int ltrace = 1;


#ifdef CRAY
#include <fortran.h>

void SET_INTERRUPT(void) {
   COM_INTERRUPT.sig_interrupt = 0;
   signal(SIGINT,sig_onintr);
}

void UNSET_INTERRUPT(void) {
   COM_INTERRUPT.sig_interrupt = 0;
   signal(SIGINT,SIG_DFL);
}

void sig_onintr(int foo) {
   COM_INTERRUPT.sig_interrupt = 1;
   signal(SIGINT,sig_onintr);
}

#else 
#ifdef __
void set_interrupt__(void) {
	if( ltrace ) {
		fprintf(stderr,"set_interrupt__: In routine\n");
	}
   com_interrupt__.sig_interrupt = 0;
   signal(SIGINT,sig_onintr);
}

void unset_interrupt__(void) {
   com_interrupt__.sig_interrupt = 0;
   signal(SIGINT,SIG_DFL);
}

void sig_onintr(int foo) {
	if( ltrace ) {
		fprintf(stderr,"sig_onintr: In routine\n");
	}
   com_interrupt__.sig_interrupt = 1;
   signal(SIGINT,sig_onintr);
}
#else
void set_interrupt_(void) {
   com_interrupt_.sig_interrupt = 0;
   signal(SIGINT,sig_onintr);
}

void unset_interrupt_(void) {
   com_interrupt_.sig_interrupt = 0;
   signal(SIGINT,SIG_DFL);
}

void sig_onintr(int foo) {
   com_interrupt_.sig_interrupt = 1;
   signal(SIGINT,sig_onintr);
}
#endif
#endif
