#ifndef F77SIG_CDEF
#define F77SIG_CDEF

#ifdef CRAY
struct  {
   int      sig_interrupt;
}              COM_INTERRUPT;

extern void  SET_INTERRUPT(void);
extern void  UNSET_INTERRUPT(void);
extern void  sig_onintr(int foo);
#else
#ifdef __
struct  COM_INTERRUPT {
   int      sig_interrupt;
}              com_interrupt__;

extern void  set_interrupt__(void);
extern void  unset_interrupt__(void);
extern void  sig_onintr(int foo);
#else
struct  COM_INTERRUPT {
   int      sig_interrupt;
}              com_interrupt_;

extern void  set_interrupt_(void);
extern void  unset_interrupt_(void);
extern void  sig_onintr(int foo);
#endif
#endif

#endif
