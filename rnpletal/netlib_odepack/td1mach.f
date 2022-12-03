c=======================================================================
c     Test program for d1mach which returns various values related
c     to the floating point implemenation.
c=======================================================================
      program     td1mach

      implicit    none

      real*8      d1mach

		integer     i 

      write(*,*) 'd1mach(1) = b**(emin-1), '//
     &           'the smallest positive magnitude = ', d1mach(1)
      write(*,*) 'd1mach( 2) = b**emax*(1 - b**(-t)), '//
     &           'the largest magnitude = ', d1mach(2)
      write(*,*) 'd1mach( 3) = b**(-t), '//
     &           'the smallest relative spacing = ', d1mach(3)
      write(*,*) 'd1mach( 4) = b**(1-t), '//
     &           'the largest relative spacing = ', d1mach(4)
      write(*,*) 'd1mach( 5) = log10(b) = ', d1mach(5)

      stop
   
      end
