c     The following 3 lines must be included in any routines that are
c     to use the signal handling facility.
c
c     Use -lutilio in link statement to use these routines.
c
      integer       sig_interrupt
      common      / com_interrupt /
     *              sig_interrupt

c     All of the 'external' statements below are is optional ...
      external      set_interrupt

      logical       query_interrupt
      external      query_interrupt

      external      reset_interrupt
      external      unset_interrupt

      integer  i,  j,  sum

c     Initialize the interrupt handler ...
      call set_interrupt
      
      j = 0
      i = 0
      write(0,*) 'Type Ctrl-C to send interrupt, Ctrl-Z to suspend'
      do while ( .true. ) 
c        Test to see whether interrupt has been generated ...
         if( query_interrupt() ) then
            write(*,*) 'Interrupt at step ', i, j
c           Reset the interrupt handler ...
            call reset_interrupt
         end if
         j = j + 1
         if( j .eq. 1 000 000 000 ) then
           i = i + 1
           r = 0
         end if
      end do

      stop

      end
