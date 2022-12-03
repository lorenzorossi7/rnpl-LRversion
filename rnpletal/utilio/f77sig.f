      subroutine reset_interrupt()
         implicit none
         include 'f77sig.com'
         sig_interrupt = 0
         return
      end 

      logical function query_interrupt()
         implicit none
         include 'f77sig.com'
         query_interrupt = sig_interrupt .ne. 0
         return
      end 
