CCCC  Originally from string.f
c
c     Returns string representation of pid number.
c
      character*6 function spid()

         implicit   none

         integer    getpid

         write(spid,100) getpid()
100      format(i6)

         return

      end

