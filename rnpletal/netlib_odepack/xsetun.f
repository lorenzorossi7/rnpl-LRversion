c     MWC changed "lun .gt. 0" to "lun .ge. 0"

      subroutine xsetun (lun)
c
c this routine resets the logical unit number for messages.
c
      integer lun, mesflg, lunit
      common /eh0001/ mesflg, lunit
c
      if (lun .ge. 0) lunit = lun
      return
c----------------------- end of subroutine xsetun ----------------------
      end
