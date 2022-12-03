      program        f_thvs

      implicit       none

      integer        vsrc,       vsxynt
      real*8         x(101),     y(101)
      integer        i,          n,         j

      n = 101
      do i = 1 , n
         x(i) = 0.1d0 * (i - 1)
      end do
      do j = 1 , n
         do i = 1 , n
            y(i) = cos(x(i)+0.1d0*(j - 1))
         end do
         vsrc = vsxynt('f_thvs',0.1d0*(j - 1),x,y,n)
      end do
      call gft_close_all()

      stop
      end
