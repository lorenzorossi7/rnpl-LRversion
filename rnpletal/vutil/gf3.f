c-----------------------------------------------------------------------
c
c     D O U B L E     P R E C I S I O N     3 -- A R R A Y
c
c                        O P E R A T I O N S
c
c
c                      ( G F 3     S T Y L E )
c
c-----------------------------------------------------------------------
c     Author:       Matthew W. Choptuik (matt@infeld.ph.utexas.edu)
c     Institution:  The University of Texas at Austin
c     Date:         1994
c----------------------------------------------------------------------
c     Dec 27, 1998: Deprecating this source file for the time being.
c     Non-standard f77 features used.
c----------------------------------------------------------------------

c----------------------------------------------------------------------
c
c     Versions of d3lib routines ...
c  
c----------------------------------------------------------------------
c----------------------------------------------------------------------
c
c     GF3 minimum ...
c  
c----------------------------------------------------------------------

      double precision function gf3_min(shape,gf3)
         implicit      none

         integer       shape(3)
         real*8        gf3(shape(1),shape(2),shape(3))

         integer       i,            j,            k

         gf3_min = gf3(1,1,1)
         do k = 1 , shape(3)
            do j = 1 , shape(2)
               do i = 1 , shape(1)
                  gf3_min = min(gf3_min,gf3(i,j,k))
               end do
            end do
         end do

         return
      end

c----------------------------------------------------------------------
c
c     GF3 maximum ...
c  
c----------------------------------------------------------------------

      double precision function gf3_max(shape,gf3)
         implicit      none

         integer       shape(3)
         real*8        gf3(shape(1),shape(2),shape(3))

         integer       i,            j,            k

         gf3_max = gf3(1,1,1)
         do k = 1 , shape(3)
            do j = 1 , shape(2)
               do i = 1 , shape(1)
                  gf3_max = max(gf3_max,gf3(i,j,k))
               end do
            end do
         end do

         return
      end
c----------------------------------------------------------------------
c
c     gf3 := gf3 + c sqrt(((x,y,z) - orig)^2)^pow 
c
c----------------------------------------------------------------------

      subroutine gf3_add_multipole(shape,gf3,x,y,z,orig,c,pow)
         
         implicit      none

         integer       shape(3)
         real*8        gf3(shape(1),shape(2),shape(3))
         real*8        x(shape(1)),  y(shape(2)),  z(shape(3))
         real*8        orig(3),      c,            pow

         real*8        r
         integer       i,            j,            k

         do k = 1 , shape(3)
            do j = 1 , shape(2)
               do i = 1 , shape(1)
                  r = sqrt((x(i) - orig(1)) ** 2 +
     &                     (y(j) - orig(2)) ** 2 +
     &                     (z(k) - orig(3)) ** 2)
                  if( r .ne. 0.0d0 ) then
                     gf3(i,j,k) = gf3(i,j,k) + c * r ** pow
                  end if 
               end do
            end do
         end do

         return

      end

c----------------------------------------------------------------------
c
c     log10( gf3 - subval )
c  
c----------------------------------------------------------------------

      subroutine gf3log10(shape,gf3in,gf3out,subval)
         implicit      none

         integer       shape(3)
         real*8        gf3in (shape(1),shape(2),shape(3)),
     &                 gf3out(shape(1),shape(2),shape(3))
         real*8        subval

         integer       i,            j,            k

         do k = 1 , shape(3)
            do j = 1 , shape(2)
               do i = 1 , shape(1)
                  gf3out(i,j,k) = log10(gf3in(i,j,k) - subval)
               end do
            end do
         end do

         return
      end
