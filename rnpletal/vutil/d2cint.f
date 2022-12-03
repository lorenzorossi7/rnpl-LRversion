c-----------------------------------------------------------------------
c
c     Dimension checker ...
c
c-----------------------------------------------------------------------

      logical function c2dim(n)

         implicit      none

         integer       n

         c2dim =  mod(n,2) .eq. 1  .and.  n .ge. 5

         return

      end

c-----------------------------------------------------------------------
c
c     2:1 cubic interpolation ...
c
c-----------------------------------------------------------------------

      subroutine d2cint(uc,uf,d1c,d2c)

         implicit       none

         logical        c2dim

         integer        d1c,     d2c
         real*8         uc(d1c,d2c),
     *                  uf(2*d1c-1,2*d2c-1)

         integer        i1,      i2
     *                  d1f,     d2f

         real*8         lc(4),      cc(4),      rc(4)
         data
     *     lc / 0.31250e0,  0.93750e0, -0.31250e0,  0.06250e0 /,
     *     cc /-0.06250e0,  0.56250e0,  0.56250e0, -0.06250e0 /,
     *     rc / 0.06250e0, -0.31250e0,  0.93750e0,  0.31250e0 /

         if( c2dim(d1c) .and. c2dim(d2c) ) then
            d1f = 2 * d1c - 1
            d2f = 2 * d2c - 1
            do 10 i2 = 1  , d2c
               call dv2i4(uc(1,i2),uf(1,2*i2-1),d1c)
 10         continue
            call dvsma4(uf(1,1),uf(1,3),
     *                  uf(1,5),uf(1,7),
     *                  uf(1,2),lc,d1f)
            do 30 i2 = 4 , d2f - 3 , 2
                call dvsma4(uf(1,i2-3),uf(1,i2-1),
     *                      uf(1,i2+1),uf(1,i2+3),
     *                      uf(1,i2),cc,d1f)
 30         continue
            call dvsma4(uf(1,d2f-6),uf(1,d2f-4),
     *                  uf(1,d2f-2),uf(1,d2f),
     *                  uf(1,d2f-1),rc,d1f)
            do 50 i2 = 1 , d2f
               call dvsma4(uf(1,i2,1),uf(1,i2,3),
     *                     uf(1,i2,5),uf(1,i2,7),
     *                     uf(1,i2,2),lc,d1f)
 50         continue
            
         else
            write(*,*) '>>> d2cint: Invalid dimension(s): ',
     *                 d1c, d2c
         end if

         return

      end

