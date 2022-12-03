c-----------------------------------------------------------------------
c
c     Writes 3d uniform mesh function to file which can be read by 
c     explorer module ReadXYZ3DData.
c
c     This version has d1, d2, d3 limits hard-wired [-1 ... +1]
c
c-----------------------------------------------------------------------

      subroutine d3put_XYZ3DData0(fname,a,d1,d2,d3)

         implicit      none

         integer       getu

         character*(*) fname
         integer       d1,     d2,     d3
         real*8        a(d1,d2,d3)

         integer       u,      i1,     i2,     i3
         real*8        x,      y,      z

         logical       ltrace
         parameter   ( ltrace = .true. )

         if( d1 .lt. 1  .or.  d2 .lt. 1  .or.  d3 .lt. 1 ) then
            write(0,*) 'd3put_XYZ3DData0: Invalid dimension.'
            return
         end if
      
         u = getu()
         open(u,file=fname,form='formatted',status='unknown',err=900)

         write(u,*) d1
         write(u,*) d2
         write(u,*) d3
         do i1 = 1 , d1
            if( d1 .eq. 1 ) then
               x = -1.0d0
            else 
               x = -1.0d0 + 2.0d0 * (i1 - 1) / (d1 - 1)
            end if
            do i2 = 1 , d2
               if( d2 .eq. 1 ) then
                  y = -1.0d0
               else 
                  y = -1.0d0 + 2.0d0 * (i2 - 1) / (d2 - 1)
               end if
               do i3 = 1 , d3
                  if( d3 .eq. 1 ) then
                     z = -1.0d0
                  else 
                     z = -1.0d0 + 2.0d0 * (i3 - 1) / (d3 - 1)
                  end if
                  write(u,*) x
                  write(u,*) y
                  write(u,*) z
                  write(u,*) a(i1,i2,i3)
               end do
            end do
         end do

         if( ltrace ) then
            write(0,1000) d1, d2, d3, fname
1000        format(' d3put_XYZ3DData0: Wrote ',i3,' x ',i3,' x ',
     *             i3,' to ',a)
         end if
         close(u)
         return

900      continue
            write(0,*) 'd3put_XYZ3DData0: Could not open ', fname
         return

      end

c-----------------------------------------------------------------------
c
c     Four-byte version of above routine.
c 
c-----------------------------------------------------------------------

      subroutine s3put_XYZ3DData0(fname,a,d1,d2,d3)

         implicit      none

         integer       getu

         character*(*) fname
         integer       d1,     d2,     d3
         real          a(d1,d2,d3)

         integer       u,      i1,     i2,     i3
         real          x,      y,      z

         logical       ltrace
         parameter   ( ltrace = .true. )

         if( d1 .lt. 1  .or.  d2 .lt. 1  .or.  d3 .lt. 1 ) then
            write(0,*) 's3put_XYZ3DData0: Invalid dimension.'
            return
         end if
      
         u = getu()
         open(u,file=fname,form='formatted',status='unknown',err=900)

         write(u,*) d1
         write(u,*) d2
         write(u,*) d3
         do i1 = 1 , d1
            if( d1 .eq. 1 ) then
               x = -1.0d0
            else 
               x = -1.0d0 + 2.0d0 * (i1 - 1) / (d1 - 1)
            end if
            do i2 = 1 , d2
               if( d2 .eq. 1 ) then
                  y = -1.0d0
               else 
                  y = -1.0d0 + 2.0d0 * (i2 - 1) / (d2 - 1)
               end if
               do i3 = 1 , d3
                  if( d3 .eq. 1 ) then
                     z = -1.0d0
                  else 
                     z = -1.0d0 + 2.0d0 * (i3 - 1) / (d3 - 1)
                  end if
                  write(u,*) 2.0d0 * x
                  write(u,*) y
                  write(u,*) z
                  write(u,*) a(i1,i2,i3)
               end do
            end do
         end do

         if( ltrace ) then
            write(0,1000) d1, d2, d3, fname
1000        format(' s3put_XYZ3DData0: Wrote ',i3,' x ',i3,' x ',
     *             i3,' to ',a)
         end if
         close(u)
         return

900      continue
            write(0,*) 's3put_XYZ3DData0: Could not open ', fname
         return

      end

c-----------------------------------------------------------------------
c
c     Writes 3d uniform mesh function to file which can be read by 
c     explorer module ReadXYZ3DData.
c
c     This version takes coordinate vectors as input.
c
c-----------------------------------------------------------------------

      subroutine d3put_XYZ3DData1(fname,a,c1,c2,c3,d1,d2,d3)

         implicit      none

         integer       getu

         character*(*) fname
         integer       d1,     d2,     d3
         real*8        a(d1,d2,d3),
     *                 c1(d1), c2(d2), c3(d3)

         integer       u,      i1,     i2,     i3
         real*8        x,      y,      z

         logical       ltrace
         parameter   ( ltrace = .true. )

         if( d1 .lt. 1  .or.  d2 .lt. 1  .or.  d3 .lt. 1 ) then
            write(0,*) 'd3put_XYZ3DData1: Invalid dimension.'
            return
         end if
      
         u = getu()
         open(u,file=fname,form='formatted',status='unknown',err=900)

         write(u,*) d1
         write(u,*) d2
         write(u,*) d3
         do i1 = 1 , d1
            do i2 = 1 , d2
               do i3 = 1 , d3
                  write(u,*) c1(i1)
                  write(u,*) c2(i2)
                  write(u,*) c3(i3)
                  write(u,*) a(i1,i2,i3)
               end do
            end do
         end do

         if( ltrace ) then
            write(0,1000) d1, d2, d3, fname
1000        format(' d3put_XYZ3DData1: Wrote ',i3,' x ',i3,' x ',
     *             i3,' to ',a)
         end if
         close(u)
         return

900      continue
            write(0,*) 'd3put_XYZ3DData1: Could not open ', fname
         return

      end

