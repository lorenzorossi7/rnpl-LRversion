c-----------------------------------------------------------------------
c     Selects "slices" of input array, ain, returning values in aout.
c-----------------------------------------------------------------------
      subroutine d3slice(ain,aout,d1in,d2in,d3in,d1out,d2out,d3out,
     &                   d1sl,d2sl,d3sl)

         implicit     none
         integer      d1in, d2in, d3in, d1out, d2out, d3out
         real*8       ain(d1in,d2in,d3in), aout(d1out,d2out,d3out)
         integer      d1sl(d1out), d2sl(d2out), d3sl(d3out)
         
         integer      i, j, k 

         do k = 1 , d3out
            do j = 1 , d2out
               do i = 1 , d1out
                  aout(i,j,k) = ain(d1sl(i),d2sl(j),d3sl(k))
               end do
            end do
         end do

         return  

      end

c-----------------------------------------------------------------------
c     Dumps 3--array labelled with LABEL on UNIT.
c-----------------------------------------------------------------------
      subroutine d3dump(a,d1,d2,d3,label,unit)
         implicit       none

         integer        d1,       d2,       d3,
     &                  unit
         real*8         a(d1,d2,d3)
         character*(*)  labeL
         integer        i,        j,        k,
     &                  st

         if( d1 .gt. 0  .and.  d2 .gt. 0  .and.  d3 .gt. 0 ) then
            write(unit,100) label
 100        format(/' <<< ',a,' >>>')
            do k = 1 , d3
               write(unit,105) k
 105           format(' <<< Plane: ',i4,'. >>>')
               do j = 1 , d2
                  st = 1
 110              continue
                     write(unit,120) ( a(i,j,k) , i = st , min(st+3,d1))
 120                 format(' ',4(1pe19.10))
                  st = st + 4
                  if( st .le. d1 ) go to 110
                  write(unit,140)
 140              format(' ')
               end do
            end do
         end if

         return

      end
