c
c     string load with character.
c
      subroutine sload(s,c)

         implicit       logical (a-z)

         character*(*)  s
         character*1    c

         integer        i

         do 10 i = 1 , len(s)
            s(i:i) = c
 10      continue

         return

      end
c
c     Upper and lower case conversion.
c
      character*(*) function l2u(s)

         implicit        logical (a-z)

         character*(*)   s

         integer         i,    ix

         character*26    uc,   lc
         data            uc / 'ABCDEFGHIJKLMNOPQRSTUVWXYZ' /,
     *                   lc / 'abcdefghijklmnopqrstuvwxyz' /

         do 10 i = 1 , len(s)
             ix = index(lc,s(i:i))
            if( ix .ne. 0 ) then
               l2u(i:i) = uc(ix:ix)
            else
               l2u(i:i) = s(i:i)
            end if
 10      continue

         return

      end

      character*(*) function u2l(s)

         implicit        logical (a-z)

         character*(*)   s

         integer         i,    ix

         character*26    uc,   lc
         data            uc / 'ABCDEFGHIJKLMNOPQRSTUVWXYZ' /,
     *                   lc / 'abcdefghijklmnopqrstuvwxyz' /

         do 10 i = 1 , len(s)
             ix = index(uc,s(i:i))
            if( ix .ne. 0 ) then
               u2l(i:i) = lc(ix:ix)
            else
               u2l(i:i) = s(i:i)
            end if
 10      continue

         return

      end
c
c     CMS --> UNIX filename conversion.
c
      character*(*) function c2ufn(s)

         implicit         logical (a-z)

         character*1024   u2l

         character*(*)    s

         integer          i, o, lens

         lens = len(s)
         if( lens .gt. 1024 ) then
            write(*,*) '<<< c2ufn:: input file name too long. >>>'
            stop
         end if

         i = 1
         o = 1

 100     continue
         if( s(i:i) .ne. ' ' ) go to 120
            i = i + 1
         if( i .gt. lens ) go to 300
         go to 100

 120     continue
         if( s(i:i) .eq. ' '  .or. s(i:i) .eq. '.' ) go to 140
            c2ufn(o:o) = s(i:i)
            o = o + 1
            i = i + 1
         if( i .gt. lens ) go to 320
         go to 120

 140     continue
         c2ufn(o:o) = '.'
         o = o + 1
         i = i + 1

 160     continue
         if( s(i:i) .ne. ' ' ) go to 180
            i = i + 1
         if( i .gt. lens ) go to 320
         go to 160

 180     continue
         if( s(i:i) .eq. ' ' ) go to 200
            c2ufn(o:o) = s(i:i)
            o = o + 1
            i = i + 1
         if( i .gt. lens ) go to 220
         go to 180

 200     continue
         if( o .gt. lens ) go to 220
            c2ufn(o:o) = ' '
            o = o + 1
         go to 200

 220     continue
         c2ufn = u2l(c2ufn)

         return

 300     continue
            write(*,*) '<<< c2ufn:: No file name detected. >>>'
            c2ufn = '*UNDEF*'
         stop

 320     continue
            write(*,*) '<<< c2ufn:: No file mode detected. >>>'
            write(*,*) '<<< c2ufn:: Argument: ',s,' >>>'
            c2ufn = '*UNDEF*'
         stop

      end
c
c     CMS --> UNIX filename conversion.
c     subroutine version which also returns length of string after
c     conversion 
c
      subroutine c2ufnl(s,c2ufn,l)

         implicit         logical (a-z)

         character*1024   u2l
         integer          indlnb

         character*(*)    s, c2ufn

         integer          i, o, lens, l

         lens = len(s)
         if( lens .gt. 1024 ) then
            write(*,*) '<<< c2ufn:: input file name too long. >>>'
            stop
         end if

         i = 1
         o = 1

 100     continue
         if( s(i:i) .ne. ' ' ) go to 120
            i = i + 1
         if( i .gt. lens ) go to 300
         go to 100

 120     continue
         if( s(i:i) .eq. ' '  .or. s(i:i) .eq. '.' ) go to 140
            c2ufn(o:o) = s(i:i)
            o = o + 1
            i = i + 1
         if( i .gt. lens ) go to 320
         go to 120

 140     continue
         c2ufn(o:o) = '.'
         o = o + 1
         i = i + 1

 160     continue
         if( s(i:i) .ne. ' ' ) go to 180
            i = i + 1
         if( i .gt. lens ) go to 320
         go to 160

 180     continue
         if( s(i:i) .eq. ' ' ) go to 200
            c2ufn(o:o) = s(i:i)
            o = o + 1
            i = i + 1
         if( i .gt. lens ) go to 220
         go to 180

 200     continue
         if( o .gt. lens ) go to 220
            c2ufn(o:o) = ' '
            o = o + 1
         go to 200

 220     continue
         c2ufn = u2l(c2ufn)
         l = indlnb(c2ufn)

         return

 300     continue
            write(*,*) '<<< c2ufn:: No file name detected. >>>'
            c2ufn = '*UNDEF*'
         stop

 320     continue
            write(*,*) '<<< c2ufn:: No file mode detected. >>>'
            write(*,*) '<<< c2ufn:: Argument: ',s,' >>>'
            c2ufn = '*UNDEF*'
         stop

         end
c
c        Counts occurences of c in s.
c
         integer function ncins(s,c)

            implicit      logical (a-z)

            character*(*) s
            character*1   c

            integer       is

            ncins = 0
            do 10 is = 1 , len(s)
               if( s(is:is) .eq. c ) then
                  ncins = ncins + 1
               end if
 10         continue

            return
               
         end
c
c     For parameter handling; added '-' detection since '-' parses
c     as 0 under Fortran formatted (and some free-formatted reads).
c     Thanks to Tiffany Shaw (UBC PHYS 410, 2002) for bringing this 
c     to my attention.
c
      logical function onedot(s)

         character*(*)     s

         integer           indlnb

         onedot = (s(1:1) .eq. '.'  .or.  s(1:1) .eq. '-')
     &            .and.  indlnb(s) .eq. 1

         return

      end
c
      logical function query(s)

         character*(*)     s

         integer           indlnb

         query = s(1:1) .eq. '?'  .and.  indlnb(s) .eq. 1

         return

      end
c
      logical function affirm(s)

         implicit          none

         character*(*)     s

         affirm = s(1:1) .eq. 'y'  .or.  s(1:1) .eq. 'Y'

         return

      end
c
c     returns 16 digit representation of x ...
c
      character*16 function r8toc16(x)

         implicit        none

         integer         indlnb

         real*8          x
      
         character*24    buffer

         write(buffer,1000) abs(x)
1000     format(17PE22.16)
         r8toc16 = buffer

         return

      end
c
c     Converts integer to character ...
c
      character*(*) function i4toc(x)

         implicit        none

         integer         indlnb,   indfnb
      
         integer         x
         character*16    buffer
         write(buffer,fmt='(I16)') x
         i4toc = buffer(indfnb(buffer):indlnb(buffer))

         return

      end

c
c     Returns length of right--most substring of s1 containing characters
c     in s2.
c
      integer function icspan(s1,s2)

         implicit       none

         character*(*)  s1,     s2

         integer        i

         do 100 i = 1 , len(s1)
            if( index(s2,s1(i:i)) .eq. 0 ) then
               icspan = i - 1
               return
            end if 
 100     continue
         icspan = len(s1)

         return

      end
c
c     Attempts to extract a real*8 number from the right--most 
c     contigous numerical field of s.
c
      double precision function r8frs(s)

         implicit      none

         integer       icspan,        indlnb

         character*(*) s

         character*12  span_string
         parameter   ( span_string = '0123456789.-' )
         character*32  sfmt

         real*8        default_r8,
     *                 never_r8
         parameter   ( default_r8  = -1.0d0, 
     *                 never_r8    =  1.0d60 )

         integer       n
         
         n = icspan(s,span_string)
         if( n .gt. 0 ) then
            r8frs = never_r8
            if( n .lt. 10 ) then
               write(sfmt,100) n
            else 
               write(sfmt,101) n
            end if
100         format('(G',i1,'.0)')
101         format('(G',i2,'.0)')
            read(s(1:n),fmt=sfmt,end=999,err=999) r8frs
            if( r8frs .eq. never_r8 ) go to 999
            return
         end if

 999     continue
            write(*,1000) s(1:indlnb(s))
1000        format('<<< r8frs:: Warning: Could not convert ',a,
     *             '. Returning -1.0. >>>')
            r8frs = default_r8
         return

      end

c
c     String to double precision ...
c
      double precision function s2r8(s)

         implicit      none

         real*8       r8_never
         parameter  ( r8_never = -1.0d-60 )

         integer      i4_never 
         parameter  ( i4_never = -2 000 000 000 )

         integer       indlnb

         character*(*) s     

         character*32  buffer
         integer       lens

         double precision default
         parameter      ( default = 0.0d0 )

         lens = indlnb(s)
         if( lens .gt. 99 ) then
            write(*,*) '>>> s2r8:: String too long for conversion.'
            s2r8 = default
         else 
            if( lens .le. 9 ) then
               write(buffer,100) lens
            else 
               write(buffer,101) lens
            end if
 100        format('(G',i1,'.0)')
 101        format('(G',i2,'.0)')
            read(s,fmt=buffer,end=900,err=900) s2r8
         end if

         return

 900        s2r8 = r8_never 
         return

      end
c
c     String to integer ...
c
      integer function s2i4(s)

         implicit      none

         real*8       r8_never
         parameter  ( r8_never = -1.0d-60 )

         integer      i4_never 
         parameter  ( i4_never = -2 000 000 000 )

         integer       indlnb

         character*(*) s     

         character*32  buffer
         integer       lens

         integer       default
         parameter   ( default = 0 )

         lens = indlnb(s)
         if( lens .gt. 99 ) then
            write(*,*) '>>> s2i4:: String too long for conversion.'
            s2i4 = default
         else 
            if( lens .le. 9 ) then
               write(buffer,100) lens
            else 
               write(buffer,101) lens
            end if
 100        format('(I',i1,')')
 101        format('(I',i2,')')
            read(s,fmt=buffer,end=900,err=900) s2i4
         end if

         return

 900        s2i4 = i4_never
         return

      end
c
c     Removes non-alphanumericunderscore chars ...
c
      character*(*) function xxspec(s)
         
         implicit         none

         character*1024   u2l

         character*(*)    s

         character*63     anu
         parameter (      anu = 'abcedfghijklmnopqrstuvwxyz'//
     *                          'ABCEDFGHIJKLMNOPQRSTUVWXYZ'//
     *                          '0123456789_' )

         character*1024   buffer,       lbuffer
         integer          buflen
         parameter      ( buflen = 1024 )

         integer          i,            j

         i = 0
         do 10 j = 1 , len(s)
            if( index(anu,s(j:j)) .gt. 0 ) then
               i = i + 1
               if( i .gt. buflen ) then
                  write(*,*) '>>> xxspec:: Warning ... buffer overflow'
                  go to 20
               end if
               buffer(i:i) = s(j:j)
            end if
 10      continue

 20      continue
         if( i .gt. 0 ) then
            lbuffer = u2l(buffer(1:i))
            xxspec = lbuffer(1:i)
         else 
            write(*,*) '>>> xxspec:: Warning ... can not convert ',
     *                 s
            xxspec = s
         end if

         return

      end 
c
c     Returns index of first string in vector vstring which 
c     contains substring key ...
c
      integer function indexv(vstring,nstring,key)

         implicit      none

         integer       nstring
         character*(*) vstring(nstring)
         character*(*) key

         do indexv = 1 , nstring
            if( index(key,vstring(indexv)) .gt. 0 ) return
         end do
         indexv = 0

         return

      end
c
c     Strips leading and trailing occurences of character ... 
c
      character*(*) function strip(s,char)

         implicit         none

         character*(*)    s
         character*1      char

         integer          bgn,      end,     pos

         bgn = 0
         do pos = 1 , len(s)
            if( s(pos:pos) .ne. char ) then
               bgn = pos
         go to 100
            end if
         end do
100      continue

         end = 0
         do pos = len(s) , 1 , -1
            if( s(pos:pos) .ne. char ) then
               end = pos
         go to 200
            end if
         end do
200      continue

         if( end .ge. bgn ) then
            strip = s(bgn:end)
         else 
            strip = ' '
         end if

         return
         
      end 
c
c     Strips leading ...
c
      character*(*) function lstrip(s,char)

         implicit         none

         character*(*)    s
         character*1      char

         integer          bgn,      end,     pos

         bgn = 0
         do pos = 1 , len(s)
            if( s(pos:pos) .ne. char ) then
               bgn = pos
         go to 100
            end if
         end do
100      continue

         end = len(s)

         if( end .ge. bgn ) then
            lstrip = s(bgn:end)
         else 
            lstrip = ' '
         end if

         return
         
      end 
