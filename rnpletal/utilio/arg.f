c
c     Argument processing routines 
c
      double precision function r8arg(argno,defval)

         implicit       none
         
         real*8         r8_never
         parameter    ( r8_never = -1.0d-60 )

         integer        i4_never 
         parameter    ( i4_never = -2 000 000 000 )

         logical        onedot 
         integer        iargc
         real*8         s2r8

         integer        argno
         real*8         defval

         character*32   argi

         if( argno .le. iargc() ) then
            call getarg(argno,argi)
            if( onedot(argi) ) then
               r8arg = defval
            else 
               r8arg = s2r8(argi)
               if( r8arg .eq. r8_never ) then
                  r8arg = defval
               end if
            end if
         else
            r8arg = defval
         end if

         return

      end

      integer function i4arg(argno,defval)

         implicit       none
         
         real*8         r8_never
         parameter    ( r8_never = -1.0d-60 )

         integer        i4_never 
         parameter    ( i4_never = -2 000 000 000 )

         logical        onedot
         integer        iargc
         integer        s2i4

         integer        argno
         integer        defval

         character*32   argi

         if( argno .le. iargc() ) then
            call getarg(argno,argi)
            if( onedot(argi) ) then
               i4arg = defval
            else
               i4arg = s2i4(argi)
               if( i4arg .eq. i4_never ) then
                  i4arg = defval
               end if
            end if
         else
            i4arg = defval
         end if

         return

      end

      character*(*) function sarg(argno,defval)

         implicit        none

         integer         iargc

         logical         onedot
         integer         argno
         character*(*)   defval
      
         character*256   buffer

         if( argno .le. iargc() ) then
            call getarg(argno,buffer)
            if( onedot(sarg) ) then
               sarg = defval
				else
					sarg = buffer
            end if
         else 
            sarg = defval
         end if

         return
      
      end
