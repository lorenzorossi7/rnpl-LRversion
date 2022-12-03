c=======================================================================
c     Utility routines for retrieving numeric values from 
c     environment variables.   Eventually should be moved to 
c     libutilio.
c=======================================================================
      real*8 function r8env(env,defval)
         implicit         none
         character*(*)    env
         real*8           defval

         character*256    stringval
         real*8           val

         integer          rc
   
         r8env = defval
         call getenv(env,stringval)
         if( stringval .ne. ' ' ) then
            read(stringval,*,iostat=rc) val
            if( rc .eq. 0 ) then
               r8env = val
            else 
               r8env = defval 
            end if
         end if

         return
      end

      integer function i4env(env,defval)
         implicit         none
         character*(*)    env
         integer          defval

         character*256    stringval
         integer          val

         integer          rc
   
         i4env = defval
         call getenv(env,stringval)
         if( stringval .ne. ' ' ) then
            read(stringval,*,iostat=rc) val
            if( rc .eq. 0 ) then
               i4env = val
            else 
               i4env = defval 
            end if
         end if

         return
      end

      character*(*) function senv(env)
         implicit         none
         character*(*)    env

         call getenv(env,senv)

         return
      end
