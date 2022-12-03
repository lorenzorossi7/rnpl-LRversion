#ifdef SGI
c-----------------------------------------------------------------------
c
c     Predicate ... is this a batch job.
c
c-----------------------------------------------------------------------

      logical function pbatch()

         implicit      none

         character*128 buffer
      
         call sload(buffer,' ')
         call getenv('ENVIRONMENT',buffer) 
         pbatch = pbatch .and. index(buffer,'BATCH') .ne. 0

         return

      end

c-----------------------------------------------------------------------
c
c     Returns number of CPU seconds used thus far ... 
c
c-----------------------------------------------------------------------

      double precision function second()

         implicit      none
         
         real          etime
         real          tarray(2)

         second = etime(tarray)

         return

      end 
c-----------------------------------------------------------------------
c
c     Returns -1 if cpu usage is within val(2) of val(1) read 
c     from maxcpu.dat ...
c
c-----------------------------------------------------------------------

      integer function pmaxcpu()

         implicit      none
         
         real*8        second

         logical       ltrace 
         parameter   ( ltrace = .true. )

         real*8        maxcpusecs,          val(10)
         integer                            nval
         common      / maxcpusecs /
     *                 val,                 nval

         logical       first,               disabled

         save          first,               disabled
         data          first / .true. /,    disabled / .false. /

         pmaxcpu = 0
         if( disabled ) then
            return
         else
            if( first ) then
               call dvfget('maxcpu.dat',val,nval,10)
               if( nval .ne. 2 ) then
                  disabled = .true.
                  if( ltrace ) then
                     write(*,*) 'pmaxcpu: Could not read two values ',
     *                          'from maxcpu.dat.  Routine disabled.'
                  end if
                  return
               else 
                  if( ltrace ) then
                     write(*,1000) int(val(1)),  int(val(2))
1000                 format(' pmaxcpu: Cpu limit: ',i5,'  Will stop ',
     *                      'when within ',i3)
                  end if
               end if
               first = .false.
            end if
            if( second() .gt. abs(val(1) - val(2)) ) then
               if( ltrace ) then
                  write(*,*) 'pmaxcpu: ====== CPU THRESHOLD ',
     *                       'EXCEEDED ======'
               end if
               pmaxcpu = -1
            end if
         end if

         return

      end
#else
      logical function pbatch()
         pbatch = .true.
         return
      end
#endif
