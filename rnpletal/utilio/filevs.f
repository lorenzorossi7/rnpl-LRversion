c-----------------------------------------------------------------------
c
c    "Fake" vsxynt routine which uses .segdat files ...
c
c    Modified to test for environment variables
c 
c    FVSTRACE
c    FVSAPPEND
c
c    Modified to allow up to 1024-character file names
c
c-----------------------------------------------------------------------
      integer function vsxynt(name,t,x,y,n)

         implicit          none

         integer           getu,            indlnb
         character*1024    xxspec

         logical           ltrace
         parameter       ( ltrace = .true. )

         logical           ltrace_full
         parameter       ( ltrace_full = .false. )

         logical           fvstrace,        fvsappend
         character*1024    evalue

         character*(*)     name
         real*8            t
         integer           n
         real*8            x(n),            y(n)

         integer           ifile,           j
         character*1024    lfile 

         character*16      fname(1)
         integer           nfcn
         parameter       ( nfcn = 1 )
c
c        "Static" vbls ...
c
         integer           mxfile
         parameter       ( mxfile = 100 )

         character*1024    vfile(mxfile)
         integer           vu(mxfile)
         integer           nfile

         save              vfile,           vu,          nfile

         data              nfile / 0 /
c
c        See if some environment variables are set ...
c
         call getenv('FVSTRACE',evalue)
         fvstrace = evalue .ne. ' '
         call getenv('FVSAPPEND',evalue)
         fvsappend = evalue .ne. ' '
         if( ltrace_full .or. fvstrace ) then
            write(0,*) '>>> vsxynt: fvstrace ', fvstrace, 
     *                 '  fvsappend ', fvsappend
         end if
c
c        If invoked with n < 0 will close, reopen and position to 
c        eof all files currently open within this routine ...
c
         if( n .lt. 0 ) then
            if( nfile .gt. 0 ) then
               do ifile = 1 , nfile
                  lfile = vfile(ifile)
                  if( ltrace_full .or. fvstrace ) then
                     write(0,*) '>>> vsxynt: Closing '//
     *                          lfile(1:indlnb(lfile))//'.segdat'//
     *                          '  Unit: ', vu(ifile)
                  end if
                  close(vu(ifile))
                  vu(ifile) = getu()
                  if( ltrace_full .or. fvstrace ) then
                     write(0,*) '>>> vsxynt: Reopening '//
     *                          lfile(1:indlnb(lfile))//'.segdat'//
     *                          '  Unit: ', vu(ifile)
                  end if
                  open(vu(ifile),
     *                 file=lfile(1:indlnb(lfile))//'.segdat',
     *                 form='unformatted',status='unknown',err=95)
                  call toeof(vu(ifile),1)
                  if( ltrace .or. fvstrace ) then
                     write(0,*) '>>> vsxynt: Rewound '//
     *                          lfile(1:indlnb(lfile))//'.segdat'
                  end if
               end do
            else 
               call err2('vsxynt','No files open.')
            end if
            vsxynt = nfile
            return
         end if

         lfile = xxspec(name)
         do 10 ifile = 1 , nfile
            if( lfile .eq. vfile(ifile) ) go to 15
 10      continue
         nfile = nfile + 1
         if( nfile .gt. mxfile ) then
            call err2('vsxynt','Too many files')
            vsxynt = - 1
            return
         end if
         ifile = nfile
         vfile(ifile) = lfile
         vu(ifile) = getu()
         if( ltrace_full .or. fvstrace ) then
            write(0,*) '>>> vsxynt:: Got unit ',vu(ifile)
         end if
         open(unit=vu(ifile),file=lfile(1:indlnb(lfile))//'.segdat',
     *        form='unformatted',err=90)
         if( ltrace .or. fvstrace ) then
            write(0,*) '>>> vsxynt:: Opened <'//
     *                 lfile(1:indlnb(lfile))//'.segdat'//'>'
         end if
         if( fvsappend ) then
            read(vu(ifile),end=14)
            call toeof(vu(ifile),1)
            if( ltrace_full .or. fvstrace ) then
               write(0,*) '>>> vsxynt:: Positioned <'//
     *                 lfile(1:indlnb(lfile))//'.segdat'//
     *                 ' to EOF.'
            end if
         go to 15
14       continue
            rewind vu(ifile)
         end if

         fname(1) = name
         write(vu(ifile)) nfcn
         write(vu(ifile)) ( fname(j) , j = 1 , nfcn )
         go to 20
 
 15      continue
         if( ltrace_full .or. fvstrace ) then
            write(0,*) '>>> vsxynt:: Appending to <'//
     *                 lfile(1:indlnb(lfile))//'.segdat'//'>'
         end if

 20      continue
         write(vu(ifile)) t, n
         write(vu(ifile)) ( x(j) , j = 1 , n )
         write(vu(ifile)) ( y(j) , j = 1 , n )

         vsxynt = n

         return 

90       continue
            write(0,*) '>>> vsxynt:: Error opening <'//
     *                 lfile(1:indlnb(lfile))//'.segdat'//'>'
            vsxynt = -1
         return
95       continue
            write(0,*) '>>> vsxynt:: Error reopening <'//
     *                 lfile(1:indlnb(lfile))//'.segdat'//'>'
            vsxynt = -1
         return

      end

		integer function vsmxynt(name,start,time,nvec,x,y)

			implicit      none

			integer       vsxynt
	
			character*(*) name
			integer       start(*)
			real*8        time(*)
			integer       nvec
			real*8        x(*),      y(*)

			integer       ivec

			if( nvec .gt. 0 ) then
				do ivec = 1 , nvec 
					vsmxynt = vsxynt(name,time(ivec),
     *                          x(start(ivec)),y(start(ivec)),
     *                          start(ivec+1) - start(ivec))
				end do
			end if

			return
		end 
c
c     Remainder of routines are stubs for compatibility with "real" vs ...
c
      INTEGER FUNCTION VSCLOSE()
         VSCLOSE = 0
         RETURN
      END

      INTEGER FUNCTION VSCONNECT()
         VSCONNECT = 0
         RETURN
      END

      INTEGER FUNCTION VSGETPARAMETER()
         VSGETPARAMETER = 0
         RETURN
      END

      INTEGER FUNCTION VSGETVSPID()
         VSGETVSPID = 0
         RETURN
      END

      INTEGER FUNCTION VSGLT()
         VSGLT = 0
         RETURN
      END

      INTEGER FUNCTION VSMXY1()
         VSMXY1 = 0
         RETURN
      END

      INTEGER FUNCTION VSNAMEQ()
         VSNAMEQ = 0
         RETURN
      END

      INTEGER FUNCTION VSOPEN()
         VSOPEN = 0
         RETURN
      END

      INTEGER FUNCTION VSRESET()
         VSRESET = 0
         RETURN
      END

      INTEGER FUNCTION VSSETAF()
         VSSETAF = 0
         RETURN
      END

      INTEGER FUNCTION VSSETMAXLXY()
         VSSETMAXLXY = 0
         RETURN
      END

      INTEGER FUNCTION VSSETPARAMETER()
         VSSETPARAMETER = 0
         RETURN
      END

      INTEGER FUNCTION VSSETTHIN()
         VSSETTHIN = 0
         RETURN
      END

      INTEGER FUNCTION VSSTATUS()
         VSSTATUS = 0
         RETURN
      END

      INTEGER FUNCTION VSXN()
         VSXN = 0
         RETURN
      END

      INTEGER FUNCTION VSXNT()
         VSXNT = 0
         RETURN
      END

      INTEGER FUNCTION VSXY1()
         VSXY1 = 0
         RETURN
      END

      INTEGER FUNCTION VSXY2()
         VSXY2 = 0
         RETURN
      END

      INTEGER FUNCTION VSXYN()
         VSXYN = 0
         RETURN
      END

      INTEGER FUNCTION VSHANG()
         VSHANG = 0
         RETURN
      END

      INTEGER FUNCTION VSPING()
         VSPING = 0
         RETURN
      END

      INTEGER FUNCTION VSGXYNI()
         VSGXYNI = 0
         RETURN
      END

      INTEGER FUNCTION VSGNXYNI()
         VSGNXYNI = 0
         RETURN
      END
