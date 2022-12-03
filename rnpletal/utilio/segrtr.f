c----------------------------------------------------------------------
c     Rescales t and r 
c    
c     History:        ~matt/utilio/critfmt.f
c
c     Author:         Matthew W. Choptuik   choptuik@physics.ubc.ca
c     Institution:    The University of British Columbia
c     Created:        Tue Mar 14 12:14:38 PST 2000
c     Last Modified:
c----------------------------------------------------------------------

      program      segrtr

      implicit     none

c=======================================================================
c     Begin io.inc
c=======================================================================
      integer         stderr,      stdin,     stdout
      parameter     ( stderr = 0,  stdin = 5, stdout = 6 )

      real*8          s2r8
      real*8          r8arg
      integer         s2i4
      integer         i4arg
      character*128   sarg

		real*8          r8_never
		parameter     ( r8_never = -1.0d-60 )

		integer         i4_never 
		parameter     ( i4_never = -2 000 000 000 )
c=======================================================================
c     End io.inc
c=======================================================================

      character*128  catsqz
      integer        iargc,     indlnb,    getu

      real*8       log_0
      parameter  ( log_0 = -1.0e20 )

      character*64 intag,     outtag
      real*8       scale

      character*64 arg
      character*64 infname,   outfname

      integer      maxfcn
      parameter  ( maxfcn = 100 )

      character*16 fname(maxfcn)

      integer      maxn
      parameter  ( maxn = 10 000 )

      integer      nt_times
      real*8       times(maxn)

		integer      rc
      integer      uin,                  uout 

      integer      useg                 
      integer      nt_seg,               nfcn,
     &             nr
      real*8       t,                    tnew

      real*8       r(maxn),              f(maxn),
     &             rnew(maxn)

      integer      i,                    j,                    
     &             n

      logical      ltrace
      parameter  ( ltrace = .true. )

      if( iargc() .lt. 3 ) goto 900

      scale = r8arg(1,r8_never)
		if( scale .eq. r8_never ) then
			call sload(arg,' ')
			call getarg(1,arg)
			write(0,*) 'segrtr: Invalid scale: '//arg(1:indlnb(arg))
			stop
		end if

      call sload(intag,' ')
      call sload(outtag,' ')
      call getarg(2,intag)
      call getarg(3,outtag)

      infname  = catsqz(intag,'.segdat')
      outfname = catsqz(outtag,'.segdat')
      if( ltrace ) then
         write(0,1000) infname(1:indlnb(infname)), 
     &                 outfname(1:indlnb(outfname)),
     &                 scale
1000     format(' segrtr: <',a,'> --> <',a,'>  scale: ',1p,e15.5)
      end if

		uin = getu() 
		open(uin,file=infname(1:indlnb(infname)),form='unformatted',
     &     status='old',iostat=rc)
		if( rc .ne. 0 ) then
			write(0,*) 'segrtr: Error opening (OLD) '//
     &              infname(1:indlnb(infname))
			stop
		end if

		if( ltrace ) then
			write(0,*) 'segrtr: Opened '//
     &              infname(1:indlnb(infname))
		end if

		uout = getu() 
		open(uout,file=outfname(1:indlnb(outfname)),form='unformatted',
     &     status='new',iostat=rc)
		if( rc .ne. 0 ) then
			write(0,*) 'segrtr: Error opening (NEW) '//
     &              outfname(1:indlnb(outfname))
			stop
		end if

		if( ltrace ) then
			write(0,*) 'segrtr: Opened '//
     &              outfname(1:indlnb(outfname))
		end if


      call segscn(uin,fname,nfcn,nt_seg)
      if( ltrace ) then
         write(0,1100) infname(1:indlnb(infname)), nfcn, nt_seg
1100     format(' segrtr: ',a,' contains ',i2,' functions, ',i4,
     &          ' groups.')
      end if
      if( nfcn .ne. 1 ) then
         write(0,*) 'segrtr: '//infname(1:indlnb(infname))//
     &              ' does not contain single function'
         stop
      end if
      rewind(uin)

      read(uin)      nfcn
      write(uout)    nfcn
      read(uin)   ( fname(i) , i = 1 , nfcn )
      write(uout) ( fname(i) , i = 1 , nfcn )

      n = 0
 500  continue 
         read(uin, end=750) t,  nr
         tnew = scale * t
         write(uout)        tnew,  nr

         read(uin)   ( r(j) , j = 1 , nr )
         call dvsm(r,scale,rnew,nr)
         write(uout) ( rnew(j) , j = 1 , nr )

         read(uin)   ( f(j) , j = 1 , nr )
         write(uout) ( f(j) , j = 1 , nr )
      go to 500
 750  continue
      close(uout)

      stop

900   continue
         write(0,*) 'usage: segrtr <scale> <in tag> <out tag>'
         write(0,*)
         write(0,*) '       Requires'
         write(0,*)
         write(0,*) '          <in tag>.segdat'
         write(0,*)
         write(0,*) '       Creates '
         write(0,*)
         write(0,*) '          <out tag>.segdat'
         write(0,*)
      stop

      end
