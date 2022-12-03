c-----------------------------------------------------------------------
c
c     History: s2s, ss
c
c     Sends data to server using vsmxy1 ... intention is to have 
c     same functionality as s2s but speedier ... 
c
c     Only plots one function per invocation, uses open by name 
c     mechanism and name from segdat file.
c
C     Modified to take stride argument; send only every stride'th
c     spatial value ...
c
c     NOTE: Some size parameters need to be adjusted on some 
c     machines (i.e. charon.cc.utexas.edu)
c
c-----------------------------------------------------------------------
c
      implicit         none              
c
      integer          vsmxynt,                 i4arg

c
      integer          indlnb,                  irdivn,
     *                 iargc
      logical          onedot,                  query
c
      character*1024   fs,                      cs1
      logical          p4fs,                    p4cs1
c
      character*80     fmt
      character*1024   lfname
      integer          lnb
c
      integer          max_nvec
      parameter      ( max_nvec = 100 000 )
      integer          ivec 
      integer          start(max_nvec + 1)

      real*8           time(max_nvec)
      integer          tag(max_nvec)
c
      integer          maxnf
      parameter      ( maxnf = 100 )
c
      integer          max_nevent
c-----------------------------------------------------------------------
c     enable following for default
      parameter      ( max_nevent = 8 000 000 )

c-----------------------------------------------------------------------
c     enable following for aurora.nic.ualberta.ca
c     parameter      ( max_nevent = 1 000 000 )

c-----------------------------------------------------------------------
c     enable following for charon
c     parameter      ( max_nevent = 1 000 000 )

      integer          max_nr
		parameter      ( max_nr = 100 000 )
      real*8           x(max_nevent),           y(max_nevent)
	   real*8           readx(max_nr),           ready(max_nr)
c
      character*16     fname(maxnf)
c
      integer          iota(maxnf),   fiv(maxnf),
     *                 tiv(max_nvec)
c
      integer          udata,         ulist,       user,
     *                 compar,        uout
      parameter      ( udata = 11,    ulist = 4,   user = 6,
     *                 compar = 2)    
c
      real*8           ttime
c
      integer          group, if, ifcn, iskip, it, j, lfiv, ltiv,
     *                 nfcn, nt, prskip(maxnf), psskip, skip, 
     *                 readnr, tnr, nrout
      integer          nvec
c
c     PAR_NAME overrides FNAME (kludge to avoid window collision 
c     with query by name.
c
      integer        narg,     larg
      character*256  argi,     pgname,     incdnm,
     *               par_name
c
      integer        par_if,   par_it
      logical        p4f,      p4t,        name_from_segdat
c
      integer        stride
c
      integer        rc

      integer        max_wt
      parameter    ( max_wt = 10 )
      integer        wt(max_wt)
      integer        lastiv

      logical        auto_thin

      integer        warn_freq
      parameter    ( warn_freq = 10 )
      integer        loop

      integer        trace
      parameter    ( trace = 0 )

		logical        ltrace 
		parameter    ( ltrace = .false. )
      
      do 5 if = 1 , maxnf
         call sload(fname(if),' ') 
 5    continue
c
c     Command line parsing ...
c
      narg = iargc()
      call getarg(0,pgname)
      if( narg .lt. 1 ) then
         go to 999
      else
         call getarg(1,incdnm)
         if( onedot(incdnm) ) then
            CALL GETENV('CODE',INCDNM)
            IF( INCDNM .EQ. ' ' ) THEN
               write(*,*) pgname(1:indlnb(pgname)),
     *              ': CODE environment variable unset.'
               stop
            end if
         end if
      end if

      p4fs = narg .lt. 2
      if( .not. p4fs ) then
         call getarg(2,fs)
         if( onedot(fs) ) then
            fs = '1-*'
         else if( query(fs) ) then
            p4fs = .true.
         end if
      end if

      p4cs1 = narg .lt. 3 
      if( .not. p4cs1 ) then
         call getarg(3,cs1)
         if( onedot(cs1) ) then
            cs1 = '1-*'
         else if( query(fs) ) then
            p4cs1 = .true. 
         end if
      end if

      if( narg .ge. 4 ) then
         name_from_segdat = .false.
         call getarg(4,par_name)
			if( onedot(par_name) ) then
				name_from_segdat = .true. 
			else 
            write(*,*) PGNAME(1:INDLNB(PGNAME))//' will label '//
     *                 'as '//PAR_NAME(1:INDLNB(PAR_NAME))//'.'
			end if
      else 
         name_from_segdat = .true.
      end if

		stride = 1
		if( narg .ge. 5 ) then
			stride = i4arg(5,1)
			if( stride .le. 0 ) then
				stride = 1
			end if
		end if
		if( ltrace ) then
			write(0,*) 'nss: stride = ', stride
		end if
c
c     Open data file ...
c
      CALL MYOPEN(UDATA,INCDNM(1:INDLNB(INCDNM))//'.segdat',
     *            'OLD',1,0,0,RC)
      if( rc .ne. 0 ) then
c
c        Try something else ...
c
         CALL MYOPEN(UDATA,'/d'//INCDNM(1:INDLNB(INCDNM))//'.segdat',
     *               'OLD',1,0,0,RC)
         if( rc .ne. 0 ) then
c
c           One more thing ...
c
            IF( INDEX(INCDNM,'.') .NE. 0 ) THEN
               CALL MYOPEN(UDATA,INCDNM(1:INDLNB(INCDNM)),'OLD',
     *                     1,0,0,rc)
               if( rc .ne. 0 ) then
                  CALL nerr1(PGNAME,'Error opening .segdat file.')
               end if
            else 
               CALL nerr1(PGNAME,'Error opening .segdat file.')
            end if
         else
            write(*,*) pgname(1:indlnb(pgname))//
     *                 ': '//'/d'//INCDNM(1:INDLNB(INCDNM))//
     *                 '.segdat opened.'
         end if
      end if
c
      read(udata) nfcn
      read(udata) ( fname(ifcn) , ifcn = 1 , nfcn )
c
c     Prompt user for functions to be plotted if necessary.
c
      if( p4fs ) then
         call ivramp(iota,1,1,nfcn)
         write(user,1000) ( iota(ifcn) , fname(ifcn) , ifcn = 1 , nfcn )
1000     FORMAT(' The following functions are stored:'//
     *          (3(I3,' : ',A)))
         write(user,*)
         CALL INPTIV(FIV,LFIV,MAXNF,NFCN,' ',USER)
c
c        ... but ignore everything but the first element of selection
c        vector ...
c
         lfiv = 1
      else
         if( irdivn(fiv,lfiv,maxnf,1,nfcn,fs) .lt. -1 ) then
            write(*,*) PGNAME(1:INDLNB(PGNAME))//': '//
     *                  'Error parsing <f iv>: '//
     *                  fs(1:indlnb(fs))//'.'
            go to 999
         end if
      end if
c
c     Get window tags.  
c
      call sload(lfname,' ')
      do 80 if = 1 , lfiv 
         if( name_from_segdat ) then
            write(lfname,8000) fname(fiv(if))
8000        format(a)
         else 
            write(lfname,8000) par_name(1:indlnb(par_name))
         end if

 80   continue
c
      nt = 0
 100  continue
      read(udata,end=400,err=350) ttime,   tnr
         nt = nt + 1
         time(nt) = ttime
         read(udata,end=300,err=300)
         do 200 ifcn = 1 , nfcn
            read(udata,end=300,err=300)
 200     continue
      go to 100
 300  continue
         WRITE(USER,*) 'nss: Unexpected error/end-of-file.'
         nt = nt - 1
      go to 400
 350  continue
         write(user,*) 'nss: Unexpected error/end-of-file.'
      go to 400
 400  continue
      if( nt .le. 0 ) then
         call nerr1('nss','No data in .segdat file.')
      end if
c
c     Prompt user for plotting times if necessary.
c
      if( p4cs1 ) then
         CALL INPTIV(TIV,LTIV,MAX_NVEC,NT,' temporal indices ',USER)
      else
         if( irdivn(tiv,ltiv,max_nvec,1,nt,cs1) .lt. -1 ) then
            write(*,*) PGNAME(1:INDLNB(PGNAME))//': '//
     *                  'Error parsing <t iv>: '//
     *                  cs1(1:indlnb(cs1))//'.'
            go to 999
         end if
      end if
c
c     ... and the PRSKIP, PSSKIP arrays ...
c
      lastiv = 0
      do 555 if = 1 , lfiv 
         prskip(if) = fiv(if) - lastiv - 1
         lastiv = fiv(if)
 555  continue
      psskip = nfcn - fiv(lfiv)

c
c     Do the transmission.
c
c
c
      rewind udata
      read(udata)
      read(udata)
c
      group = 1
      start(1) = 1
      do 575 it = 1 , ltiv
         call uskip(udata,(2 + nfcn) * (tiv(it) - group))
         read(udata) time(it), readnr
         tnr = 1 + (readnr - 1) / stride
c 
c        Check for internal array overflow ...
c
			if( (start(it) + tnr) .gt. max_nevent ) then
				call err2('nss','Internal array overflow: '//
     &                'transmitting partial data')
				nvec = it - 1
            go to 600
			end if

			if( ltrace ) then
				write(0,*) it, readnr, '->', tnr
			end if

         read(udata) ( readx(j) , j = 1 , readnr )
         do 560 if = 1 , lfiv 
            call uskip(udata,prskip(if))
            read(udata) ( ready(j) , j = 1 , readnr )
 560     continue
			call dvinj(readx,x(start(it)),stride,readnr)
			call dvinj(ready,y(start(it)),stride,readnr)
         call uskip(udata,psskip)
         group = tiv(it) + 1
         start(it + 1) = start(it) + tnr
 575  continue

		nvec = ltiv
 600  continue

      rc = vsmxynt(lfname(1:indlnb(lfname)),start,time,nvec,x,y)
      if( rc .lt. 0 ) then
         call err2('nss','Could not transmit data to server.')
      end if

      stop

 999  continue
         WRITE(*,*) 'usage: '//PGNAME(1:INDLNB(PGNAME))//
     *              ' <segdat file> [<fcn index>] [<tiv>] '//
     *              '[<name>] [<stride>]'
      stop
c
      end
