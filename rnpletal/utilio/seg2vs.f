c-----------------------------------------------------------------------
c
c     History: nss
c
c     Sends all time groups of first function stored in segdat file
c     to server using vsmxynt ...
c
c     Modified so that 'modern' straightforward open statement using 
c     supplied file name is used when 'all else fails' ...
c
c     Modified to accept arbitrary # of file names; no check for 
c     duplicates.  Also deprecated 'current code' feature.
c
c----------------------------------------------------------------------- 
c
      implicit         none              
c
      integer          vsmxynt,                 i4arg

c
      integer          indlnb,                  irdivn,
     &                 iargc
c
      character*1024   fs,                      cs1
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
     *                 readnr, tnr, nrout, iarg, nvec
c
c     PAR_NAME overrides FNAME (kludge to avoid window collision 
c     with query by name.
c
      integer        narg,     larg
      character*256  argi,     pgname,     incdnm,
     *               par_name
c
      integer        par_if,   par_it
      logical        name_from_segdat
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
      if( narg .lt. 1 ) go to 999

      do iarg = 1 , narg
         call getarg(iarg,incdnm)

         if( ltrace ) then
            write(0,*) '<'//incdnm(1:indlnb(incdnm))//'>'
         end if

         name_from_segdat = .true.

         stride = 1
c
c        Open data file ...
c
         CALL MYOPEN(UDATA,INCDNM(1:INDLNB(INCDNM))//'.segdat',
     *               'OLD',1,0,0,RC)
         if( rc .ne. 0 ) then
c
c           Try something else ...
c
            call myopen(udata,
     &         '/d'//INCDNM(1:INDLNB(INCDNM))//'.segdat',
     &               'OLD',1,0,0,RC)
            if( rc .ne. 0 ) then
c
c              One more thing ...
c
               IF( INDEX(INCDNM,'.') .NE. 0 ) THEN
                  call myopen(udata,
     &               INCDNM(1:INDLNB(INCDNM)),'OLD',
     &               1,0,0,rc)
               end if
               open(udata,file=incdnm(1:indlnb(incdnm)),
     *              form='unformatted',status='old',iostat=rc)
               if( rc .ne. 0 ) then
                  write(0,*) pgname(1:indlnb(pgname))//
     &               ': Error opening <'//
     &               incdnm(1:indlnb(incdnm))//'>'
                  go to 800
               end if
            else
               write(*,*) pgname(1:indlnb(pgname))//
     *                    ': '//'/d'//INCDNM(1:INDLNB(INCDNM))//
     *                    '.segdat opened.'
            end if
         end if
 
         read(udata) nfcn
         read(udata) ( fname(ifcn) , ifcn = 1 , nfcn )
 
c        Only transmit the first stored function ...
 
         lfiv = 1
         fiv(1) = 1
 
c        Get window tags.  
 
         call sload(lfname,' ')
         if( name_from_segdat ) then
            write(lfname,8000) fname(fiv(1))
8000        format(a)
         else 
            write(lfname,8000) par_name(1:indlnb(par_name))
         end if
c
         nt = 0
 100     continue
         read(udata,end=400,err=350) ttime,   tnr
            nt = nt + 1
            time(nt) = ttime
            read(udata,end=300,err=300)
            do 200 ifcn = 1 , nfcn
               read(udata,end=300,err=300)
 200        continue
         go to 100
 300     continue
            WRITE(USER,*) 'seg2vs: Unexpected error/end-of-file.'
            nt = nt - 1
         go to 400
 350     continue
            write(user,*) 'seg2vs: Unexpected error/end-of-file.'
         go to 400
 400     continue
         if( nt .le. 0 ) then
            call nerr1('seg2vs','No data in .segdat file.')
         end if
c
c        ... and the PRSKIP, PSSKIP arrays ...
c
         lastiv = 0
         do 555 if = 1 , lfiv 
            prskip(if) = fiv(if) - lastiv - 1
            lastiv = fiv(if)
 555     continue
         psskip = nfcn - fiv(lfiv)

c        Do the transmission.

         rewind udata
         read(udata)
         read(udata)
c
         group = 1
         start(1) = 1
         ltiv = nt
         call ivramp(tiv,1,1,nt)
         do 575 it = 1 , ltiv
            call uskip(udata,(2 + nfcn) * (tiv(it) - group))
            read(udata) time(it), readnr
            if( ltrace ) then
               write(0,*) 'seg2vs: Group ',it, '  Time: ', time(it)
            end if
            tnr = 1 + (readnr - 1) / stride
            if( (start(it) + tnr) .gt. max_nevent ) then
               write(0,*) 'seg2vs: Internal array overflow.'//
     &                    ' Transmitting partial data.'
               nvec = it - 1
               go to 600
            end if

            read(udata) ( readx(j) , j = 1 , readnr )
            do 560 if = 1 , lfiv 
               call uskip(udata,prskip(if))
               read(udata) ( ready(j) , j = 1 , readnr )
 560        continue
            call dvinj(readx,x(start(it)),stride,readnr)
            call dvinj(ready,y(start(it)),stride,readnr)
            call uskip(udata,psskip)
            group = tiv(it) + 1
            start(it + 1) = start(it) + tnr
 575     continue

         nvec = ltiv
 600     continue

         rc = vsmxynt(lfname(1:indlnb(lfname)),start,time,nvec,x,y)
         if( rc .lt. 0 ) then
            call nerr2('seg2vs','Could not transmit data to server.')
         end if

 800     continue

      end do


      stop

 999  continue
         WRITE(*,*) 'usage: '//PGNAME(1:INDLNB(PGNAME))//
     *              ' <segdat file> [ <segdat file> ... ]'
      stop

c
      end
