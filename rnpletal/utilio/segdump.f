c-----------------------------------------------------------------------
c
c     History: SEGLIST.
c
c     Program for extracting single snap shot of single function
c     from SEGDAT file.
c
c     Modified so that number of points in segment *not* output.
c
c     Modified for multiple snap shots ... extra column for time ...
c 
c     Modified to take extra optional argument for print format ...
c
c     Modified to use case-sensitive MYOPEN -> MYOPENCS ... 
c
c-----------------------------------------------------------------------
c
      implicit         none
c
      integer          indlnb,        irdivn
c
      character*256    fmt
      character*16     lfname
      integer          lnb
c
      integer          maxnf,          maxnr,           maxnt
      parameter      ( maxnf = 100,    maxnr = 250 000, maxnt = 10 000 )
c
      character*16     fname(maxnf)
c
      real*8           f(maxnr),     r(maxnr),     time(maxnt),
     *                 fout(maxnr),  rout(maxnr),  tout(maxnr)
c
      integer          iota(maxnf),   fiv(maxnf),
     *                 nr(maxnt),     tiv(maxnt)
c
      integer          udata,         ulist,       user,
     *                 compar,        uout
      parameter      ( udata = 11,    ulist = 4,   user = 6,
     *                 compar = 2)    
c
      real*8           ttime
c
      integer          group, if, ifcn, iskip, it, j, lfiv, ltiv,
     *                 nfcn, nt, prskip, psskip, skip, tnr, nrout
c
c->   INCLUDE       '/usr/people/matt/include/io.inc'
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

      integer        narg,     iargc,      larg
      character*256  argi,     pgname,     incdnm,   ctiv,
     *               dvp_fmt
c
      integer        par_if,   par_it
      logical        p4f,      p4t,        dvp_fmt_arg
c
      integer        rc

      logical        ltrace
      parameter    ( ltrace = .false. )
c
c     Command line parsing ...
c
      narg = iargc()
      call getarg(0,pgname)
      if( narg .lt. 1 ) then
         go to 999
      else
         CALL SLOAD(INCDNM,' ')
         call getarg(1,incdnm)
         IF( INCDNM(1:1) .EQ. '.'  .AND.  
     *       indlnb(incdnm) .eq. 1 ) then
            CALL GETENV('CODE',INCDNM)
            IF( INCDNM .EQ. ' ' ) THEN
               WRITE(*,*) '<<< ',PGNAME(1:INDLNB(PGNAME)),
     *              ': CODE environment variable unset. >>>'
               stop
            end if
         end if
         if( narg .gt. 1 ) then
            call getarg(2,argi)
            par_if = s2i4(argi)
            if( par_if .eq. -999 ) go to 999
            p4f = .false.
         else
            p4f = .true.
         end if

         if( narg .gt. 2 ) then
            call getarg(3,ctiv)
            p4t = .false.
         else
            p4t = .true.
         end if

         if( narg .gt. 3 ) then 
            DVP_FMT = SARG(4,' ')
            dvp_fmt_arg = .true.
         else 
            dvp_fmt_arg = .false.
         end if
      end if
c
c     Open data file ...
c
      CALL MYOPENCS(UDATA,INCDNM(1:INDLNB(INCDNM))//'.segdat',
     *            'OLD',1,0,0,RC)
      if( rc .ne. 0 ) goto 900
c
c     Open listing file ... if program invoked with all arguments,
c     file is standard output.
c
      if( p4f .or. p4t ) then
         CALL MYOPENCS(ULIST,INCDNM(1:INDLNB(INCDNM))//'.listing',
     *              'UNKNOWN',0,0,0,RC)
         if( rc .ne. 0 ) then
            WRITE(*,*) '<<< '//PGNAME(1:INDLNB(PGNAME))//
     *                 ': Error opening '//INCDNM(1:INDLNB(INCDNM))//
     *                 '.listing. >>>'
            stop
         end if
         uout = ulist
      else
         uout = 6
      end if
c
c
      read(udata) nfcn
      read(udata) ( fname(ifcn) , ifcn = 1 , nfcn )
c
      nt = 0
 100  continue
      read(udata,end=400,err=350) ttime,   tnr
         nt = nt + 1
         time(nt) = ttime
         nr(nt) = tnr
         read(udata,end=300,err=300)
         do 200 ifcn = 1 , nfcn
            read(udata,end=300,err=300)
 200     continue
      go to 100
 300  continue
         WRITE(USER,*) '<<< Unexpected error/end-of-file. >>>'
         nt = nt - 1
      go to 400
 350  continue
         write(user,*) '<<< Unexpected error/end-of-file. >>>'
      go to 400
 400  continue

      if( p4f ) then
c
c        Prompt user for functions to be listed.
c
         call ivramp(iota,1,1,nfcn)
         write(user,1000) ( iota(ifcn) , fname(ifcn) , ifcn = 1 , nfcn )
1000     FORMAT(' The following functions are stored:'//
     *          (3(I3,' : ',A)))
         write(user,*)
         CALL INPTIV(FIV,LFIV,MAXNF,NFCN,' ',USER)
c
c        ... ignore all but first selction in input range.
c
      else
         fiv(1) = par_if
      end if
      lfiv = 1
   
      if( p4t ) then
c
c        Prompt user for listing time ...
c
         CALL INPTIV(TIV,LTIV,MAXNT,NT,' temporal index ',USER)
c
c        ... ignore all but first selction in input range.
c
      else
         if( irdivn(tiv,ltiv,maxnt,1,nt,ctiv) .lt. -1 ) go to 999
         if( ltrace ) then
            CALL IVDUMP(TIV,LTIV,'temporal index vector',6)
         end if
      end if
c
c     Do the listing.
c
      do 600 if = 1 , lfiv
c
         ifcn = fiv(if)
         prskip = ifcn - 1
         psskip = nfcn - ifcn
c
         rewind udata
         read(udata)
         read(udata)
c
         group = 1
         do 575 it = 1 , ltiv
            call uskip(udata,(2 + nfcn) * (tiv(it) - group))
            read(udata) ttime, tnr
c
c           Should check against internal arrays here ... 
c
            if( tnr .gt. maxnr ) then
               WRITE(0,*) 'segdump: Internal array overflow for nr = ',
     *                    tnr
               call uskip(udata,2+prskip+psskip)
            else 
               read(udata) ( r(j) , j = 1 , tnr )
               call uskip(udata,prskip)
               read(udata) ( f(j) , j = 1 , tnr )
               call uskip(udata,psskip)
c
               if( ltiv .gt. 1 ) then
                  call dvls(tout,ttime,tnr)
                  if( dvp_fmt_arg ) then
                     call dvtwri1f(r,f,tout,tnr,dvp_fmt,uout)
                  else
                     call dvtwri1(r,f,tout,tnr,uout)
                  end if
               else 
                  if( dvp_fmt_arg ) then
                     call dvpwri1f(r,f,tnr,dvp_fmt,uout)
                  else
                     call dvpwri1(r,f,tnr,uout)
                  end if
               end if
            end if
c
            group = tiv(it) + 1
 575     continue
 600  continue
c
      stop
c
 900  continue
         WRITE(0,*) '<<< '//PGNAME(1:INDLNB(PGNAME))//
     *              ': Error opening '//INCDNM(1:INDLNB(INCDNM))//
     *              '.segdat. >>>'
      stop

 999  continue 
         WRITE(*,*) 'usage: '//PGNAME(1:INDLNB(PGNAME))//
     *              ' <segdat tag> [<fcn index> '//
     *              '<time index vec.> <format>].'
      stop

      end
