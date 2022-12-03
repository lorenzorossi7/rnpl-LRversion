c---------------------------------------------------------------------------
c 
c     Miscellaneous "IO" routines. 
c
c---------------------------------------------------------------------------

c---------------------------------------------------------------------------
c     Prompts for user input and possibly exits ...
c---------------------------------------------------------------------------
      subroutine prexit(prompt,exitstr)
         implicit        none
         character*1     l2u

         character*(*)   prompt,     exitstr
         character*1     resp
         integer         ntry,       maxtry,         rc
         parameter     (             maxtry = 3 )
         logical         ltrace
         parameter     ( ltrace = .true. )
         
         ntry = 0
100      continue
            write(*,fmt='(a)') prompt
            read(*,fmt='(a)',iostat=rc) resp
            ntry = ntry + 1
         if( rc .ne. 0  .and.  ntry .lt. maxtry ) go to 100

         if( l2u(resp) .eq. l2u(exitstr(1:1)) )  then
            if( ltrace ) then
               write(0,*) '*** Exiting from prexit'
            end if
            stop
         end if

         return
      end

c---------------------------------------------------------------------------
c
c     Prints message and abends ...
c
c---------------------------------------------------------------------------

      subroutine err1(pgname,string)

         integer          indlnb
         character*(*)    pgname,     string

         write(*,100) pgname(1:indlnb(pgname)), 
     *                string(1:indlnb(string)) 
100      format(' <<< ',a,':: ',a,' >>>')
         stop

      end

c---------------------------------------------------------------------------
c
c     Same as above without <<< >>> delimiters ...
c
c---------------------------------------------------------------------------

      subroutine nerr1(pgname,string)

         integer          indlnb
         character*(*)    pgname,     string

         write(*,100) pgname(1:indlnb(pgname)), 
     *                string(1:indlnb(string)) 
100      format(' ',a,': ',a)
         stop

      end

c---------------------------------------------------------------------------

      subroutine err1on0(pgname,string)

         integer          indlnb
         character*(*)    pgname,     string

         write(0,100) pgname(1:indlnb(pgname)), 
     *                string(1:indlnb(string)) 
100      format(' <<< ',a,':: ',a,' >>>')
         stop

      end

c---------------------------------------------------------------------------
c
c     Prints message and returns ...
c
c---------------------------------------------------------------------------

      subroutine err2(pgname,string)

         integer          indlnb
         character*(*)    pgname,     string

         write(*,100) pgname(1:indlnb(pgname)), 
     *                string(1:indlnb(string)) 
100      format(' <<< ',a,':: ',a,' >>>')
         return

      end

c---------------------------------------------------------------------------
c
c     As above but without '<<<' '>>>' delimiters ...
c
c---------------------------------------------------------------------------

      subroutine nerr2(pgname,string)

         integer          indlnb
         character*(*)    pgname,     string

         write(*,100) pgname(1:indlnb(pgname)), 
     *                string(1:indlnb(string)) 
100      format(a,': ',a)
         return

      end

c---------------------------------------------------------------------------

      subroutine err2on0(pgname,string)

         integer          indlnb
         character*(*)    pgname,     string

         write(0,100) pgname(1:indlnb(pgname)), 
     *                string(1:indlnb(string)) 
100      format(' <<< ',a,':: ',a,' >>>')
         return

      end

c---------------------------------------------------------------------------
c
c     Conditionally prints string ...
c
c---------------------------------------------------------------------------

      subroutine trace_it(logic,string)

         implicit         none

         integer          indlnb

         logical          logic
         character*(*)    string

         if( logic ) then
            write(*,100) string(1:indlnb(string)) 
100         format(' <<< ',a,' >>>')
         end if

         return

      end


c---------------------------------------------------------------------------
c
c     Basic "scanning" routine for .segdat's. Returns names of functions
c     stored and number of groups stored.
c
c---------------------------------------------------------------------------

      subroutine segscn(u,fname,nfcn,nt)

         integer        u
         character*16   fname(1)
         integer        nfcn, nt

         integer        ifcn

         logical        ltrace
         parameter    ( ltrace = .false. )

         read(u,end=999,err=999) nfcn
         read(u,end=999,err=999) ( fname(ifcn) , ifcn = 1 , nfcn )

         nt = 0
 100     continue
            read(u,end=200,err=999) 
            nt = nt + 1
            read(u,end=150,err=999)
            do 125 ifcn = 1 , nfcn
               read(u,end=150,err=999)
 125        continue
         go to 100
 150        write(*,*) '<<< segscn:: Unexpected end--of--file. >>>'
            nt = nt - 1
 200     continue
         if( ltrace ) then
            write(*,2000) nt    
2000        format(' <<< segscn:: ',i5,' groups in segdat file. >>>')
         end if
c
c        Rewind and reposition ... 
c
         rewind u
         read(u,end=999,err=999)
         read(u,end=999,err=999)
      
         return

 999        call err1('segscn','Error reading .segdat file.')  

      end 

C-----------------------------------------------------------------------
C
C     Does logarithmic transformation ...
C
C-----------------------------------------------------------------------

      subroutine dvlntr(x,y,fstar,yn,n)

         integer        n
         real*8         x(n),     y(n)
         real*8         fstar,    yn

         real*8         alpha,    x1xsm1,    xstar
         integer        j

         xstar = x(1) - fstar 
         x1xsm1 = 1.0d0 / (x(1) - xstar)
         alpha = yn / log(x1xsm1 * (x(n) - xstar))
      
         do 10 j = 1 , n
            y(j) = alpha * log(x1xsm1 * (x(j) - xstar))
 10      continue

         return

      end

C-----------------------------------------------------------------------
C
C     Another routine as above ...
C
C-----------------------------------------------------------------------

      subroutine dvlnt1(x,y,rin,rout,fstar,x1,xn,yn,nin,nout)

         integer        nin,      nout
         real*8         x(1),     y(1),   
     *                  rin(1),   rout(1)
         real*8         fstar,    x1,
     *                  xn,       yn

         real*8         alpha,    x1xsm1,    xstar
         integer        j

         xstar = x1 - fstar
         x1xsm1 = 1.0d0 / (x1 - xstar)
         alpha = yn / log(x1xsm1 * (xn - xstar))

         nout = 0
      
         do 10 j = 1 , nin
            if( x(j) .ge. x1  .and.  x(j) .le. xn ) then
               nout = nout + 1
               y(nout) = alpha * log(x1xsm1 * (x(j) - xstar))
               rout(nout) = rin(j)
            end if
 10      continue

         return

      end

c-----------------------------------------------------------------------
c
c     Scans groups in segdat file as selected by TIV. Returns minimum 
c     and maximum of IXF'th function.
c
c-----------------------------------------------------------------------

      subroutine ssflim(u,ixf,tiv,ltiv,fmin,fmax)

         real*8       l_dvmin,  l_dvmax

         integer      u,      ixf,    ltiv
         integer      tiv(ltiv)
         real*8       fmin,   fmax

         integer      maxnr 
         parameter  ( maxnr = 100 000 )
         real*8       f(maxnr)

         integer      group,  it,     tnr,     nfcn,     j
         real*8       ttime
         logical      first

         rewind u 
         read(u) nfcn
         read(u)

         first = .true.
         group = 1
         do 500 it = 1 , ltiv 
            call uskip(u,(2 + nfcn) * (tiv(it) - group))
            read(u) ttime, tnr
            read(u)
            if( tnr .gt. maxnr ) then
               call err1('ssflim','f array overflow')
            end if
            call uskip(u,ixf-1)
            read(u) ( f(j) , j = 1 , tnr )
            if( first ) then
               fmin = l_dvmin(f,tnr)
               fmax = l_dvmax(f,tnr)
               first = .false.
            else
               fmin = min(fmin,l_dvmin(f,tnr))
               fmax = max(fmax,l_dvmax(f,tnr))
            end if
            call uskip(u,nfcn-ixf)
            group = tiv(it) + 1
 500     continue
c
c        Rewind and reposition ...
c
         rewind u
         read(u)
         read(u)

         return

      end

c------------------------------------------------------------------------
c
c     Returns 1 if file exists, 0 if not.
c
c------------------------------------------------------------------------
 
      integer function pfilex(fname,type)

         integer        indlnb

         character*(*)  fname

         integer        u,       rc,      type
         parameter    ( u = 20 )

         call myopen(u,fname(1:indlnb(fname)),'OLD',type,0,0,rc)
         if( rc .eq. 0 ) then
            pfilex = 1 
            close(u)
         else
            pfilex = 0
         end if

         return 

      end

c-------------------------------------------------------------------------
c
c     Returns first unit number .ge. umin not attached to a file.
c
c-------------------------------------------------------------------------

      integer function getu()

         implicit      none

         integer       umin
         parameter   ( umin = 10 )

         integer       umax
         parameter   ( umax = 99 )

         integer       u
         logical       opened

         getu = -1
         do 10 u = umin , umax
            inquire(unit=u,opened=opened)
            if( .not. opened ) then
               getu = u
               return
            end if       
 10      continue
         call err1('getu','Panic--no available unit number')

         return
      end

c-------------------------------------------------------------------
c
c     Deletes <fname> ... so user beware ...
c     Returns > 0 success, number of tries it took to delete file,
c     -1 if file doesn't exist.
c
c-------------------------------------------------------------------

      integer function delete(fname)     

      implicit      none

      character*(*) fname

      integer       indlnb, getu
      integer       u

      character*128 access, form
      logical       exist

      inquire(file=fname(1:indlnb(fname)),exist=exist)
      if( exist ) then
         inquire(file=fname(1:indlnb(fname)),
     *           access=access,
     *           form=form)
         delete = 0
 200     continue
            u = getu( )
            open(unit=u,file=fname(1:indlnb(fname)),
     *           access=access,
     *           form=form)
              close(u,status='delete')
            delete = delete + 1
            inquire(file=fname(1:indlnb(fname)),exist=exist)
         if( exist ) go to 200
      else
         delete = -1
      end if

      return
      end
c
c-------------------------------------------------------------------
c
c     Positions, if possible .segdat file (attached to u) at group
c     with time t (+/- t * dt). returns groupd index if found, 
c     -1 otherwise.
c
c-------------------------------------------------------------------

      subroutine segp2t(u,nfcn,t,dt,nr,rc)

         implicit       none

         integer        u,        nfcn
         real*8         t,        dt
         integer        nr,       rc

         real*8         epst,     lt
         integer        i,        lnfcn

         rewind u
         read(u) lnfcn
         if( lnfcn .ne. nfcn ) then
            call err2('seg2pt','Mismatch between arg and local nfcn')
            rc = -10
            return
         end if

         epst = dt * t
         read(u)
         rc = 0
 100     continue
            read(u,end=200) lt, nr
            rc = rc + 1
            if( abs(t - lt) .le. epst ) return
            do 150 i = 1 , nfcn + 1
               read(u,end=200) 
 150        continue
         go to 100
 
 200     continue   
         rc = -1
         return

      end

c-------------------------------------------------------------------------------
c
c     Complete .segdat scanning routine ...
c
c     Modified to take length of string <file_name> as arg. to facilitate
c     calls from C on various archs. (RS6000 in particular)
c
c-------------------------------------------------------------------------------

      subroutine segscan(in_file_name,in_file_name_len,
     *                   fname,nfcn,time,nr,nt,
     *                   r1mn,r1mx,rnrmn,rnrmx,rc)

         implicit       none

         integer        indlnb

         integer        maxnr
         parameter    ( maxnr = 50 000 )
         real*8         r(maxnr)

         integer        u
         parameter    ( u = 80 )

         character*(*)  in_file_name 
         integer        in_file_name_len
         integer        nfcn

         character*256  file_name

         character*16   fname(1)
         real*8         time(1)
         integer        nr(1)
         integer        nt,      ngroup
         real*8         r1mn,    r1mx,    rnrmn,    rnrmx
         integer        rc
   
         integer        j,       jj,      lnr
   
         logical        ltrace
         parameter    ( ltrace = .false. )

         call sload(file_name,' ')
         file_name = in_file_name(1:in_file_name_len)
         if( ltrace ) then
            write(*,1000) file_name(1:indlnb(file_name))
1000        format(' >>> segscan: file_name: <',a,'>')
         end if
         open(u,file=file_name(1:indlnb(file_name)),
     *        form='unformatted',status='old',err=990)

         if( ltrace ) then
            write(*,*) '>>> segscan:: file opened ...'
         end if

         read(u,end=900,err=900) nfcn
         if( ltrace ) then
            write(*,*) '>>> segscan:: ',nfcn,' functions stored.'
         end if
         read(u,end=900,err=900) ( fname(j) , j = 1 , nfcn )
         do 50 jj = 1 , nfcn
            fname(j) = fname(j)
 50      continue 

         nt = 0
 100     continue 
            read(u,end=300,err=900) time(nt+1), nr(nt+1)
            lnr = nr(nt+1) 
            read(u,end=200) ( r(j) , j = 1 , lnr )
            if( nt .eq. 0 ) then
               r1mn = r(1)
               r1mx = r(1)
               rnrmn = r(lnr)
               rnrmx = r(lnr)
            else
               r1mn = min(r1mn,r(1))
               r1mx = max(r1mx,r(1))
               rnrmn = min(rnrmn,r(lnr))
               rnrmx = max(rnrmx,r(lnr))
            end if  
            do 150 jj = 1 , nfcn
               read(u,end=200)
 150        continue
            nt = nt + 1
            if( ltrace ) then
               write(*,*) '>>> segscan:: group ',nt,' read ...'
            end if
         go to 100
 200     continue
            call err2('segscan','Unexpected end--of--file.')
 300     continue
         rc = 0
c->      rewind u
         close(u)
         return

 900     rc = -1
c->      rewind u
         close(u)
         return

 990     rc = -1
         return

         end
c
c        Copies <seg1> .segdat file to <seg2> but uses supplied 
c        time stamps.
c
         subroutine segrlt(seg1,seg2,time2,ng,rc)

            implicit       none

            character*(*)  seg1,      seg2
            integer        ng
            real*8         time2(ng)
            integer        rc

            integer        MAXF
            parameter    ( MAXF = 100 )
            character*16   fname(MAXF)

            integer        MAXNR
            parameter    ( MAXNR = 50000 )
            real*8         v(MAXNR)

            integer        nfcn,            nr

            real*8         otime
            integer        ifcn,            j,               
     *                     lng

            integer        u1,              u2
            parameter    ( u1 = 20,         u2 = 21 )

            open(u1,file=seg1,status='old',form='unformatted',
     *           err=990)

            read(u1,end=991,err=991) nfcn
            if( nfcn .gt. MAXF ) then
               call err1('segrlt','.segdat file contains too '//
     *                   'many functions')
            end if

            open(u2,file=seg2,status='unknown',form='unformatted',
     *           err=990)
            write(u2) nfcn
            read(u1,end=991,err=991)  
     *         ( fname(ifcn) , ifcn = 1 , nfcn )
            write(u2) ( fname(ifcn) , ifcn = 1 , nfcn )

            lng = 0
 100        continue
               read(u1,end=200,err=991) otime, nr
               lng = lng + 1
               if( lng .gt. ng ) then
                  call err2('segrlt','too few time stamps supplied')
                  go to 999
               end if
               write(u2) time2(lng), nr
               if( nr .gt. MAXNR ) then
                  call err2('segrlt','segment overflow.')
                  go to 999
               end if
               read(u1,end=991,err=991)  ( v(j) , j = 1 , nr )
               write(u2) ( v(j) , j = 1 , nr )
               do 50 ifcn = 1 , nfcn
                  read(u1,end=991,err=991) ( v(j) , j = 1 , nr )
                  write(u2) ( v(j) , j = 1 , nr )
  50           continue
            go to 100

 200        continue
            if( lng .ne. ng ) then 
               call err2('segrlt','warning: too many time '//
     *                   'stamps supplied')
               go to 999
            else
               rc = 0
            end if
            return

 990        continue
            call err1('segrlt','error opening source'//
     *                '/target .segdat file.')
            
 991        continue
            call err1('segrlt','error reading source'//
     *                ' .segdat file.')

 999        continue
            rc = -1
            return
            
         end 
c
c     Complete .segdat scanning routine ...
c
      integer function segfn(file_name,ifcn,it,
     *                       t,r,f,nr)

         implicit       none

         integer        indlnb

         character*(*)  file_name 
         integer        ifcn,      it,      nr
         real*8         t
         real*8         f(1),      r(1)
         
         integer        u
         parameter    ( u = 80 )

         integer        nfcn,      j

         logical        ltrace
         parameter    ( ltrace = .false. )

         open(u,file=file_name(1:indlnb(file_name)),
     *        form='unformatted',status='old',err=990)

         if( ltrace ) then
            write(*,*) '>>> segfn:: file opened ...'
         end if

         read(u,end=900,err=900) nfcn
         if( ltrace ) then
            write(*,*) '>>> segfn:: ',nfcn,' functions stored.'
         end if
         if( nfcn .lt. ifcn ) then
            segfn = -5
            return
         end if

         do 100 j = 1 , 1 + (nfcn + 2) * (it - 1) 
            read(u,end=900,err=900)
 100     continue 
         read(u,end=900,err=900) t, nr
         do 150 j = 1 , ifcn - 1
            read(u,end=900,err=900)
 150     continue
         read(u,end=900,err=900) ( r(j) , j = 1 , nr )
         read(u,end=900,err=900) ( f(j) , j = 1 , nr )

         segfn = nr
         rewind u
         return

 900     segfn = -2
         rewind u
         return

 990     segfn = -1
         return

         end

c-----------------------------------------------------------------------
c
c     Returns ig'th group of if'th function in segdat file <tag> 
c     (full name including extension) ...
c
c-----------------------------------------------------------------------

      integer function segpick0(tag, if,ig, x,f,nx)

         implicit         none

         integer          skipu
   
         integer          max_fcn
         parameter      ( max_fcn = 1000 )
         character*16     fname(max_fcn)

         character*(*)    tag
         integer          if,       ig
         real*8           x(*),     f(*)
         integer          nx

         integer          usegdat 
         parameter      ( usegdat = 10 )

         real*8           time
         integer          rc,       j,       nfcn,       group

         call myopen(usegdat,tag,'old',1,0,0,rc)
         if( rc .ne. 0 ) then
            segpick0 = -1
            return
         end if
         
         read(usegdat,end=900,err=900) nfcn
         read(usegdat,end=900,err=900) ( fname(j) , j = 1 , nfcn )
         if( 1 .gt. if  .or.  if .gt. nfcn ) then
            segpick0 = -2
            return
         end if

         if( skipu(usegdat, (ig - 1) * (nfcn + 2)) .ne. 0 ) then
            segpick0 = -3
            return
         end if

         read(usegdat,end=900,err=900) time,  nx
         read(usegdat,end=900,err=900) ( x(j) , j = 1 , nx )
         if( skipu(usegdat,if-1) .ne. 0 ) then
            segpick0 = -3
            return
         end if
         read(usegdat,end=900,err=900) ( f(j) , j = 1 , nx )

         close(usegdat)

         segpick0 = nx

         return

900      continue
            segpick0 = -3
         return

      end

c-----------------------------------------------------------------------
c
c     Skips n records on u, returns 0 for success, -1 otherwise.
c
c-----------------------------------------------------------------------

      integer function skipu(u,n)
   
         implicit      none

         integer       u,      n

         integer       i

         skipu = 0
         do 10 i = 1 , n
            read(u,end=90) skipu
 10      continue 
         return

 90      continue
            skipu = -1
         return

      end

c-----------------------------------------------------------------------
c
c     Is integer key in index vector defined in file <fname>?
c
c-----------------------------------------------------------------------

      logical function isinivf(fname,key)

         implicit         none

         integer          posivf

         character*(*)    fname
         integer          key

         isinivf = posivf(fname,key) .gt. 0

         return
      
      end 

c-----------------------------------------------------------------------
c
c     Integer key position in index vector defined in file <fname>.
c
c-----------------------------------------------------------------------

      integer function posivf(fname,key)

         implicit         none

         integer          l_ivindx,      indlnb,
     *                    irdivn,        getu

         logical          ltrace
         parameter      ( ltrace = .false. )

         character*(*)    fname
         integer          key
c
c        Internal Data Structures ...
c
         integer          max_niv
         parameter      ( max_niv = 10 )

         integer          max_liv
         parameter      ( max_liv = 10 000 )

         character*1024   ivfname(max_niv)
         integer          iv(max_liv,max_niv),  
     *                    liv(max_liv)

         integer          ncall,         niv
c
c        Default index vector limits ...
c
         integer          inf,           sup
         parameter      ( inf = 0,       sup = 1 000 000 )
c
c        Locals ...
c
         integer          i,             u,          rc,
     *                    whichiv
         character*1024   lfname,        cline
c
c        Initialization, state-saving ...
c
         data             ncall / 0 /,   niv / 0 /
         save             ivfname,       iv,         liv,
     *                    ncall,         niv

         posivf = 0
c
c        First invocation ...
c
         if( ncall .eq. 0 ) then
            do i = 1 , max_niv
               call sload(ivfname(i),' ')
            end do
         end if
         ncall = ncall + 1

         whichiv = 0
         do i = 1 , niv
            if( ivfname(i) .eq. fname ) then
               whichiv = i
         go to 100
            end if
         end do
100      continue
         if( whichiv .eq. 0 ) then
            lfname = fname
            u = getu()
            open(u,file=lfname(1:indlnb(lfname)),status='old',err=900)
            niv = niv + 1
            ivfname(niv) = lfname(1:indlnb(lfname))
            call mess1(ltrace,'posivf',
     *                 'Opened lfname(1:indlnb(lfname)).')
            read(u,fmt='(a)',end=910,err=910) cline
            close(u)

            whichiv = niv
            rc = irdivn(iv(1,whichiv),liv(whichiv),max_liv,
     *                  inf,sup,cline)
            if( ltrace ) then
               call ivdump(iv(1,whichiv),liv(whichiv),'posivf: '//
     *                     lfname(1:indlnb(lfname)),0)
            end if

         end if

         posivf = l_ivindx(iv(1,whichiv),liv(whichiv),key)
         
         return

900      continue
            call mess1(ltrace,'posivf','Error opening '//
     *                 lfname(1:indlnb(lfname)))
            posivf = -1
         return

910      continue
            call mess1(ltrace,'posivf','Error reading '//
     *                 lfname(1:indlnb(lfname)))
            posivf = -2
         return

      end 

c-----------------------------------------------------------------------
c
c     Tracing routine ...
c
c-----------------------------------------------------------------------

      subroutine mess1(enabled,s1,s2)

         implicit      none

         logical       enabled

         character*(*) s1,       s2

         integer       unit
         parameter   ( unit = 0 )
         
         if( enabled ) then
            write(unit,1000) s1, s2
1000        format(a,': ',a)
         end if

         return

      end 


c-----------------------------------------------------------------------
c
c     Tests to see whether a file exists ... presumably can also
c     be accomplished via INQUIRE ...
c 
c-----------------------------------------------------------------------

      logical function file_exists(fname,form)

         implicit        none
      
         character*(*)   fname
         character*(*)   form

         integer         getu
         integer         u

         u = getu()
         open(u,file=fname,form=form,status='old',err=900)
         close(u)
         file_exists = .true.
         return

900      continue
         file_exists = .false.
         return

      end

c-----------------------------------------------------------------------
c
c     Converts real*8 value to string of 16 digits, padded to left if
c     necessary with 0's ...
c 
c-----------------------------------------------------------------------

      character*16 function r8tostring(x)

         implicit      none

         real*8        x
         character*64  buffer
         
         character*10  digits
         parameter   ( digits = '0123456789' )

         integer       bcurs,       xcurs,      xxcurs

         call sload(buffer,' ')
         write(buffer,1000) x
1000     format(1p,e25.17)
         xcurs = 1
         do bcurs = 1 , len(buffer)
            if( index(digits,buffer(bcurs:bcurs)) .gt. 0 ) then
               r8tostring(xcurs:xcurs) = buffer(bcurs:bcurs)
               xcurs = xcurs + 1
               if( xcurs .gt. 16 ) return
            end if
         end do
         do xxcurs = xcurs , 16
            r8tostring(xxcurs:xxcurs) = '0'
         end do

         return    

      end

C---------------------------------------------------------------------
c     Local versions of other library routines to make library 
c     standalone.
C---------------------------------------------------------------------
C
C     Vector minimum.
C
C---------------------------------------------------------------------
C
      DOUBLE PRECISION FUNCTION L_DVMIN(V1,N)
C
         REAL*8      V1(1)
         INTEGER     I, N
C
         L_DVMIN = V1(1)
         DO 10 I = 2 , N
            L_DVMIN = MIN(L_DVMIN,V1(I))
 10      CONTINUE
C
         RETURN
C
      END
C
C---------------------------------------------------------------------
C
C     Vector maximum.
C
C---------------------------------------------------------------------
C
      DOUBLE PRECISION FUNCTION L_DVMAX(V1,N)
C
         REAL*8      V1(1)
         INTEGER     I, N
C
         L_DVMAX = V1(1)
         DO 10 I = 2 , N
            L_DVMAX = MAX(L_DVMAX,V1(I))
 10      CONTINUE
C
         RETURN
C
      END
C
C---------------------------------------------------------------------
C
C     Returns index of first occurence of KEY in first N elements of
C     V, or 0 if not found.
C
C---------------------------------------------------------------------
C
      INTEGER FUNCTION L_IVINDX(V,N,KEY)
C
         INTEGER     V(1)
         INTEGER     I, KEY, N
C
         L_IVINDX = 0
         IF( N .GT. 0 ) THEN
            DO 10 I = 1 , N
               IF( V(I) .EQ. KEY ) GO TO 20
 10         CONTINUE
         END IF
C
         RETURN
C
 20      L_IVINDX = I
C
         RETURN
C
      END
