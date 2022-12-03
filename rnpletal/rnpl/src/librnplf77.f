c-----------------------------------------------------------------------
c     Fortran support routines for RNPL.
c 
c     Copyright 1995:  Matthew W Choptuik
c     Author:          Matthew W Choptuik
c-----------------------------------------------------------------------

c=======================================================================
c     Routines from ~matt/utilio/ctoirang.f
c=======================================================================

c-------------------------------------------------------------------------------
c
c     Checks a group (ST-FIN/INC) for semantic validity, performing minor
c     adjustments if necessary.
c
c-------------------------------------------------------------------------------
 
      integer function chkgrp(st,fn,inc,rmin,rmax)
 
         integer       inc, fn, rmax, rmin, st
 
         chkgrp = 0
         if( st .eq. fn  .and.  inc .ne. 1 ) then
            inc = 1
            chkgrp = -1
         end if
         if( inc .gt. abs(fn-st) ) then
            fn = st
            chkgrp = -1
         end if
         if( st .gt. fn  .and.  inc .gt. 0 ) then
            inc = -inc
         end if
         if( st .lt. rmin  .or.  st .gt. rmax  .or.
     *       fn .lt. rmin  .or.  fn .gt. rmax  .or.
     *       inc .eq. 0 ) then
            chkgrp = -2
         end if
 
         return
 
      end
c-------------------------------------------------------------------------------
c
c     "Loads" character variable with single character.
c
c-------------------------------------------------------------------------------
 
      subroutine cload(cvar,lchar)
 
         character*(*)   cvar
         character*1     lchar
         integer         ic
 
         do 10 ic = 1 , len(cvar)
            cvar(ic:ic) = lchar
 10      continue
 
         return
 
      end
c-------------------------------------------------------------------------------
c
c     Converts character representation of signed integer in CREP to integer.
c     Representation assumed left-justified, blanks ignored, but no error
c     checking.
c
c-------------------------------------------------------------------------------
 
      integer function ctoi(crep)
 
         character*(*)    crep
 
         integer          index, len
 
         character*10     nums 
         character*1      blank, misign, plsign, char
         integer          curs, cursst, plmin
         
         DATA             NUMS / '0123456789' /
     *                    BLANK / ' ' /, MISIGN / '-' /,
     *                    PLSIGN / '+' /
 
         ctoi = 0
         cursst = 1
         plmin = 1
         if( crep(1:1) .eq. plsign ) then
            cursst = 2
         else
            if( crep(1:1) .eq. misign ) then
               cursst = 2
               plmin = -1
            end if
         end if
         do 10 curs = cursst , len(crep)
            char = crep(curs:curs)
            if( char .ne. blank ) then
               ctoi = 10 * ctoi + index(nums,char) - 1
            end if
 10      continue
         ctoi = plmin * ctoi
 
         return
 
      end
c-------------------------------------------------------------------------------
c
c     Returns token.
c
c-------------------------------------------------------------------------------
 
      subroutine gettok(s,curs,tok,toklen,catcod)
 
         integer         index, len, skblnk
 
         character*(*)   tok, s
         integer         catcod, curs, toklen
 
         character*14    catch, tokspn
         integer         ncat, catst(5), catfn(5) 
         character*1     char
         integer         icat

         DATA            CATCH / '0123456789,-/*' /,
     *                   ncat / 5 /,
     *                   catst /  1, 11, 12, 13, 14 /,
     *                   catfn / 10, 11, 12, 13, 14 /
 
         curs = skblnk(s,curs)
         if( curs .le. len(s) ) then
c
c           Determine category type.
c
            do 10 icat = 1 , ncat
               if( index(catch(catst(icat):catfn(icat)),
     *                   s(curs:curs)) .ne. 0 ) go to 20
 10         continue
c
c           Invalid character encountered.
c
            catcod = -1
            return
c
c           Accumulate token.
c
 20         continue
            catcod = icat
            tokspn = catch(catst(catcod):catfn(catcod))
            toklen = 0
 30         continue
               char = s(curs:curs)
               if( index(tokspn,char) .eq. 0 ) go to 40
                  toklen = toklen + 1
                  tok(toklen:toklen) = char
                  curs = curs + 1
                  curs = skblnk(s,curs)
            if( curs .le. len(s) ) go to 30
 40         continue
c
         else
c
            catcod = 0
            return
c
         end if
c
      end
c-------------------------------------------------------------------------------
c
c     Looks for integer group of form ST-FIN/INC starting at CURSth position
c     of S.
c
c-------------------------------------------------------------------------------
c
      subroutine ggroup(s,curs,st,fin,inc,found)
c
         integer         ctoi
c
         character*(*)   s
         integer         curs, inc, st, fin
         logical         found
c
         character*80    tok
         integer         catcod, ocurs, toklen
         logical         frange
c
         found = .true.
         ocurs = curs
         call grange(s,curs,st,fin,frange)
         if( frange ) then
            ocurs = curs
            call gettok(s,curs,tok,toklen,catcod)
            if( catcod .eq. 0 ) then
               inc = 1
            else if( catcod .eq. 2 ) then
               curs = ocurs
               inc = 1
            else if( catcod .eq. 4 ) then
               ocurs = curs
               call gettok(s,curs,tok,toklen,catcod)
               if( catcod .ne. 1 ) then
                  found = .false.
               else
                  inc = ctoi(tok(1:toklen))
               end if
            else
               found = .false.
            end if
         else
            found = .false.
         end if
         if( .not. found ) then
            curs = ocurs
         end if
c
         return
c
      end
c-------------------------------------------------------------------------------
c
c     Looks for integer range of form ST-FIN starting at CURSth position
c     of S.
c
c-------------------------------------------------------------------------------
c
      subroutine grange(s,curs,st,fin,found)
c
         integer         ctoi
c
         character*(*)   s
         integer         curs, st, fin
         logical         found
c
         character*80    tok
         integer         catcod, ocurs, toklen
c
         found = .true.
         ocurs = curs
         call gettok(s,curs,tok,toklen,catcod)
         if( catcod .ne. 1  .and.  catcod .ne. 5 ) then
            found = .false.
         else
            if( catcod .eq. 1 ) then
               st = ctoi(tok(1:toklen))
            else
               st = -999 999
            end if
            ocurs = curs
            call gettok(s,curs,tok,toklen,catcod)
            if( catcod .eq. 0 ) then
               fin = st
            else if ( catcod .eq. 3 ) then
               ocurs = curs
               call gettok(s,curs,tok,toklen,catcod)
               if( catcod .eq. 1 ) then
                  fin = ctoi(tok(1:toklen))
               else if ( catcod .eq. 5 ) then
                  fin = -999 999
               else
                  found = .false.
               end if
            else if ( catcod .eq. 2 ) then
               fin = st
               curs = ocurs
            else
               found = .false.
            end if
         end if
         if( .not. found ) then
            curs = ocurs
         end if
c
         return
c
      end
c
c-----------------------------------------------------------------------
c
c     Standard routine for getting user to input group specifications.
c     Modification 060489: if PRUNIT < 0, "read" string from PRSTR,
c     no way of error checking right now.
c
c-----------------------------------------------------------------------
c
      subroutine inptiv(iv,liv,mxliv,sup,prstr,prunit)
c
         character*80    itoc
         integer         ixspan, makind
c
         character*(*)   prstr
         integer         iv(1)
         integer         liv, mxliv, prunit, sup
c
         integer         rdunit
c
         character*80    cline, csup
         integer         inc(100), fn(100), st(100),
     *                   curs, ng, idummy
         logical         found
c
         csup = itoc(sup)
c
         if( prunit .gt. 0 ) then
            if( prunit .eq. 6  .or.  prunit .eq. 0 ) then
               rdunit = 5
            else
               rdunit = prunit
            end if
 100        continue
               WRITE(PRUNIT,1020) PRSTR, CSUP(1:IXSPAN(CSUP,' '))
1020           FORMAT(' Select',A,'from 1 to ',A,': ')
c
c              Aug 4 1988, Had to change FORMAT statement from
c              simply A, was generating end-of-record on FILE
c              FT06F001 ...
c
c              Mar 18 1989, Still a problem, trying A40.
c
C              Modified: Sep 5 1990: eof == ' '
c
               read(rdunit,1040,end=110) cline
1040           format(a80)
               go to 120
 110           continue
                  CLINE = ' '
 120           continue
               IF( CLINE .EQ. ' ') THEN
                  call livrmp(iv,1,1,sup)
                  liv = sup
               else
                  curs = 1
                  call psexpr(cline,curs,st,fn,inc,ng,found)
                  if( .not. found ) go to 100
                  if( makind(iv,liv,mxliv,st,fn,inc,ng,1,sup)
     *                .lt. -1 ) go to 100
               end if
            else
               cline = prstr
               IF( CLINE .EQ. ' ' ) THEN
                  call livrmp(iv,1,1,sup)
                  liv = sup
               else
                  curs = 1
                  call psexpr(cline,curs,st,fn,inc,ng,found)
                  if( .not. found ) then
                     WRITE(*,*) '<<< INPTIV: Could not parse',
     *                  ' index vector rep. passed in PRSTR. >>>'
                     return
                  end if
                  idummy = makind(iv,liv,mxliv,st,fn,inc,ng,1,sup)
               end if
            end if
c
         return

900      continue 
         stop
c
      end
c-------------------------------------------------------------------------------
c
c     Converts integer to character representation.
c
c-------------------------------------------------------------------------------
c
      character*(*) function itoc(irep)
c
         character*10   nums 
c
         integer        curs, dig, irep, k, mag, lcirep, lower,
     *                  upper

         DATA           NUMS / '0123456789' /
c
         ITOC = ' '
         lcirep = irep
         if( lcirep .eq. 0 ) then
            ITOC(1:1) = '0'
            return
         else if( lcirep .lt. 0 ) then
            ITOC(1:1) = '-'
            lcirep = -lcirep
            curs = 2
         else
            curs = 1
         end if
         lower = 1
         do 10 mag = 1 , 10
            upper = 10 * lower
            if( lcirep .ge. lower  .and.
     *          lcirep .lt. upper ) go to 20
            lower = upper
 10      continue
 20      continue
         do 30 k = mag , 1 , -1
            dig = lcirep / (10 ** (k - 1))
            itoc(curs:curs) = nums(dig + 1:dig + 1)
            lcirep = lcirep - dig * (10 ** (k - 1))
            curs = curs + 1
 30      continue
c
         return
c
      end
c-------------------------------------------------------------------------------
c
c     Returns index of character to the left of first occurence of CHAR
c     in S.
c
c-------------------------------------------------------------------------------
c
      integer function ixspan(s,char)
c
         character*(*)   s
         character*1     char
c
         do 10 ixspan = 0 , len(s) - 1
            if( s(ixspan+1:ixspan+1) .eq. char ) return
 10      continue
         ixspan = len(s)
c
         return
c
      end
c-------------------------------------------------------------------------------
c
c     Produces index vector from list of groups provided no serious semantic
c     errors exist and INDVEC is sufficiently roomy. MINCOD is the minimum
c     severity level returned by CHKGRD which will allow processing to
c     continue.
c
c-------------------------------------------------------------------------------
c
      integer function makind(indvec,lvec,maxvec,st,fn,inc,ng,rmin,rmax)
c
         integer       chkgrp
c
         integer       lvec, maxvec, rmax, rmin, ng
         integer       indvec(maxvec), inc(ng), fn(ng), st(ng)
         integer       i, ig, mincod 

         data          mincod / -1 /
c
         makind = 0
         do 10 ig = 1 , ng
            if( st(ig) .eq. -999 999 ) then
               st(ig) = rmin
            end if
            if( fn(ig) .eq. -999 999 ) then
               fn(ig) = rmax
            end if
            makind = min(makind,chkgrp(st(ig),fn(ig),inc(ig),rmin,rmax))
 10      continue
         if( makind .ge. mincod ) then
            lvec = 0
            do 30 ig = 1 , ng
               do 20 i = st(ig) , fn(ig) , inc(ig)
                  lvec = lvec + 1
                  if( lvec .gt. maxvec ) then
                     makind = -5
                     return
                  end if
                  indvec(lvec) = i
 20            continue
 30         continue
         end if
c
         return
c
      end
c-------------------------------------------------------------------------------
c
c     Parses expression consisting of one or more groups, returning STart,
c     FINish and INCrement arrays.
c
c-------------------------------------------------------------------------------
c
      subroutine psexpr(s,curs,st,fin,inc,ngroup,sucess)
c
         character*(*)   s
         integer         inc(1), fin(1), st(1), curs, ngroup
         logical         sucess
c
         character*80    tok
         integer         catcod, ocurs, toklen
         logical         gfound
c
         ngroup = 0
 10      ocurs = curs
         call ggroup(s,curs,st(ngroup+1),fin(ngroup+1),
     *               inc(ngroup+1),gfound)
         if( gfound ) then
            ngroup = ngroup + 1
            ocurs = curs
            call gettok(s,curs,tok,toklen,catcod)
            if( catcod .eq. 0 ) then
               sucess = .true.
            else if( catcod .eq. 2 ) then
               go to 10
            else
               sucess = .false.
            end if
         else
            sucess = .false.
         end if
         if( .not. sucess ) then
            curs = ocurs
         end if
c
         return
c
      end

c-------------------------------------------------------------------------------
c
c     Same as INPTIV except command line is to be read (non-interactively)
c     from RDUNIT.
c
c     Modified so that null line == blank line.
c
c-------------------------------------------------------------------------------
c
      integer function readiv(iv,liv,mxliv,inf,sup,rdunit)
c
         integer         makind
c
         integer         iv(1)
         integer         inf, liv, mxliv, rdunit, sup
c
         character*80    cline
         integer         inc(100), fn(100), st(100),
     *                   curs, ng
         logical         found
c
         read(rdunit,1040,end=100,err=100) cline
1040     format(/a)
         IF( CLINE .EQ. ' ') THEN
            call livrmp(iv,inf,1,sup-inf+1)
            liv = sup-inf+1
            readiv = 0
         else
            curs = 1
            call psexpr(cline,curs,st,fn,inc,ng,found)
            if( found ) then
               readiv =
     *         makind(iv,liv,mxliv,st,fn,inc,ng,inf,sup)
            else
               readiv = -2
            end if
         end if
c
         return
c
 100     continue
            readiv = -3
         return
c
      end
c

c-------------------------------------------------------------------------------
c
c     History: READIV.                                                  y)
c
c     Blank line returns null index vector in this version.
c
c-------------------------------------------------------------------------------
c
      integer function rdivnl(iv,liv,mxliv,inf,sup,rdunit)
c
         integer         makind
c
         integer         iv(1)
         integer         inf, liv, mxliv, rdunit, sup
c
         character*80    cline
         integer         inc(100), fn(100), st(100),
     *                   curs, ng
         logical         found
c
         read(rdunit,1040,end=100,err=100) cline
1040     format(/a)
         IF( CLINE .EQ. ' ') THEN
            liv = 0
            rdivnl = 0
         else
            curs = 1
            call psexpr(cline,curs,st,fn,inc,ng,found)
            if( found ) then
               rdivnl =
     *         makind(iv,liv,mxliv,st,fn,inc,ng,inf,sup)
            else
               rdivnl = -2
            end if
         end if
c
         return
c
 100     continue
            rdivnl = -3
         return
c
      end

c-------------------------------------------------------------------------------
c
c     History: READIV, RDIVNL.
c
c     Blank line returns null index vector in this version.
c
c     Character string passed directly instead of reading via RDUNIT.
c
c-------------------------------------------------------------------------------
c
      integer function irdivn(iv,liv,mxliv,inf,sup,cline)
c
         integer         makind
c
         integer         iv(1)
         integer         inf, liv, mxliv, sup
c
         character*(*)   cline
         integer         inc(100), fn(100), st(100),
     *                   curs, ng
         logical         found
c
         IF( CLINE .EQ. ' ') THEN
            liv = 0
            irdivn = 0
         else
            curs = 1
            call psexpr(cline,curs,st,fn,inc,ng,found)
            if( found ) then
               irdivn =
     *         makind(iv,liv,mxliv,st,fn,inc,ng,inf,sup)
            else
               irdivn = -2
            end if
         end if
c
         return
c
 100     continue
            irdivn = -3
         return
c
      end

c-------------------------------------------------------------------------------
c
c     History: IRDIVN.
c
c     Blank line returns full index vector in this version.
c
c     Character string passed directly instead of reading via RDUNIT.
c
c-------------------------------------------------------------------------------
c
      integer function irdiv(iv,liv,mxliv,inf,sup,cline)
c
         integer         makind
c
         integer         iv(1)
         integer         inf, liv, mxliv, sup
c
         character*(*)   cline
         integer         inc(100), fn(100), st(100),
     *                   curs, ng
         logical         found
c
         IF( CLINE .EQ. ' ') THEN
            liv = sup - inf + 1
            call locivramp(iv,sup,1,liv)
            irdiv = 0
         else
            curs = 1
            call psexpr(cline,curs,st,fn,inc,ng,found)
            if( found ) then
               irdiv =
     *         makind(iv,liv,mxliv,st,fn,inc,ng,inf,sup)
            else
               irdiv = -2
            end if
         end if
c
         return
c
 100     continue
            irdiv = -3
         return
c
      end


c-------------------------------------------------------------------------------
c
c     Returns position of first non-blank character starting from (and
c     including) CURSORth position.
c
c-------------------------------------------------------------------------------
c
      integer function skblnk(s,cursor)
c
         character*(*)   s
         integer         cursor
c
         character*1     blank 
         integer         i

         DATA            BLANK / ' ' /
c
         if( cursor .le. len(s) ) then
            do 10 i = cursor , len(s)
               if( s(i:i) .ne. blank ) go to 20
 10         continue
            skblnk = len(s) + 1
            return
c
 20         skblnk = i
            return
        else
            skblnk = cursor
            return
        end if
c
      end
c
c-----------------------------------------------------------------------
c
c     Blank-delimited token isolator.
c
c-----------------------------------------------------------------------
c
      character*(*) function gtok(s,curs)
c
         integer        index, skblnk
         character*(*)  s
         integer        curs, ncurs
c
         GTOK(1:LEN(GTOK)) = ' '
         curs = skblnk(s,curs)
C*       WRITE(*,*) '***1 CURS = ',CURS
         if( curs .le. len(s) ) then
            NCURS = INDEX(S(CURS:LEN(S)),' ') + CURS - 1
            if( ncurs .lt. curs ) ncurs = len(s) + 1
            gtok = s(curs:ncurs-1)
            curs = skblnk(s,ncurs)
C*          WRITE(*,*) '**2 CURS =',CURS,' NCURS = ',NCURS
         end if
c
         return
c
      end
c
c-----------------------------------------------------------------------
c
c     Concatenates S1, S2; squeezing out blanks in process.
c
c-----------------------------------------------------------------------
c
      character*(*) function catsqz(s1,s2)
c
         character*(*)   s1, s2
         integer         ixspan, skblnk
         integer         ifin, ist, n1, n2
c
         CATSQZ = ' '
         ist = skblnk(s1,1)
         if( ist .le. len(s1) ) then
            IFIN = IST + IXSPAN(S1(IST:LEN(S1)),' ') - 1
            n1 = ifin - ist + 1
            catsqz(1:n1) = s1(ist:ifin)
         else
            n1 = 0
         end if
         ist = skblnk(s2,1)
         if( ist .le. len(s2) ) then
            IFIN = IST + IXSPAN(S2(IST:LEN(S2)),' ') - 1
            n2 = ifin - ist + 1
            catsqz(n1+1:n1+1+n2) = s2(ist:ifin)
         else
            n2 = 0
         end if
c
         return
c
      end
c
c-----------------------------------------------------------------------
c
c     Returns index of last non-blank character in S.
c
c-----------------------------------------------------------------------
c
      integer function indlnb(s)
c
         character*(*)    s
c
         do 10 indlnb = len(s) , 1 , -1
            IF( S(INDLNB:INDLNB) .NE. ' ' ) RETURN
 10      continue
         indlnb = 0
c
         return
c
      end

c-----------------------------------------------------------------------
c
c     Returns index of first non-blank character in S.
c
c-----------------------------------------------------------------------
c
      integer function indfnb(s)
c
         character*(*)    s
c
         do 10 indfnb = 1 , len(s)
            IF( S(INDFNB:INDFNB) .NE. ' ' ) RETURN
 10      continue
         indfnb = 0
c
         return
c
      end

c---------------------------------------------------------------------
c
c     Initializes V to ramp function - origin V0, increment VINC.
c
c---------------------------------------------------------------------
c
      subroutine livrmp(v,v0,vinc,n)
c
         integer     v(1)
         integer     v0, vinc
         integer     i, n
c
         v(1) = v0
         do 10 i = 2 , n
            v(i) = v(i-1) + vinc
 10      continue
c
         return
c
      end
c
c---------------------------------------------------------------------
c
c     LOCAL VERSION of IVECLIB routine ...
c
c     Initializes V to ramp function - origin V0, increment VINC.
c
c---------------------------------------------------------------------
c
      subroutine locivramp(v,v0,vinc,n)
c
         integer     v(1)
         integer     v0, vinc
         integer     i, n
c
         v(1) = v0
         do 10 i = 2 , n
            v(i) = v(i-1) + vinc
 10      continue
c
         return
c
      end
c=======================================================================
c     Routines from ~matt/utilio/misc.f
c=======================================================================

c-------------------------------------------------------------------------
c
c     Returns first unit number .ge. umin not attached to a file.
c
c-------------------------------------------------------------------------

      integer function getu()

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
			write(*,*) 'getu: **** No available unit number ****'

         return
      end


c=======================================================================
c     Routines from ~matt/vutil/iveclib.f
c=======================================================================

c---------------------------------------------------------------------
c     Load vector with scalar.
c---------------------------------------------------------------------
      subroutine l_ivls(v1,s1,n)
			implicit    none

         integer     v1(1)
         integer     s1
         integer     i, n

         do 10 i = 1 , n
            v1(i) = s1
 10      continuE
 
         returN
 
      end

c-----------------------------------------------------------------------
c     Computes product of vector elements.
c-----------------------------------------------------------------------
      integer function l_ivprod(v,n)
			implicit      none

         integer       v(*),      n

         integer       i
         
         if( n .gt. 0 ) then
            l_ivprod = v(1)
            do 10 i = 2 , n
               l_ivprod = l_ivprod * v(i)
 10         continue
         else 
            l_ivprod = 0
         end if

         return

      end 
