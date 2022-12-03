c---------------------------------------------------------------------
c
c     S I N G L E     P R E C I S I O N     V E C T O R
c
c                    O P E R A T I O N S
c
c---------------------------------------------------------------------
c
c
c---------------------------------------------------------------------
c
c     Vector-vector multiply.
c
c---------------------------------------------------------------------
c
      subroutine svvm(v1,v2,v3,n)
c
         real*4      v1(1), v2(1), v3(1)
         integer     i, n
c
         do 10 i = 1 , n
            v3(i) = v1(i) * v2(i)
 10      continue
c
         return
c
      end
c
c---------------------------------------------------------------------
c
c     Vector-vector divide.
c
c---------------------------------------------------------------------
c
      subroutine svvd(v1,v2,v3,n)
c
         real*4      v1(1), v2(1), v3(1)
         integer     i, n
c
         do 10 i = 1 , n
            v3(i) = v1(i) / v2(i)
 10      continue
c
         return
c
      end
c
c---------------------------------------------------------------------
c
c     Vector-vector add.
c
c---------------------------------------------------------------------
c
      subroutine svva(v1,v2,v3,n)
c
         real*4      v1(1), v2(1), v3(1)
         integer     i, n
c
         do 10 i = 1 , n
            v3(i) = v1(i) + v2(i)
 10      continue
c
         return
c
      end
c
c---------------------------------------------------------------------
c
c     Vector-vector subtract.
c
c---------------------------------------------------------------------
c
      subroutine svvs(v1,v2,v3,n)
c
         real*4      v1(1), v2(1), v3(1)
         integer     i, n
c
         do 10 i = 1 , n
            v3(i) = v1(i) - v2(i)
 10      continue
c
         return
c
      end
c
c---------------------------------------------------------------------
c
c     Vector-scalar multiply
c
c---------------------------------------------------------------------
c
      subroutine svsm(v1,s1,v2,n)
c
         real*4      v1(1), v2(1)
         real*4      s1
         integer     i, n
c
         do 10 i = 1 , n
            v2(i) = s1 * v1(i)
 10      continue
c
         return
c
      end
c
c---------------------------------------------------------------------
c
c     Vector-scalar add.
c
c---------------------------------------------------------------------
c
      subroutine svsa(v1,s1,v2,n)
c
         real*4      v1(1), v2(1)
         real*4      s1
         integer     i, n
c
         do 10 i = 1 , n
            v2(i) = v1(i) + s1
 10      continue
c
         return
c
      end
c
c---------------------------------------------------------------------
c
c     Load vector with scalar.
c
c---------------------------------------------------------------------
c
      subroutine svls(v1,s1,n)
c
         real*4      v1(1)
         real*4      s1
         integer     i, n
c
         do 10 i = 1 , n
            v1(i) = s1
 10      continue
c
         return
c
      end
c
c---------------------------------------------------------------------
c
c     Vector negate.
c
c---------------------------------------------------------------------
c
      subroutine svneg(v1,v2,n)
c
         real*4      v1(1), v2(1)
         integer     i, n
c
         do 10 i = 1 , n
            v2(i) = -v1(i)
 10      continue
c
         return
c
      end
c
c---------------------------------------------------------------------
c
c     Vector function application.
c
c---------------------------------------------------------------------
c
      subroutine svfapl(v1,v2,f,n)
c
         real*4      f
         external    f
         real*4      v1(1), v2(1)
         integer     i, n
c
         do 10 i = 1 , n
            v2(i) = f(v1(i))
 10      continue
c
         return
c
      end
c
c---------------------------------------------------------------------
c
c     Initializes V to ramp function - origin V0, increment VINC.
c
c---------------------------------------------------------------------
c
      subroutine svramp(v,v0,vinc,n)
c
         real*4      v(1)
         real*4      v0, vinc
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
c     Vector copy.
c
c---------------------------------------------------------------------
c
      subroutine svcopy(v1,v2,n)
c
         real*4      v1(1), v2(1)
         integer i, n
c
         do 10 i = 1 , n
            v2(i) = v1(i)
 10      continue
c
         return
c
      end
c
c---------------------------------------------------------------------
c
c     Vector reversal.
c
c---------------------------------------------------------------------
c
      subroutine svrev(v1,v2,n)
c
         real*4      v1(1), v2(1)
         integer     i, n
c
         do 10 i = 1 , n
            v2(i) = v1(n-i+1)
 10      continue
c
         return
c
      end
c
c---------------------------------------------------------------------
c
c     Vector extrapolation.
c
c---------------------------------------------------------------------
c
      subroutine svextr(v1,v2,v3,p,n)
c
         real*4      v1(1), v2(1), v3(1)
         real*4      p
         integer     i, n
c
         do 10 i = 1 , n
            v3(i) = (1.0e0 + p) * v1(i) - p * v2(i)
 10      continue
c
         return
c
      end
c
c---------------------------------------------------------------------
c
c     Vector/scalar divide.
c     (Vector reciprocal: CALL SVSD(V1,V2,1.0E0,N) )
c
c---------------------------------------------------------------------
c
      subroutine svsd(v1,v2,s1,n)
c
         real*4      v1(1), v2(1)
         real*4      s1
         integer     i, n
c
         do 10 i = 1 , n
            v2(i) = s1 / v1(i)
 10      continue
c
         return
c
      end
c
c---------------------------------------------------------------------
c
c     Linearly interpolates regularly spaced values from scattered
c     values.
c
c---------------------------------------------------------------------
c
      subroutine svinq1(v,x,n,vin,nin,xin0,dxin,vs,vf)
c
         real*4      x(1), v(1), vin(1)
         real*4      dxin, p, vf, vs, xbar, xin0, xj
         integer     i, j, n, nin
c
         i = 1
         j = 1
 10      if( i .gt. nin ) go to 100
             xbar = xin0 + dxin * (i - 1)
             xj = x(j)
             if( xbar - xj ) 20, 50, 60
 20              if( j .eq. 1 ) go to 30
                     p = (xbar - x(j-1)) / (xj - x(j-1))
                     vin(i) = (1.0 - p) * v(j-1) + p * v(j)
                 go to 40
 30                  vin(i) = vs
 40              i = i + 1
             go to 90
 50              vin(i) = v(j)
                 i = i + 1
             go to 90
 60              if( j .eq. n ) go to 70
                     j = j + 1
                 go to 80
 70                  vin(i) = vf
                     i = i + 1
 80          go to 90
 90      go to 10
c
100      return
c
      end
c
c---------------------------------------------------------------------
c
c     Linearly interpolates one set of values from another set.
c
c---------------------------------------------------------------------
c
      subroutine svinq2(v,x,vbar,xbar,n,nbar,vs,vf)
c
         real*4      v(1), vbar(1), x(1), xbar(1)
         real*4      p, vf, vs, xibar, xj
         integer     ibar, j, n, nbar
c
         ibar = 1
         j = 1
 10      if( ibar .gt. nbar ) go to 100
             xibar = xbar(ibar)
             xj = x(j)
             if( xibar - xj ) 20, 50, 60
 20              if( j .eq. 1 ) go to 30
                     p = (xibar - x(j-1)) / (xj - x(j-1))
                     vbar(ibar) = (1.0 - p) * v(j-1) + p * v(j)
                 go to 40
 30                  vbar(ibar) = vs
 40              ibar = ibar + 1
             go to 90
 50              vbar(ibar) = v(j)
                 ibar = ibar + 1
             go to 90
 60              if( j .eq. n ) go to 70
                     j = j + 1
                 go to 80
 70                  vbar(ibar) = vf
                     ibar = ibar + 1
 80          go to 90
 90      go to 10
c
100      return
c
      end
c
c---------------------------------------------------------------------
c
c     Vector first divided difference.
c
c---------------------------------------------------------------------
c
      subroutine svdd1(v1,v2,h,n)
c
         real*4      v1(1), v2(1)
         real*4      h, hm1
         integer     i, n
c
         hm1 = 1.0e0 / h
         do 10 i = 2 , n
            v2(i-1) = hm1 * (v1(i) - v1(i-1))
 10      continue
c
         return
c
      end
c
c---------------------------------------------------------------------
c
c     Vector average.
c
c---------------------------------------------------------------------
c
      subroutine svav(v1,v2,n)
c
         real*4      v1(1), v2(1)
         integer     i, n
c
         do 10 i = 2 , n
            v2(i-1) = 0.5e0 * (v1(i) + v1(i-1))
 10      continue
c
         return
c
      end
c
c---------------------------------------------------------------------
c
c     Vector vector scalar multiply.
c
c---------------------------------------------------------------------
c
      subroutine svvsm(v1,v2,v3,sc,n)
c
         real*4      v1(1), v2(1), v3(1)
         real*4      sc
         integer     i, n
c
         do 10 i = 1 , n
            v3(i) = sc * v1(i) * v2(i)
 10      continue
c
         return
c
      end
c
c---------------------------------------------------------------------
c
c     Vector vector add scalar multiply.
c
c---------------------------------------------------------------------
c
      subroutine svvasm(v1,v2,v3,sc,n)
c
         real*4      v1(1), v2(1), v3(1)
         real*4      sc
         integer     i, n
c
         do 10 i = 1 , n
            v3(i) = sc *(v1(i) + v2(i))
 10      continue
c
         return
c
      end
c
c---------------------------------------------------------------------
c
c     (Vector scalar multiply) add (vector scalar multiply).
c
c---------------------------------------------------------------------
c
      subroutine svsvsa(v1,v2,v3,s1,s2,n)
c
         real*4      v1(1), v2(1), v3(1)
         real*4      s1, s2
         integer     i, n
c
         do 10 i = 1 , n
            v3(i) = s1 * v1(i) + s2 * v2(i)
 10      continue
c
         return
c
      end
c
c---------------------------------------------------------------------
c
c     Vector absolute difference.
c
c---------------------------------------------------------------------
c
      subroutine svabd(v1,v2,v3,n)
c
         real        abs
         real        v1(1), v2(1), v3(1)
         integer     i, n
c
         do 10 i = 1 , n
            v3(i) = abs(v1(i) - v2(i))
 10      continue
c
         return
c
      end
c
c---------------------------------------------------------------------
c
c     Vector absolute relative difference.
c
c---------------------------------------------------------------------
c
      subroutine svabrd(v1,v2,v3,n)
c
         real        abs
         real        v1(1), v2(1), v3(1)
         real        v1i, v2i
         integer     i, n
c
         do 20 i = 1 , n
            v1i = v1(i)
            v2i = v2(i)
            if( v1i .eq. v2i .or. v1i .eq. 0.0e0 ) go to 10
                v3(i) = abs((v1i - v2i) / v1i)
            go to 20
 10             v3(i) = 0.0e0
 20      continue
c
         return
c
      end
c
c---------------------------------------------------------------------
c
c     Computes "comcavity" function which is undefined at endpoints.
c
c---------------------------------------------------------------------
c
      subroutine svconc(v1,v2,n)
c
         real        v1(1), v2(1)
         integer     n, nm1
c
         nm1 = n - 1
         v2(1) = 0.0e0
         do 10 j = 2 , nm1
            v2(j) = v1(j) - 0.5e0 * (v1(j-1) + v1(j+1))
 10      continue
         v2(n) = 0.0e0
c
         return
c
      end
c
c---------------------------------------------------------------------
c
c     Vecotr dot product.
c
c---------------------------------------------------------------------
c
      real function svvdot(v1,v2,n)
c
         real        v1(1), v2(1)
         integer     i, n
c
         svvdot = v1(1) * v2(1)
         do 10 i = 2 , n
            svvdot = svvdot + v1(i) * v2(i)
 10      continue
c
         return
c
      end
c
c---------------------------------------------------------------------
c
c     Calculates component of V1 normal to V2.
c
c---------------------------------------------------------------------
c
      subroutine svvprj(v1,v2,v3,n)
c
         real        dsqrt, svvdot
         real        v1(1), v2(1), v3(1)
         integer     n
c
         call svsvsa(v1,v2,v3,1.0e0,-svvdot(v1,v2,n)/svvdot(v2,v2,n),
     *               n)
c
         return
c
      end
c
c---------------------------------------------------------------------
c
c     Vector sum.
c
c---------------------------------------------------------------------
c
      real function svsum(v,n)
c
         real        v(1)
         integer     i, n
c
         svsum = v(1)
         do 10 i = 2 , n
            svsum = svsum + v(i)
 10      continue
c
         return
c
      end
c
c---------------------------------------------------------------------
c
c     Produces vector from Nth value and vector of divided
c     differences.
c
c---------------------------------------------------------------------
c
      subroutine svdupr(v,sv,h,n)
c
         real        sv(1), v(1)
         real        h
         integer     j, k, n, nm1
c
         nm1 = n - 1
         do 10 j = 1 , nm1
            k = n - j
            v(k) = v(k+1) - h * sv(k)
 10      continue
c
         return
c
      end
c
c---------------------------------------------------------------------
c
c     Produces vector from 1st value and vector of divided differ-
c     ences.
c
c---------------------------------------------------------------------
c
      subroutine svdup(v,sv,h,n)
c
         real        sv(1), v(1)
         real        h
         integer     j, nm1
c
         nm1 = n - 1
         do 10 j = 1 , nm1
            v(j+1) = v(j) + h * sv(j)
10       continue
c
         return
c
      end
c
c---------------------------------------------------------------------
c
c     Vector-vecotr divide with check for division by 0.
c
c---------------------------------------------------------------------
c
      subroutine svvd0(v1,v2,v3,n)
c
         real        v1(1), v2(1), v3(1)
         real        v2j
         integer     j, n
c
         do 30 j = 1 , n
            v2j = v2(j)
            if( v2j .eq. 0.0e0 ) go to 10
               v3(j) = v1(j) / v2j
            go to 20
 10            v3(j) = 0.0e0
 20         continue
 30      continue
c
         return
c
      end
c
c---------------------------------------------------------------------
c
C     Injects every INC'th element of V1 into V2.
c
c---------------------------------------------------------------------
c
      subroutine svinj(v1,v2,inc,n)
c
         real        v1(1), v2(1)
         integer     i, inc, j, n
c
         j = 1
         do 10 i = 1 , n , inc
            v2(j) = v1(i)
            j = j + 1
 10      continue
c
         return
c
      end
c
c---------------------------------------------------------------------
c
c     Computes normalized l-2 norm of V.
c
c---------------------------------------------------------------------
c
      real function svnrm2(v,n)
c
         real        sqrt, svvdot
         real        v(1)
         integer     n
c
         svnrm2 = sqrt((1.0e0 / n) * svvdot(v,v,n))
c
         return
c
      end
c
c-----------------------------------------------------------------------
c
c     Returns index of element in ascending ordered array V which is
c     closest to KEY.
c
c-----------------------------------------------------------------------
c
      integer function isvcls(v,n,key)
c
         integer      n
         real         v(n)
         real         key
c
         do 10 i = 1 , n-1
            if( v(i) .ge. key ) then
               isvcls = i
               go to 20
            else
               if ( v(i+1) .gt. key ) then
                  if( abs(v(i) - key) .lt. abs(v(i+1) - key) ) then
                     isvcls = i
                  else
                     isvcls = i + 1
                  end if
                  go to 20
               end if
            end if
 10      continue
         isvcls = n
c
 20      return
c
      end
c
c---------------------------------------------------------------------
c
c     Vector minimum.
c
c---------------------------------------------------------------------
c
      real function svmin(v1,n)
c
         real        amin1
         real        v1(1)
         integer     i, n
c
         svmin = v1(1)
         do 10 i = 2 , n
            svmin = amin1(svmin,v1(i))
 10      continue
c
         return
c
      end
c
c---------------------------------------------------------------------
c
c     Vector maximum.
c
c---------------------------------------------------------------------
c
      real function svmax(v1,n)
c
         real        amax1
         real        v1(1)
         integer     i, n
c
         svmax = v1(1)
         do 10 i = 2 , n
            svmax = amax1(svmax,v1(i))
 10      continue
c
         return
c
      end
c
c---------------------------------------------------------------------
c
c     Returns index of (first encountered) minimum of V.
c
c---------------------------------------------------------------------
c
      integer function isvmin(v,n)
c
         real        v(1)
         real        vi, vmin
         integer     i ,n
c
         vmin = v(1)
         isvmin = 1
         do 10 i = 2 , n
            vi = v(i)
            if( vi .ge. vmin ) go to 10
               vmin = vi
               isvmin = i
 10      continue
c
         return
c
      end
c
c---------------------------------------------------------------------
c
c     Returns index of (first encountered) maximum of V.
c
c---------------------------------------------------------------------
c
      integer function isvmax(v,n)
c
         real        v(1)
         real        vi, vmax
         integer     i ,n
c
         vmax = v(1)
         isvmax = 1
         do 10 i = 2 , n
            vi = v(i)
            if( vi .le. vmax ) go to 10
               vmax = vi
               isvmax = i
 10      continue
c
         return
c
      end
c
c-----------------------------------------------------------------------
c
c     Vector double to single conversion.
c
c-----------------------------------------------------------------------
c
      subroutine svsngl(v1,v2,n)
c
         integer     i, n
         real*8      v1(n)
         real        v2(n)
c
         do 10 i = 1 , n
            v2(i) = sngl(v1(i))
 10      continue
c
         return
c
      end
c
c-----------------------------------------------------------------------
c
c     Dumps vector labelled with LABEL on UNIT.
c
c-----------------------------------------------------------------------
c
      subroutine svdump(v,n,label,unit)
c
         real           v(1)
         character*(*)  label
         integer        i, n, st, unit
c
         if( n .lt. 1 ) go to 130
            write(unit,100) label
 100        FORMAT(//' <<< ',A,' >>>'/)
            st = 1
 110        continue
               write(unit,120) ( v(i) , i = st , min(st+3,n))
 120           FORMAT(' ',4(1PE19.10))
               st = st + 4
            if( st .le. n ) go to 110
c
 130     continue
c
         return
c
      end
c
c-----------------------------------------------------------------------
c
c     Performs linear transformation of X to XP which maps X(1) to
c     XP1 and X(N) to XPN.
c
c-----------------------------------------------------------------------
c
      subroutine svlmap(x,xp,n,xp1,xpn)
c
         implicit      logical*1 (a-z)
c
         integer       n
         real          x(n),     xp(n)
         real          xp1,      xpn
c
c        Introduce temporaries so destructive transformation will work.
c
         real          x1,       xn
c
         x1 = x(1)
         xn = x(n)
         if( x1 .ne. xn ) then
            call svsm(x,(xpn - xp1)/(xn - x1),xp,n)
            call svsa(xp,(xn * xp1 - x1 * xpn) / (xn - x1),
     *                xp,n)
         end if
c
         return
c
      end
c
c-----------------------------------------------------------------------
c     "Thins" (XFR(i),YFR(i)) pairs by scaling vectors to 1.0 x ASPECT
c     box and demanding that arc length between (XTO(i),YTO(i) pairs
c     is at least FRMNDS x length of box diagonal. End points always
c     members of returned vectors. Aborts if no x, y range.
c
c-----------------------------------------------------------------------

      subroutine svthin(xfr,yfr,nfr, xto,yto,nto, aspect,frmnds)

         implicit       logical*1 (a-z)

         real           svmin,     svmax

         integer        nfr,       nto
         real           xfr(1),    yfr(1),    xto(1),    yto(1)
         real           aspect,    frmnds

         real           xrange,    yrange,    xscale,    yscale,
     *                  mnds
         integer        ifr

         xrange = svmax(xfr,nfr) - svmin(xfr,nfr)
         if( xrange .eq. 0.0e0 ) then
            WRITE(*,*) '<<< SVTHIN: No x--range. >>>'
            nto = 0
            return
         end if

         yrange = svmax(yfr,nfr) - svmin(yfr,nfr)
         if( yrange .eq. 0.0e0 ) then
            WRITE(*,*) '<<< SVTHIN: No y--range. >>>'
            nto = 0
            return
         end if

         xscale = 1.0e0 / xrange
         yscale = aspect / yrange

         mnds = frmnds * sqrt(1.0e0 + aspect * aspect)

         xto(1) = xfr(1)
         yto(1) = yfr(1)
         nto = 1
         do 10 ifr = 2 , nfr
            if( sqrt( (xscale * (xfr(ifr) - xto(nto))) ** 2 +
     *                (yscale * (yfr(ifr) - yto(nto))) ** 2 )  .ge.
     *          mnds ) then
               nto = nto + 1
               xto(nto) = xfr(ifr)
               yto(nto) = yfr(ifr)
            end if
 10      continue
         if( xto(nto) .ne. xfr(nfr)  .or.
     *       yto(nto) .ne. yfr(nfr) ) then
            nto = nto + 1
            xto(nto) = xfr(nfr)
            yto(nto) = yfr(nfr)
         end if

         return

      end
c
c
c-----------------------------------------------------------------------
c
c     Transfers X(j), Y(j) such that xmin <= X(j) <= xmax to
c     XTO, YTO.
c
c-----------------------------------------------------------------------
c
      subroutine svrng(x,y,n,xto,yto,nto,xmin,xmax)
c
         implicit     logical*1 (a-z)
c
         integer      n,       nto
         real         x(1),    y(1),
     *                xto(1),  yto(1)
         real         xmin,    xmax
c
         integer      j,       jst,    jfin
c
         if( xmax .ge. xmin  .and.  n .ge. 1 ) then
            jst  = n + 1
            jfin = 0
            j = 1
 100        continue
            if( j .gt. n  .or.  jst .le. n ) go to 200
               if( x(j) .ge. xmin ) then
                  jst = j
               end if
               j = j + 1
            go to 100
 200        continue
            j = n
 300        continue
            if( j .lt. 1  .or.  jfin .ge. 1 ) go to 400
               if( x(j) .le. xmax ) then
                  jfin = j
               end if
               j = j - 1
            go to 300
 400        continue
            nto = jfin - jst + 1
            if( nto .ge. 1 ) then
               call svcopy(x(jst),xto,nto)
               call svcopy(y(jst),yto,nto)
            end if
c
         else
            nto = 0
         end if

         return

      end

c-----------------------------------------------------------------------
c
c     Scales single precision vector (if possible) to integer rep.
c     where range of data corresponds (is binned) to IMAX - IMIN + 1
c     equally spaced sub--ranges.
c
c-----------------------------------------------------------------------

      subroutine sviscl(v,iv,n,imin,imax)

         implicit     logical (a-z)

         integer      n
         real         v(n)
         integer      iv(n)
         integer      imin,         imax

         real         vmin,         vmax,        dvm1
         integer      j

         if( imax .eq. imin ) then
            do 10 j = 1 , n
               iv(j) = imin
 10         continue
            return
         end if

         vmin = v(1)
         vmax = v(1)

         do 20 j = 1 , n
            vmin = min(v(j),vmin)
            vmax = max(v(j),vmax)
 20      continue

         if( vmin .eq. vmax ) then
            do 30 j = 1 , n
               iv(j) = (imax - imin) / 2
 30         continue
         else
            dvm1 = 1.0e0 * (imax - imin + 1) / (vmax - vmin)
            do 40 j = 1 , n
               iv(j) = min(imax,imin +
     *                          int( dvm1 * (v(j) - vmin) ))
 40         continue
         end if

         return

       end

c-----------------------------------------------------------------------
c
c     V3 = sqrt( v1 ** 2 + v2 ** 2 ).
c
c-----------------------------------------------------------------------
c
      subroutine svpyth(v1,v2,v3,n)
c
         intrinsic    sqrt
         real         v1(1), v2(1), v3(1)
         integer      n
c
         call svvm(v1,v1,v3,n)
         call svavvm(v3,v2,v2,v3,n)
         call svfapl(v3,v3,sqrt,n)
c
         return
c
      end

c-----------------------------------------------------------------------
c
c     V4 = V1 + V2 * V3.
c
c-----------------------------------------------------------------------
c
      subroutine svavvm(v1,v2,v3,v4,n)
c
         real       v1(1), v2(1), v3(1), v4(1)
         integer    i, n
c
         do 10 i = 1 , n
            v4(i) = v1(i) + v2(i) * v3(i)
 10      continue
c
         return
c
      end

c-----------------------------------------------------------------------

      integer function myisamax(n,x,incx)

         implicit       none

         integer        n,     incx 
         real           x(n)

         integer        i,     iter
         real           maxval

         myisamax = 1

         if( incx .eq. 0 ) then
            return 
         end if

         if( abs(incx) .ne. 1 ) then
            write(0,*) 'myisamax: This routine not'//
     *                 ' implemented for abs(incx) .ne. 1'
            return
         end if

         if( incx .gt. 0 ) then
            myisamax = 1
            maxval   = abs(x(1))
            iter = 1
            do i = 1 + incx , n , incx
               iter = iter + 1
               if( abs(x(i)) .gt. maxval ) then
                  myisamax = iter
                  maxval = abs(x(i))
               end if
            end do
         else 
            myisamax = n
            maxval = abs(x(n))
            iter = 1
            do i = n + incx , 1 , incx
               iter = iter + 1
               if( abs(x(i)) .gt. maxval ) then
                  myisamax = iter
                  maxval = abs(x(i))
               end if
            end do
         end if

         return

      end
