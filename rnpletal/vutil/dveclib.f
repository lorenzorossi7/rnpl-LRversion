c---------------------------------------------------------------------
c
c     D O U B L E     P R E C I S I O N     V E C T O R
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
      subroutine dvvm(v1,v2,v3,n)
c
         real*8      v1(1), v2(1), v3(1)
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
      subroutine dvvd(v1,v2,v3,n)
c
         real*8      v1(1), v2(1), v3(1)
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
c-----------------------------------------------------------------------
c
c     Vector/vector divide with check for division by zero.
c
c-----------------------------------------------------------------------
c
      subroutine dvvdx(v1,v2,v3,n)
c
         real*8      v1(1), v2(1), v3(1)
         integer     i, n
c
         do 10 i = 1 , n
            if( v2(i) .eq. 0.0d0 ) then
               v3(i) = 0.0d0
            else
               v3(i) = v1(i) / v2(i)
            end if
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
      subroutine dvva(v1,v2,v3,n)
c
         real*8      v1(1), v2(1), v3(1)
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
      subroutine dvvs(v1,v2,v3,n)
c
         real*8      v1(1), v2(1), v3(1)
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
      subroutine dvsm(v1,s1,v2,n)
c
         real*8      v1(1), v2(1)
         real*8      s1
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
      subroutine dvsa(v1,s1,v2,n)
c
         real*8      v1(1), v2(1)
         real*8      s1
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
      subroutine dvls(v1,s1,n)
c
         real*8      v1(1)
         real*8      s1
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
      subroutine dvneg(v1,v2,n)
c
         real*8      v1(1), v2(1)
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
      subroutine dvfapl(v1,v2,f,n)
c
         external    f
c
         real*8      f
         real*8      v1(1), v2(1)
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
      double precision function dminus(x)

         real*8      x

         dminus = -x
   
         return

      end

      double precision function drecip(x)

         real*8      x

         drecip = 1.0d0 / x
   
         return

      end

      double precision function ldexp(x)

         real*8      x

         ldexp = exp(x)
   
         return

      end
c
c---------------------------------------------------------------------
c
c     Initializes V to ramp function - origin V0, increment VINC.
c
c---------------------------------------------------------------------
c
      subroutine dvramp(v,v0,vinc,n)
c
         real*8      v(1)
         real*8      v0, vinc
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
      subroutine dvcopy(v1,v2,n)
c
         real*8      v1(1), v2(1)
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
      subroutine dvrev(v1,v2,n)
c
         real*8      v1(1), v2(1)
         integer     i, n
c
         do 10 i = 1 , n
            v2(i) = v1(n-i+1)
 10      continue
c
         return
c
      end

c---------------------------------------------------------------------
c
c     Vector in--place reversal.
c
c---------------------------------------------------------------------
c
      subroutine dvrevp(v,n)
c
         implicit    none

         real*8      v(1)
         integer     i, n
         real*8      vt

         do 100 i = 1 , n / 2
            vt= v(i) 
            v(i) = v(n-i+1)
            v(n-i+1) = vt
 100     continue

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
      subroutine dvextr(v1,v2,v3,p,n)
c
         real*8      v1(1), v2(1), v3(1)
         real*8      p
         integer     i, n
c
         do 10 i = 1 , n
            v3(i) = (1.0d0 + p) * v1(i) - p * v2(i)
 10      continue
c
         return
c
      end
c
c---------------------------------------------------------------------
c
c     Vector/scalar divide.
c     (Vector reciprocal: CALL DVSD(V1,V2,1.0D0,N) )
c
c---------------------------------------------------------------------
c
      subroutine dvsd(v1,v2,s1,n)
c
         real*8      v1(1), v2(1)
         real*8      s1
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
      subroutine dvinq1(v,x,n,vin,nin,xin0,dxin,vs,vf)
c
         real*8      x(1), v(1), vin(1)
         real*8      dxin, p, vf, vs, xbar, xin0, xj
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
      subroutine dvinq2(v,x,vbar,xbar,n,nbar,vs,vf)
c
         real*8      v(1), vbar(1), x(1), xbar(1)
         real*8      p, vf, vs, xibar, xj
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
      subroutine dvdd1(v1,v2,h,n)
c
         real*8      v1(1), v2(1)
         real*8      h, hm1
         integer     i, n
c
         hm1 = 1.0d0 / h
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
      subroutine dvav(v1,v2,n)
c
         real*8      half 
         parameter ( half = 0.5d0 )

         real*8      v1(1), v2(1)
         integer     i, n

c
         do 10 i = 2 , n
            v2(i-1) = half * (v1(i) + v1(i-1))
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
      subroutine dvvsm(v1,v2,v3,sc,n)
c
         real*8      v1(1), v2(1), v3(1)
         real*8      sc
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
      subroutine dvvasm(v1,v2,v3,sc,n)
c
         real*8      v1(1), v2(1), v3(1)
         real*8      sc
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
c     Vector vector subtract scalar multiply.
c
c---------------------------------------------------------------------
c
      subroutine dvvssm(v1,v2,v3,sc,n)
c
         real*8      v1(1), v2(1), v3(1)
         real*8      sc
         integer     i, n
c
         do 10 i = 1 , n
            v3(i) = sc * (v2(i) - v1(i))
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
      subroutine dvsvsa(v1,v2,v3,s1,s2,n)
c
         real*8      v1(1), v2(1), v3(1)
         real*8      s1, s2
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
      subroutine dvabd(v1,v2,v3,n)
c
         real*8      v1(1), v2(1), v3(1)
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
      subroutine dvabrd(v1,v2,v3,n)
c
         real*8      v1(1), v2(1), v3(1)
         real*8      v1i, v2i
         integer     i, n
c
         do 20 i = 1 , n
            v1i = v1(i)
            v2i = v2(i)
            if( v1i .eq. v2i .or. v1i .eq. 0.0d0 ) go to 10
                v3(i) = abs((v1i - v2i) / v1i)
            go to 20
 10             v3(i) = 0.0d0
 20      continue

         return
c
      end
c
c-----------------------------------------------------------------------
c
c     Vector relative difference.
c
c-----------------------------------------------------------------------
c
      subroutine dvrd(v1,v2,v3,n)
c
         real*8      v1(1), v2(1), v3(1)
         integer     n
c
         call dvvs(v2,v1,v3,n)
         call dvvdx(v3,v1,v3,n)
c
         return
c
      end
c
c---------------------------------------------------------------------
c
c     Computes "concavity" function which is undefined at endpoints
c     (essentially a second difference).
c
c---------------------------------------------------------------------
c
      subroutine dvconc(v1,v2,n)
c
         real*8      v1(1), v2(1)
         integer     n, nm1
c
         nm1 = n - 1
         v2(1) = 0.0d0
         do 10 j = 2 , nm1
            v2(j) = v1(j) - 0.5d0 * (v1(j-1) + v1(j+1))
 10      continue
         v2(n) = 0.0d0
c
         return
c
      end
c
c---------------------------------------------------------------------
c
c     Vector dot product.
c
c---------------------------------------------------------------------
c
      double precision function dvvdot(v1,v2,n)
c
         real*8      v1(1), v2(1)
         integer     i, n
c
         dvvdot = 0.0d0
         do i = 1 , n
            dvvdot = dvvdot + v1(i) * v2(i)
         end do
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
      subroutine dvvprj(v1,v2,v3,n)
c
         real*8      dvvdot
         real*8      v1(1), v2(1), v3(1)
         integer     n
c
         call dvsvsa(v1,v2,v3,1.0d0,-dvvdot(v1,v2,n)/dvvdot(v2,v2,n),
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
      double precision function dvsum(v,n)
c
         real*8      v(1)
         integer     i, n
c
         if( n .gt. 0 ) then
            dvsum = v(1)
            do 10 i = 2 , n
               dvsum = dvsum + v(i)
 10         continue
         end if
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
      double precision function dvmean(v,n)

         real*8       dvsum

         real*8       v(1)
         integer      n

         dvmean = dvsum(v,n) / n

         return

      end

c---------------------------------------------------------------------
c
c     Computes deviation about mean.
c
c---------------------------------------------------------------------

      subroutine dvdevm(v1,v2,n)

         real*8       dvmean

         real*8       v1(1),      v2(1)
         integer      n

         call dvsa(v1,-dvmean(v1,n),v2,n)

         return

      end
c
c---------------------------------------------------------------------
c
c     Vector sum of absolute values.
c
c---------------------------------------------------------------------
c
      double precision function dvsuma(v,n)
c
         real*8      v(1)
         integer     i, n
c
         dvsuma = abs(v(1))
         do 10 i = 2 , n
            dvsuma = dvsuma + abs(v(i))
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
      subroutine dvdupr(v,dv,h,n)
c
         real*8      dv(1), v(1)
         real*8      h
         integer     j, k, n, nm1
c
         nm1 = n - 1
         do 10 j = 1 , nm1
            k = n - j
            v(k) = v(k+1) - h * dv(k)
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
      subroutine dvdup(v,dv,h,n)
c
         real*8      dv(1), v(1)
         real*8      h
         integer     j, nm1
c
         nm1 = n - 1
         do 10 j = 1 , nm1
            v(j+1) = v(j) + h * dv(j)
10       continue
c
         return
c
      end
c
c---------------------------------------------------------------------
c
c     Vector first centred divided difference.
c
c---------------------------------------------------------------------
c
      subroutine dvdd01(v1,v2,h,n)
c
         real*8      v1(1), v2(1)
         real*8      h, hhm1
         integer     j, n, nm1
c
         nm1 = n - 1
         hhm1 = 0.5d0 / h
         do 10 j = 2 , nm1
            v2(j) = hhm1 * (v1(j+1) - v1(j-1))
 10      continue
c
         return
c
      end
c
c---------------------------------------------------------------------
c
c     Vector second centred divided difference.
c
c---------------------------------------------------------------------
c
      subroutine dvdd02(v1,v2,h,n)
c
         real*8      v1(1), v2(1)
         real*8      h, hm2
         integer     j, n, nm1
c
         nm1 = n - 1
         hm2 = 1.0d0 / (h * h)
         do 10 j = 2 , nm1
            v2(j) = hm2 * (v1(j+1) - 2.0d0 * v1(j) + v1(j-1))
 10      continue
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
      subroutine dvinj(v1,v2,inc,n)
c
         real*8      v1(1), v2(1)
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
C     Injects every INC'th element of V1 into V2 and returns
c     effected change in V2 in DV2.
c
c---------------------------------------------------------------------
c
      subroutine dvinju(v1,v2,dv2,inc,n)
c
         real*8      v1(1), v2(1), dv2(1)
         integer     i, inc, j, n
c
         j = 1
         do 10 i = 1 , n , inc
            dv2(j) = v1(i) - v2(j)
            v2(j)  = v2(j) + dv2(j)
            j = j + 1
 10      continue
c
         return
c
      end
c
c---------------------------------------------------------------------
c
c     V4 = V1 + SC * (V2 + V3)
c
c---------------------------------------------------------------------
c
      subroutine dvsvva(v1,v2,v3,sc,v4,nr)
c
         real*8         v1(1), v2(1), v3(1), v4(1)
         real*8         sc
         integer        j, nr
c
         do 10 j = 1 , nr
            v4(j) = v1(j) + sc * (v2(j) + v3(j))
 10      continue
c
         return
c
      end
c
c---------------------------------------------------------------------
c
c     V4 = V1 + SC * (V2 - V3)
c
c---------------------------------------------------------------------
c
      subroutine dvsvvs(v1,v2,v3,sc,v4,nr)
c
         real*8         v1(1), v2(1), v3(1), v4(1)
         real*8         sc
         integer        j, nr
c
         do 10 j = 1 , nr
            v4(j) = v1(j) + sc * (v2(j) - v3(j))
 10      continue
c
         return
c
      end

c
c---------------------------------------------------------------------
c
c     Routine useful for performing general 2nd order differencing of
c     first derivative term.
c
c---------------------------------------------------------------------
c
      subroutine dvdd1g(v1,v2,sc,v3,nr)
c
         real*8         v1(1), v2(1), v3(1)
         real*8         sc
         integer        j, nr, nrm1
c
         nrm1 = nr - 1
         do 10 j = 1 , nrm1
            v3(j) = sc * v1(j) * (v2(j+1) - v2(j))
 10      continue
c
         return
c
      end
c
c---------------------------------------------------------------------
c
c     Routine useful for performing general 2nd order centred
c     differencing of 2nd derivative terms.
c
c---------------------------------------------------------------------
c
      subroutine dvdd2g(v1,v2,v3,sc,v4,nr)
c
         real*8         v1(1), v2(1), v3(1), v4(1)
         real*8         sc
         integer        j, jp1, nr, nrm1
c
         nrm1 = nr - 1
         do 10 j = 2 , nrm1
            jp1 = j + 1
            v4(j) = sc * v1(j) * (v2(jp1) * (v3(jp1) - v3(j)) -
     *                            v2(j)   * (v3(j) - v3(j-1)))
 10      continue
c
         return
c
      end
c
c---------------------------------------------------------------------
c
c     Subroutine useful in implementing certain types of evolution
c     steps, etc.
c
c---------------------------------------------------------------------
c
      subroutine dvpms(v1,v2,v3,v4,v5,nr)
c
         real*8         v1(1), v2(1), v3(1), v4(1), v5(1)
         integer        j, nr
c
         do 10 j = 1 , nr
            v5(j) = v1(j) + v2(j) * (v3(j) - v4(j))
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
      double precision function dvnrm2(v,n)
c
         real*8      dvvdot
         real*8      v(*)
         integer     n
c
         dvnrm2 = sqrt((1.0d0 / n) * dvvdot(v,v,n))
c
         return
c
      end
c
c---------------------------------------------------------------------
c
c     Computes normalized l-1 norm of V.
c
c---------------------------------------------------------------------
c
      double precision function dvnrm1(v,n)
c
         real*8      v(1)
         integer     j,     n
c
         dvnrm1 = abs(v(1))
         do 10 j = 2 , n
            dvnrm1 = dvnrm1 + abs(v(j))
 10      continue
         dvnrm1 = dvnrm1 / n
c
         return
c
      end
c
c---------------------------------------------------------------------
c
c     Returns FTOC : 1  linear refinement of V1 in V2.
c
c---------------------------------------------------------------------
c
      subroutine dvliq1(v1,v2,ftoc,n1)
c
         real*8      v1(1), v2(1)
         real*8      dv2
         integer     ftoc, i, j, k, n1, n1m1
c
         n1m1 = n1 - 1
         j = 1
         v2(j) = v1(1)
         do 20 i = 1 , n1m1
            dv2 = (v1(i+1) - v1(i)) / ftoc
            do 10 k = 1 , ftoc
               j = j + 1
               v2(j) = v2(j-1) + dv2
 10         continue
 20      continue
c
         return
c
      end
c
c---------------------------------------------------------------------
c
C     Returns FTOC : 1 cubic refinement of V1 in V2 (user's respons-
c     ibility to ensure that V1 contains a minimum of 4 elements).
c
c---------------------------------------------------------------------
c
      subroutine dvcuq1(v1,v2,ftoc,n1)
c
         real*8      ngfpq3
         real*8      v1(*), v2(*)
         real*8      ftocr
         integer     ftoc, ftocp1, i1, i2, i, j, n1, n1m2
c
         n1m2 = n1 - 2
         ftocp1 = ftoc + 1
         i2 = 1
         ftocr = 1.0d0 / ftoc
c
         do 10 j = 1 , ftoc
            v2(i2) = ngfpq3(v1(1),v1(2),v1(3),v1(4),(j-1)*ftocr)
            i2 = i2 + 1
 10      continue
         do 30 i1 = 2 , n1m2
            do 20 j = 1 , ftoc
               v2(i2) = ngfpq3(v1(i1-1),v1(i1),v1(i1+1),v1(i1+2),
     *                         1.0d0 + (j-1)*ftocr)
               i2 = i2 + 1
 20         continue
 30      continue
         do 40 j = 1 , ftocp1
            v2(i2) = ngfpq3(v1(n1-3),v1(n1-2),v1(n1-1),v1(n1),
     *                      2.0d0 + (j-1)*ftocr)
            i2 = i2 + 1
 40      continue
c
         return
c
      end
c
c---------------------------------------------------------------------
c
c     Returns cubically interpolated value at X = X0 + U * H.
c
c---------------------------------------------------------------------
c
      double precision function ngfpq3(f0,f1,f2,f3,u)
c
         real*8      c1, c2, f0, f1, f2, f3, u
         real*8      half,             third
         parameter ( half = 0.5d0,     
     *               third = 0.3333333333333333d0 )
c
         c1 = half * u * (u - 1.0d0)
         c2 = third * c1 * (u - 2.0d0)

         ngfpq3 = f0 * (1.0d0 - u + c1 - c2) +
     *            f1 * (u - 2.0d0 * c1 + 3.0d0 * c2) +
     *            f2 * (c1 - 3.0d0 * c2) +
     *            f3 * c2
c
         return
c
      end
c
c---------------------------------------------------------------------
c
c     Will return index of maximum of convex discrete function V,
c     maximum endpoint of concave, unpredictable otherwise.
c
c---------------------------------------------------------------------
c
      integer function idvmcx(v,n)
c
         real*8      v(1)
         real*8      vm
         integer     l, m, n, om, r
c
         l = 1
         r = n
         m = 0
 10      continue
            om = m
            m = (l + r) / 2
            if( m .eq. 1  .or.  m .eq. n  .or.  m .eq. om ) go to 50
            vm = v(m)
            if( v(m-1) .gt. vm ) go to 30
               if( v(m+1) .gt. vm ) go to 20
                  idvmcx = m
         go to 40
 20            continue
                  l = m
         go to 10
 30         continue
            r = m
         go to 10
c
 40      return
c
 50      continue
         if ( m .eq. n-1 ) m = n
         idvmcx = m
         if( idvmcx .eq. n ) go to 60
            if( v(idvmcx) .lt. v(n) ) idvmcx = n
         go to 70
 60         if( v(idvmcx) .lt. v(1) ) idvmcx = 1
c
 70      return
c
      end
c
c---------------------------------------------------------------------
c
c     Will return index of minimum of concave discrete function V,
c     minimum endpoint of concave, unpredictable otherwise.
c
c---------------------------------------------------------------------
c
      integer function idvmcv(v,n)
c
         real*8      v(1)
         real*8      vm
         integer     l, m, n, om, r
c
         l = 1
         r = n
         m = 0
 10      continue
            om = m
            m = (l + r) / 2
            if( m .eq. 1  .or.  m .eq. n  .or.  m .eq. om ) go to 50
            vm = v(m)
            if( v(m-1) .lt. vm ) go to 30
               if( v(m+1) .lt. vm ) go to 20
                  idvmcv = m
         go to 40
 20            continue
                  l = m
         go to 10
 30         continue
            r = m
         go to 10
c
 40      return
c
 50      continue
         if( m .eq. n-1 ) m = n
         idvmcv = m
         if( idvmcv .eq. n ) go to 60
            if( v(idvmcv) .gt. v(n) ) idvmcv = n
         go to 70
 60         if( v(idvmcv) .gt. v(1) ) idvmcv = 1
c
 70      return
c
      end
c
c---------------------------------------------------------------------
c
c     Returns index of an extrema. Looks for max/min first if
c     OLDFL >< 0. NEWFL indicates type of extrema found.
c
c---------------------------------------------------------------------
c
      integer function idvxtr(v,n,oldfl,newfl)
c
         integer     idvmcv, idvmcx
         logical     pibetw
c
         real*8      v(1)
         integer     newfl, newmax, newmin, oldfl
c
         if( oldfl .lt. 0 ) go to 40
            newmax = idvmcx(v,n)
            if( pibetw(1,newmax,n) ) go to 30
               newmin = idvmcv(v,n)
               if( pibetw(1,newmin,n) ) go to 20
 10               idvxtr = newmax
                  newfl = oldfl
               go to 80
 20               idvxtr = newmin
                  newfl = -oldfl
            go to 80
 30            idvxtr = newmax
               newfl = oldfl
         go to 80
 40         newmin = idvmcv(v,n)
            if( pibetw(1,newmin,n) ) go to 70
               newmax = idvmcx(v,n)
               if( pibetw(1,newmax,n) ) go to 60
 50               idvxtr = newmin
                  newfl = oldfl
               go to 80
 60               idvxtr = newmax
                  newfl = -oldfl
            go to 80
 70            idvxtr = newmin
               newfl = oldfl
 80      continue
c
         return
c
      end
c
c---------------------------------------------------------------------
c
c     Predicate: .TRUE. <---> I1 < I2 < I3
c
c---------------------------------------------------------------------
c
      logical function pibetw(i1,i2,i3)
c
         integer     i1, i2, i3
c
         pibetw = i2 .gt. i1  .and.  i3 .gt. i2
c
         return
c
      end
c
c---------------------------------------------------------------------
c
c     Another vector copy which works from the tail end.
c
c---------------------------------------------------------------------
c
      subroutine dvcopr(v1,v2,n)
c
         real*8      v1(1), v2(1)
         integer     i, n
c
         do 10 i = 1 , n
            v2(n-i+1) = v1(n-i+1)
 10      continue
c
         return
c
      end
c
c---------------------------------------------------------------------
c
c     Vector right and left shifts.
c
c---------------------------------------------------------------------
c
      subroutine dvrsh(v,sh,n)
c
         real*8      v(1)
         integer     n, sh
c
         call dvcopr(v,v(sh+1),n-sh)
c*
         call dvls(v,0.0d0,sh)
c*
c
         return
c
      end
c
c---------------------------------------------------------------------
c
      subroutine dvlsh(v,sh,n)
c
         real*8      v(1)
         integer     n, sh
c
         call dvcopy(v(sh+1),v,n-sh)
c*
         call dvls(v(n-sh+1),0.0d0,sh)
c*
c
         return
c
      end
c
c     Circular shift-by-1.
c
      subroutine dvucrsh(v,n)
c
         real*8      v(*)
         integer     n

         real*8      spill
c
         spill = v(n)
         call dvcopr(v,v(2),n-1)
         v(1) = spill
c
         return
c
      end
c
c---------------------------------------------------------------------
c
      subroutine dvuclsh(v,n)
c
         real*8      v(*)
         integer     n
c
         spill = v(1)
         call dvcopy(v(2),v,n-1)
         v(n) = spill
c
         return
c
      end
c
c---------------------------------------------------------------------
c
c     Replaces all elements in V with absolute value less than TOL
c     with 0.
c
c---------------------------------------------------------------------
c
      subroutine dvnull(v,tol,n)
c
         real*8      v(1)
         real*8      tol

         real*8      zero
         parameter ( zero = 0.0d0 )

         integer     i, n
c
         do 10 i = 1 , n
            if( abs(v(i)) .lt. tol ) v(i) = zero
 10      continue
c
         return
c
      end
c
c---------------------------------------------------------------------
c
c     Returns index of first element of V which is not .EQ. to SC.
c
c---------------------------------------------------------------------
c
      integer function idvfne(v,sc,n)
c
         real*8      v(1)
         real*8      sc
         integer     i, n
c
         idvfne = 0
         do 10 i = 1 , n
            if( v(i) .ne. sc ) go to 20
 10      continue
c
         return
c
 20      idvfne = i
c
         return
c
      end
c
c---------------------------------------------------------------------
c
c     Returns cubically interpolated vector at "time" T = T0 + DT * U.
c
c---------------------------------------------------------------------
c
      subroutine dvciq1(v0,v1,v2,v3,vi,u,n)
c
         real*8      ngfpq3
         real*8      vi(1), v0(1), v1(1), v2(1), v3(1)
         real*8      u
         integer     i, n
c
         do 10 i = 1 , n
            vi(i) = ngfpq3(v0(i),v1(i),v2(i),v3(i),u)
 10      continue
c
         return
c
      end
c
c---------------------------------------------------------------------
c
c     Routines for calculating finite difference approximations
c     to derivatives. 2nd order forward, central, and backward
c     expressions are used.
c
c---------------------------------------------------------------------
c
      subroutine dvd2q1(v,vp,h,n)
c
         real*8         v(*), vp(*)
         real*8         h, hhm1
         integer        i, n, nm1
c
         hhm1 = 0.5d0 / h
         nm1 = n - 1
         vp(1) = hhm1 * (-v(3) + 4.0d0 * v(2) - 3.0d0 * v(1))
         do 10 i = 2 , nm1
            vp(i) = hhm1 * (v(i+1) - v(i-1))
 10      continue
         vp(n) = hhm1 * (3.0d0 * v(n) - 4.0d0 * v(n-1) + v(n-2))
c
         return
c
      end
c
c---------------------------------------------------------------------
c
      subroutine dvd2q2(v,vpp,h,n)
c
         real*8         v(*), vpp(*)
         real*8         h, hm2
         integer        n, nm1
c
         nm1 = n - 1
         hm2 = 1.0d0 / (h * h)
         vpp(1) = hm2 * (-v(4) + 4.0d0 * v(3) - 5.0d0 * v(2) +
     *                    2.0d0 * v(1))
         do 10 i = 2 , nm1
            vpp(i) = hm2 * (v(i+1) - 2.0d0 * v(i) + v(i-1))
 10      continue
         vpp(n) = hm2 * (2.0d0 * v(n) - 5.0d0 * v(n-1) +
     *                   4.0d0 * v(n-2) - v(n-3))
c
         return
c
      end
c
c---------------------------------------------------------------------
c
      subroutine dvd2q3(v,vppp,h,n)
c
         real*8         v(1), vppp(1)
         real*8         h, hhm3
         integer        i, n, nm1, nm2
c
         nm1 = n - 1
         nm2 = n - 2
         hhm3 = 0.5d0 / (h * h * h)
         do 10 i = 1 , 2
            vppp(i) = hhm3 * ( -3.0d0 * v(i+4) + 14.0d0 * v(i+3) -
     *                         24.0d0 * v(i+2) + 18.0d0 * v(i+1) -
     *                          5.0d0 * v(i))
 10      continue
         do 20 i = 3 , nm2
            vppp(i) = hhm3 * ( v(i+2) - v(i-2) +
     *                         2.0d0 * (v(i-1) - v(i+1)))
 20      continue
         do 30 i = nm1 , n
            vppp(i) = hhm3 * ( 5.0d0 * v(i)   - 18.0d0 * v(i-1) +
     *                        24.0d0 * v(i-2) - 14.0d0 * v(i-3) +
     *                         3.0d0 * v(i-4))
 30      continue
c
         return
c
      end
c
c---------------------------------------------------------------------
c
c     "Rounds" a double precision number.
c
c---------------------------------------------------------------------
c
      integer function idroun(x)
c
         real*8 x
c
         idroun = x + 0.5d0
c
         return
c
      end
c
c---------------------------------------------------------------------
c
c     Second centred divided difference for function defined on
c     staggered grid with right hand ghost point and vanishing
c     derivative at right hand boundary.
c
c---------------------------------------------------------------------
c
      subroutine dd02q2(v1,v2,h,n)
c
         real*8      v1(*), v2(*)
         real*8      h, hm2
         integer     j, n, nm1
c
         nm1 = n - 1
         hm2 = 1.0d0 / (h * h)
         do 10 j = 3 , nm1
            v2(j) = hm2 * (v1(j+1) - 2.0d0 * v1(j) + v1(j-1))
 10      continue
         v2(2) = hm2 * (-25.0d0 * v1(2) + 26.0d0 * v1(3) - v1(4))
     *               / 23.0d0
c
         return
c
      end
c
c---------------------------------------------------------------------
c
c     Initializes vector to "noise" function.
c
c---------------------------------------------------------------------
c
      subroutine dnoiz1(v,eps,n)
c
         real*8         v(1)
         real*8         eps
         integer        i, n
c
         v(1) = eps
         do 10 i = 2 , n
            v(i) = -v(i-1)
 10      continue
c
         return
c
      end
c
c---------------------------------------------------------------------
c
c     Extension of DVFAPL which allows another argument to be passed
c     to the externally supplied function.
c
c---------------------------------------------------------------------
c
      subroutine dvfap2(v1,v2,f,arg,n)
c
         external       f
c
         real*8         f
         real*8         v1(1), v2(1)
         real*8         arg
         integer        i, n
c
         do 10 i = 1 , n
            v2(i) = f(v1(i),arg)
 10      continue
c
         return
c
      end
c
c---------------------------------------------------------------------
c
c     DVFAP2(V1,V2,DPOWR,P,N) computes V2 = V1 ** P.
c
c---------------------------------------------------------------------
c
      double precision function dpowr(x,p)
c
         real*8         x, p
c
         if( x .ne. 0.0d0 ) then
            dpowr = x ** p
         else
            if( p .eq. 0.0d0 ) then
               dpowr = 1.0d0
            else
               dpowr = 0.0d0
            end if
         end if
c
         return
c
      end
c
c---------------------------------------------------------------------
c
c     Vector minimum.
c
c---------------------------------------------------------------------
c
      double precision function dvmin(v1,n)
c
         real*8      v1(1)
         integer     i, n
c
         dvmin = v1(1)
         do 10 i = 2 , n
            dvmin = min(dvmin,v1(i))
 10      continue
c
         return
c
      end
c
c---------------------------------------------------------------------
c
c     Vector minimum excluding 0.0d0.
c
c---------------------------------------------------------------------
c
      double precision function dvminnz(v1,n)
c
         real*8      v1(1)
   
         logical     define
         integer     i, n
c
         define = .false.
         do 10 i = 1 , n
            if( v1(i) .ne. 0.0d0 ) then
               if( define ) then
                  dvminnz = min(dvminnz,v1(i))
               else
                  dvminnz = v1(i)
                  define = .true.
               end if
            end if
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
      double precision function dvmax(v1,n)
c
         real*8      v1(1)
         integer     i, n
c
         dvmax = v1(1)
         do 10 i = 2 , n
            dvmax = max(dvmax,v1(i))
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
      integer function idvmin(v,n)
c
         real*8      v(1)
         real*8      vi, vmin
         integer     i ,n
c
         vmin = v(1)
         idvmin = 1
         do 10 i = 2 , n
            vi = v(i)
            if( vi .ge. vmin ) go to 10
               vmin = vi
               idvmin = i
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
      integer function idvmax(v,n)
c
         real*8      v(1)
         real*8      vi, vmax
         integer     i ,n
c
         vmax = v(1)
         idvmax = 1
         do 10 i = 2 , n
            vi = v(i)
            if( vi .le. vmax ) go to 10
               vmax = vi
               idvmax = i
 10      continue
c
         return
c
      end
c
c-----------------------------------------------------------------------
c
c     "l-inifinity" norm of V.
c
c-----------------------------------------------------------------------
c
      double precision function dvlinf(v,n)
c
         real*8     v(1), dabsvi
         integer    n
c
         dvlinf = abs(v(1))
         do 10 i = 2 , n
            dabsvi = abs(v(i))
            if( dabsvi .gt. dvlinf ) dvlinf = dabsvi
 10      continue
c
         return
c
      end
c
c-----------------------------------------------------------------------
c
c     Performs smoothing operation using second divided difference.
c
c-----------------------------------------------------------------------
c
      subroutine dvsmth(v1,v2,n,dx,alpha)
c
         real*8         v1(1), v2(1)
         real*8         alpha, c1, c2, dx
         integer        j, n
c
         c2 = alpha / (dx * dx)
         c1 = 1.0d0 - 2.0d0 * alpha
         v2(1) = v1(1)
         do 10 j = 2 , n-1
            v2(j) = c1 * v1(j) + c2 * (v1(j+1) + v1(j-1))
 10      continue
         v2(n) = v1(n)
c
         return
c
      end
c
c-----------------------------------------------------------------------
c
c     Initializes vector to "Gaussian" profile.
c
c-----------------------------------------------------------------------
c
      subroutine dvgaus(v,x,v0,x0,xwid,n)
c
         real*8       v(1), x(1)
         real*8       v0, xwid, x0
         real*8       arg 
         real*8       argmax 
         parameter  ( argmax = 169.0d0 )
         integer      i, n
c
         do 10 i = 1 , n
            arg = ((x(i) - x0) / xwid) ** 2
            if( arg .lt. argmax ) then
               v(i) = v0 * exp(-arg)
            else
               v(i) = 0.0d0
            end if
 10      continue
c
         return
c
      end
c
c-----------------------------------------------------------------------
c
c     Vector "merge".
c
c-----------------------------------------------------------------------
c
      subroutine dvmrg(v1,v2,vmrg,n)
c
         real*8      v1(1), v2(1), vmrg(1)
         integer     j, n
c
         do 10 j = 1 , n
            vmrg(2*j-1) = v1(j)
            vmrg(2*j) = v2(j)
 10      continue
c
         return
c
      end
c
c-----------------------------------------------------------------------
c
c     Vector "unmerge".
c
c-----------------------------------------------------------------------
c
      subroutine dvumrg(vmrg,v1,v2,n)
c
         real*8      v1(1), v2(1), vmrg(1)
         integer     j, n
c
         do 10 j = 1 , n
            v1(j) = vmrg(2*j-1)
            v2(j) = vmrg(2*j)
 10      continue
c
         return
c
      end
c
c-----------------------------------------------------------------------
c
c     Computes local variation in vector relative to range of values.
c
c-----------------------------------------------------------------------
c
      subroutine dvvarr(v1,v2,n)
c
         intrinsic      dabs
         real*8         dvmax, dvmin
         real*8         v1(1), v2(1)
         real*8         range
         integer        n
c
         range = dvmax(v1,n) - dvmin(v1,n)
         if( range .ne. 0.0d0 ) then
            call dvdd1(v1,v2,1.0d0,n)
            call dvfapl(v2,v2,dabs,n)
            call dvsm(v2,1.0d0/range,v2,n)
         else
            call dvls(v2,0.0d0,n)
         end if
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
      subroutine dvdump(v,n,label,unit)
c
         real*8         v(1)
         character*(*)  label
         integer        i, n, st, unit
c
         if( n .lt. 1 ) go to 130
            write(unit,100) label
 100        FORMAT(/' <<< ',A,' >>>'/)
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
c     History: DVDUMP.
c
c     Dumps vector labelled with LABEL on UNIT. All 16 sig. figs.
c     displayed.
c
c-----------------------------------------------------------------------
c
      subroutine dvdmpx(v,n,label,unit)
c
         real*8         v(1)
         character*(*)  label
         integer        i, n, st, unit
c
         if( n .lt. 1 ) go to 130
            write(unit,100) label
 100        FORMAT(' <<< ',A,' >>>')
C100        FORMAT(//' <<< ',A,' >>>'/)
            st = 1
 110        continue
               write(unit,120) ( v(i) , i = st , min(st+2,n))
 120           FORMAT(' ',3(1PE24.16))
               st = st + 3
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
c     Dumps V2 "as function" of V1 on UNIT.
c
c-----------------------------------------------------------------------
c
      subroutine dvpdmp(v1,v2,n,label,unit)
c
         integer          n,   unit
         real*8           v1(n),   v2(n)
         character*(*)    label
c
         integer          i,    st
c
         if( n .ge. 1 ) then
            write(unit,100) label
 100        FORMAT(/' <<< ',A,' >>>'/)
            st = 1
 110        continue
               if( st + 1 .le. n ) then
                  write(unit,120) ( v1(i), v2(i) , i = st , st + 1)
 120              FORMAT(T2, '(',F10.5,' :',1P,E16.8,')',0P,
     *                   T40,'(',F10.5,' :',1P,E16.8,')',0P)
C120              FORMAT(T2, '(',F10.5,' :',1P,E24.16,')',0P,
C    *                   T50,'(',F10.5,' :',1P,E24.16,')',0P)
                  st = st + 2
               else
                  write(unit,130) v1(n), v2(n)
 130              FORMAT(T2, '(',F10.5,' :',1P,E16.8,')')
C130              FORMAT(T2, '(',F10.5,' :',1P,E24.16,')')
                  st = st + 1
               end if
            if( st .le. n ) go to 110
         end if
c
         return
c
      end
c
c-----------------------------------------------------------------------
c
c     Extension of DVDUMP which does the obvious thing.
c
c-----------------------------------------------------------------------
c
      subroutine dvdmp1(v,w,inc,n,label,unit)
c
         real*8         v(1), w(1)
         character*(*)  label
         integer        inc, n, unit
c
         call dvinj(v,w,inc,n)
         call dvdump(w,1+(n-1)/inc,label,unit)
c
         return
c
      end
c
c-----------------------------------------------------------------------
c
c     Returns index of first element of V equal to, less than or
c     greater than VKEY for CODE = 1, 2 or 3 respectively; returns
c     0 if condition not met.
c
c-----------------------------------------------------------------------
c
      integer function idvscn(v,n,vkey,code)
c
         real*8       v(1)
         real*8       vkey
         integer      code, i, n
c
         go to ( 100, 200, 300 ), code
c
 100     continue
            idvscn = 0
            do 110 i = 1 , n
               if( v(i) .eq. vkey ) then
                  idvscn = i
                  go to 1000
               end if
 110        continue
         go to 1000
c
 200     continue
            idvscn = 0
            do 210 i = 1 , n
               if( v(i) .lt. vkey ) then
                  idvscn = i
                  go to 1000
               end if
 210        continue
         go to 1000
c
 300     continue
            idvscn = 0
            do 310 i = 1 , n
               idvscn = 0
               if( v(i) .gt. vkey ) then
                  idvscn = i
                  go to 1000
               end if
 310        continue
         go to 1000
c
1000     continue
c
         return
c
      end
c
c-----------------------------------------------------------------------
c
c     Quadratically interpolates one set of values from another set.
c
c-----------------------------------------------------------------------
c
      subroutine dvinq3(v,x,vbar,xbar,n,nbar,vs,vf)
c
         integer      n, nbar
         real*8       flip2
         real*8       v(n), vbar(nbar), x(n), xbar(nbar)
         real*8       vf, vs
         integer      j, jbar
c
         jbar = 1
         j = 1
100      continue
         if( jbar .gt. nbar ) go to 200
            if( xbar(jbar) .lt. x(j) ) then
               if( j .eq. 1 ) then
                  vbar(jbar) = vs
               else if( j .eq. 2 ) then
                  vbar(jbar) = flip2(xbar(jbar),x(1),x(2),x(3),
     *                               v(1),v(2),v(3))
               else if( j .eq. n  .or.
     *                  xbar(jbar) .le. 0.5d0 * (x(j) + x(j-1)) ) then
                  vbar(jbar) = flip2(xbar(jbar),x(j-2),x(j-1),x(j),
     *                               v(j-2),v(j-1),v(j))
               else
                  vbar(jbar) = flip2(xbar(jbar),x(j-1),x(j),x(j+1),
     *                               v(j-1),v(j),v(j+1))
               end if
               jbar = jbar + 1
            else if( xbar(jbar) .eq. x(j) ) then
               vbar(jbar) = v(j)
               jbar = jbar + 1
            else
               if( j .eq. n ) then
                  vbar(jbar) = vf
                  jbar = jbar + 1
               else
                  j = j + 1
               end if
            end if
         go to 100
c
200      continue
c
         return
c
      end
c
c-----------------------------------------------------------------------
c
c     Low level routine for quadratic polynomial interpolation.
c
c-----------------------------------------------------------------------
c
      double precision function flip2(x,x0,x1,x2,f0,f1,f2)
c
         real*8       f0, f1, f2, x, x0, x1, x2,
     *                d1, d2, d3, n1, n2, n3
c
         n1 = x - x0
         n2 = x - x1
         n3 = x - x2
         d1 = x0 - x1
         d2 = x0 - x2
         d3 = x1 - x2
         flip2 =  (f0 * n2 * n3) / (d1 * d2)
     *           -(f1 * n1 * n3) / (d1 * d3)
     *           +(f2 * n1 * n2) / (d2 * d3)
c
         return
c
      end
c
c-----------------------------------------------------------------------
c
c     Cubically interpolates one set of values from another set.
c
c-----------------------------------------------------------------------
c
      subroutine dvinq4(v,x,vbar,xbar,n,nbar,vs,vf)
c
         integer      n, nbar
         real*8       flip3
         real*8       v(n), vbar(nbar), x(n), xbar(nbar)
         real*8       vf, vs
         integer      j, jbar
c
         jbar = 1
         j = 1
100      continue
         if( jbar .gt. nbar ) go to 200
            if( xbar(jbar) .lt. x(j) ) then
               if( j .eq. 1 ) then
                  vbar(jbar) = vs
               else if( j .le. 3 ) then
                  vbar(jbar) = flip3(xbar(jbar),x(1),x(2),x(3),x(4),
     *                               v(1),v(2),v(3),v(4))
               else if( j .eq. n  ) then
                  vbar(jbar) = flip3(xbar(jbar),
     *                               x(n-3),x(n-2),x(n-1),x(n),
     *                               v(n-3),v(n-2),v(n-1),v(n))
               else
                  vbar(jbar) = flip3(xbar(jbar),
     *                               x(j-2),x(j-1),x(j),x(j+1),
     *                               v(j-2),v(j-1),v(j),v(j+1))
               end if
               jbar = jbar + 1
            else if( xbar(jbar) .eq. x(j) ) then
               vbar(jbar) = v(j)
               jbar = jbar + 1
            else
               if( j .eq. n ) then
                  vbar(jbar) = vf
                  jbar = jbar + 1
               else
                  j = j + 1
               end if
            end if
         go to 100
c
200      continue
c
         return
c
      end
c
c-----------------------------------------------------------------------
c
c     Low level routine for cubic polynomial interpolation.
c
c-----------------------------------------------------------------------
c
      double precision function flip3(x,x0,x1,x2,x3,f0,f1,f2,f3)
c
         real*8       f0, f1, f2, f3, x, x0, x1, x2, x3,
     *                d1, d2, d3, d4, d5, d6, n1, n2, n3, n4
c
         n1 = x - x0
         n2 = x - x1
         n3 = x - x2
         n4 = x - x3
         d1 = x0 - x1
         d2 = x0 - x2
         d3 = x0 - x3
         d4 = x1 - x2
         d5 = x1 - x3
         d6 = x2 - x3
         flip3 =  (f0 * n2 * n3 * n4) / (d1 * d2 * d3)
     *          - (f1 * n1 * n3 * n4) / (d1 * d4 * d5)
     *          + (f2 * n1 * n2 * n4) / (d2 * d4 * d6)
     *          - (f3 * n1 * n2 * n3) / (d3 * d5 * d6)
c
         return
c
      end
c
c-----------------------------------------------------------------------
c
c     (NINTRP-1)th degree interpolation from one set of values to another
c     (Both sets must be in ascending order.)
c
c-----------------------------------------------------------------------
c
      subroutine dvinqn(v,x,vbar,xbar,n,nbar,vs,vf,nintrp)
c
         implicit     logical (a-z)
c
         real*8       flipn
         integer      llogic
         logical      leven
c
         integer      n, nbar, nintrp
         real*8       v(n), vbar(nbar), x(n), xbar(nbar)
c
         real*8       vs, vf
         integer      i, j, jbar

         logical      ltrace
         parameter  ( ltrace = .false. )
c
         if( ltrace ) then
            WRITE(*,*) '>>> dvinqn:: n',n
            CALL DVPDMP(X,V,N,'V(X)',6)
         end if
c
         jbar = 1
         j = 1
 100     continue
         if( jbar .gt. nbar ) go to 200
            if( xbar(jbar) .lt. x(j) ) then
               if( j .eq. 1 ) then
                  vbar(jbar) = vs
               else
                  if( leven(nintrp) ) then
                     i = min(n-nintrp+1,max(1,j-(nintrp/2)))
                  else
                     i = min(n-nintrp+1,max(1,j-(nintrp+1)/2 +
     *                       llogic(xbar(jbar) .gt.
     *                             0.5d0 * (x(j) + x(j-1)))))
                  end if
                  vbar(jbar) = flipn(xbar(jbar),x(i),v(i),nintrp)
               end if
               jbar = jbar + 1
            else if( xbar(jbar) .eq. x(j) ) then
               vbar(jbar) = v(j)
               jbar = jbar + 1
            else
               if( j .eq. n ) then
                  vbar(jbar) = vf
                  jbar = jbar + 1
               else
                  j = j + 1
               end if
            end if
         go to 100
c
 200     continue
c
         if( ltrace ) then
            WRITE(*,*) '>>> dvinqn:: nbar',nbar
            CALL DVPDMP(XBAR,VBAR,NBAR,'VBAR(XBAR)',6)
         end if

         return
c
      end
c
c-----------------------------------------------------------------------
c
c     Local version of EVEN to stop Loader messages.
c
c-----------------------------------------------------------------------
c
      logical function leven(n)
c
         integer      n
c
         leven = mod(n,2) .eq. 0
c
         return
c
      end
c
c-----------------------------------------------------------------------
c 
c     Periodic interpolation ... same calling sequence as dvinqn
c     without default boundary values vs and vf.
c
c     Input/output vectors in standard pbc representation.
c
c     Output on naively selected power of 2 mesh.
c
c-----------------------------------------------------------------------
c
      subroutine dvinq2n(v,x,vbar,xbar,n,nbar,nintrp)

         implicit    none

         integer     nnpbc  
         logical     haspbc 

         real*8      v(*),     x(*),     vbar(*),     xbar(*)
         integer     n,        nbar,     nintrp

         real*8      dx,       dxbar,    xmax

         logical     ltrace
         parameter ( ltrace = .true. )

         if( haspbc(v,n) .and. haspbc(x,n) ) then
            nbar =  nnpbc(n)
            if( nbar .lt. 2 ) then
               call dvcopy(x,xbar,nbar)
               call dvcopy(v,vbar,nbar)
            else 
               dx = (x(n) - x(2)) / (n - 2)
               xmax = x(n) + dx
               x(n+1) = xmax
               dxbar = (xmax - x(2)) / (nbar - 1)
               call dvramp(xbar(2),x(2),dxbar,nbar)
               call dvinqn(v(2),x(2),vbar(2),xbar(2),n,nbar,
     *                     v(2),v(n+1),nintrp)
               call dvpbc(x,n)
               call dvpbc(xbar,nbar)
            end if
         else 
            if( ltrace ) then
               write(*,*) '>>> dvinq2n:: Input vector(s) not in '//
     *                    'standard pbc form ...'
            end if
         end if

         return

      end

c-----------------------------------------------------------------------
c
c     "Converts" FORTRAN Boolean to {0,1} Boolean.
c
c     Local version to get rid of loader messages.
c
c-----------------------------------------------------------------------
c
      integer function llogic(tvalue)
         logical     tvalue
         if( tvalue ) then
            llogic = 1
         else
            llogic = 0
         end if
         return
      end
c
c-----------------------------------------------------------------------
c
c     Low level routine for (N-1)th (N > 1) order polynomial interp-
C     olation. Straightforward implementation of Neville's algortihm.
c     Should only be used for reasonably well-conditioned problems.
c
c-----------------------------------------------------------------------
c
      double precision function flipn(xbar,x,y,n)
c
         implicit     logical (a-z)
c
         integer      nmax
         parameter    ( nmax = 20 )
         real*8       p(nmax)
c
         integer      n
         real*8       x(n), y(n)
         real*8       xbar
c
         integer      j, l
c
         do 10 l = 1 , n
            p(l) = y(l)
 10      continue
         do 30 l = 1 , n-1
            do 20 j = 1 , n-l
               p(j) = ((xbar - x(j+l)) * p(j) +
     *                 (x(j) - xbar)   * p(j+1)) / (x(j) - x(j+l))
 20         continue
 30      continue
         flipn = p(1)
c
         return
c
      end
c
c-----------------------------------------------------------------------
c
c     Performs APL "compression" function given "Boolean" INTEGER array
c     SELECT.
c
c-----------------------------------------------------------------------
c
      subroutine dvicps(v1,select,v2,n1,n2)
c
         integer      n1, n2
         real*8       v1(n1), v2(n1)
         integer      select(n1)
         integer      i
c
         n2 = 0
         do 10 i = 1 , n1
            if( select(i) .ne. 0 ) then
               n2 = n2 + 1
               v2(n2) = v1(i)
            end if
 10      continue
c
         return
c
      end
c
c-----------------------------------------------------------------------
c
c     Performs APL "compression" function given "Boolean" LOGICAL array
c     SELECT.
c
c-----------------------------------------------------------------------
c
      subroutine dvlcps(v1,select,v2,n1,n2)
c
         integer      n1, n2
         real*8       v1(n1), v2(n1)
         logical*1    select(n1)
         integer      i
c
         n2 = 0
         do 10 i = 1 , n1
            if( select(i) ) then
               n2 = n2 + 1
               v2(n2) = v1(i)
            end if
 10      continue
c
         return
c
      end
c
c-----------------------------------------------------------------------
c
c     Vector-vector indirect (indexed) copy.
c
c-----------------------------------------------------------------------
c
      subroutine dvcpyi(v1,ind,v2,n)
c
         integer      n
         real*8       v1(1), v2(n)
         integer      ind(n)
         integer       i
c
         do 10 i = 1 , n
            v2(i) = v1(ind(i))
 10      continue
c
         return
c
      end
c
c-----------------------------------------------------------------------
c
c     Applies Richardson extrapolation procedure to sequence of
c     approximations in V. V(1) contains final approximation on
c     return. V(1) - V(2) can be used as an error estimate.
c
c-----------------------------------------------------------------------
c
      subroutine dvrex(v,nl,rho)
c
         real*8       v(1)
         real*8       rho
         integer      l, k, nl
c
         do 20 l = 1 , nl-1
            do 10 k = 1 , nl-l
               v(k) = (rho ** (2 * l) * v(k+1) - v(k)) /
     *                (rho ** (2 * l) - 1.0d0)
 10         continue
 20      continue
c
         return
c
      end
c
c-----------------------------------------------------------------------
c
c     V3 = sqrt( v1 ** 2 + v2 ** 2 ).
c
c-----------------------------------------------------------------------
c
      subroutine dvpyth(v1,v2,v3,n)
c
         intrinsic    dsqrt
         real*8       v1(1), v2(1), v3(1)
         integer      n
c
         call dvvm(v1,v1,v3,n)
         call dvavvm(v3,v2,v2,v3,n)
         call dvfapl(v3,v3,dsqrt,n)
c
         return
c
      end

c-----------------------------------------------------------------------
c
c     V3 = v1 ** 2 + v2 ** 2.
c
c-----------------------------------------------------------------------
c
      subroutine dvsofs(v1,v2,v3,n)
c
         real*8       v1(1), v2(1), v3(1)
         integer      n
c
         call dvvm(v1,v1,v3,n)
         call dvavvm(v3,v2,v2,v3,n)
c
         return
c
      end

c-----------------------------------------------------------------------
c
c     V3 = v1 ** 2 - v2 ** 2.
c
c-----------------------------------------------------------------------
c
      subroutine dvdofs(v1,v2,v3,n)
c
         real*8       v1(1), v2(1), v3(1)
         integer      n
c
         call dvvm(v1,v1,v3,n)
         call dvsvvm(v3,v2,v2,v3,n)
c
         return
c
      end
c
c-----------------------------------------------------------------------
c
c     V4 = V1 + V2 * V3.
c
c-----------------------------------------------------------------------
c
      subroutine dvavvm(v1,v2,v3,v4,n)
c
         real*8     v1(1), v2(1), v3(1), v4(1)
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
c
c     V4 = V1 - V2 * V3.
c
c-----------------------------------------------------------------------
c
      subroutine dvsvvm(v1,v2,v3,v4,n)
c
         real*8     v1(1), v2(1), v3(1), v4(1)
         integer    i, n
c
         do 10 i = 1 , n
            v4(i) = v1(i) - v2(i) * v3(i)
 10      continue
c
         return
c
      end
c
c-----------------------------------------------------------------------
c
c     Sort of thing that should be expanded in-lie. performs various
c     types of averaging.
c
c-----------------------------------------------------------------------
c
      double precision function mu(v,code)
c
         real*8      v(2)
         integer     j, code
c
         if( code .eq. 0 ) then
            mu = 0.5d0 * (v(1) + v(2))
         else if ( code .eq. 1 ) then
            mu = 0.625d0 * v(1) + 0.375d0 * v(2)
         end if
c
         return
c
      end
c
c-----------------------------------------------------------------------
c
c     V2(I) := if V1(I) .ge. SC then 1.0 else 0.0
c
c     Modified to use absolute value ...
c
c-----------------------------------------------------------------------
c
      subroutine dvgesc(v1,v2,sc,n)
c
         integer      n
         real*8       v1(n), v2(n)
         real*8       sc
         integer      i
c
         do 10 i = 1 , n
            if( abs(v1(i)) .ge. sc ) then
                v2(i) = 1.0d0
            else
                v2(i) = 0.0d0
            end if
 10      continue
c
         return
c
      end
c
c-----------------------------------------------------------------------
c
c     Evaluates Chebyshev polynomials of first kind (Tn(x)), up
c     MAXORDth ORDER.
c
c-----------------------------------------------------------------------
c
      subroutine dvchtn(v,x,n,order)
c
         implicit     logical*1 (a-z)
c
         integer      n, order
         real*8       x(n), v(n)
c
         integer      j, m
c
         integer      maxord
         parameter    ( maxord = 8 )
c
         real*8       c( 0:maxord , 0:maxord )
         data         c
     *        /
     *             1,                                            8*0,
     *             0,    1,                                      7*0,
     *            -1,    0,    2,                                6*0,
     *             0,   -3,    0,    4,                          5*0,
     *             1,    0,   -8,    0,    8,                    4*0,
     *             0,    5,    0,  -20,    0,   16,              3*0,
     *            -1,    0,   18,    0,  -48,    0,   32,        2*0,
     *             0,   -7,    0,   56,    0, -112,    0,   64,  1*0,
     *             1,    0,  -32,    0,  160,    0, -256,    0,  128
     *        /
c
         if( 1 .le. order  .and.  order .le. maxord ) then
            do 200 j = 1 , n
               v(j) = c(order,order)
               do 100 m = order - 1 , 0 , -1
                  v(j)  = x(j) * v(j) + c(m,order)
 100           continue
 200        continue
         else if (order .eq. 0 ) then
            call dvls(v,c(order,order),n)
         end if
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
      subroutine dvsngl(dv,sv,n)
c
         implicit     logical*1 (a-z)
c
         integer      n
         real*8       dv(n)
         real*4       sv(n)
c
         integer      i
c
         do 10 i = 1 , n
            sv(i) = dv(i)
 10      continue
c
         return
c
      end

c-----------------------------------------------------------------------
c
c     Vector single to double conversion.
c
c-----------------------------------------------------------------------
c
      subroutine dvdoub(sv,dv,n)
c
         implicit     logical*1 (a-z)
c
         integer      n
         real*8       dv(n)
         real*4       sv(n)
c
         integer      i
c
         do 10 i = 1 , n
            dv(i) = sv(i)
 10      continue
c
         return
c
      end
c
c-----------------------------------------------------------------------
c
c     Returns IRHO : 1 cubically interpolated refinement of V1 in V2.
c
c-----------------------------------------------------------------------
c
      subroutine dvi4q1(v1,v2,n1,irho)
c
         implicit     logical*1 (a-z)
c
         integer      irho, n1
         real*8       v1(n1),     v2(1)
c
         real*8       c0, c1, c2, c3, rhom1, sigma
         integer      i, j
c
         real*8       half,     mhalf,     sixth,     msixth
         parameter  ( half   =  0.5000 0000 0000 0000 d0,  
     *                mhalf  = -0.5000 0000 0000 0000 d0,  
     *                sixth  =  0.1666 6666 6666 6667 d0,  
     *                msixth = -0.1666 6666 6666 6667 d0  )
c
         rhom1 = 1.0d0 / irho
c
         do 10 i = 1 , n1
            v2(irho * (i - 1) + 1) = v1(i)
 10      continue
c
         do 20 j = 1 , irho - 1
            sigma = rhom1 * j
            c0 = msixth * (sigma - 1.0d0) * (sigma - 2.0d0) *
     *                    (sigma - 3.0d0)
            c1 = half   *  sigma          * (sigma - 2.0d0) *
     *                    (sigma - 3.0d0)
            c2 = mhalf  *  sigma          * (sigma - 1.0d0) *
     *                    (sigma - 3.0d0)
            c3 = sixth  *  sigma          * (sigma - 1.0d0) *
     *                    (sigma - 2.0d0)
            v2(j+1) = c0 * v1(1) + c1 * v1(2) + c2 * v1(3) + c3 * v1(4)
 20      continue
c
         do 50 j = 1 , irho - 1
            sigma = 1.0d0 + rhom1 * j
            c0 = msixth * (sigma - 1.0d0) * (sigma - 2.0d0) *
     *                    (sigma - 3.0d0)
            c1 = half   *  sigma          * (sigma - 2.0d0) *
     *                    (sigma - 3.0d0)
            c2 = mhalf  *  sigma          * (sigma - 1.0d0) *
     *                    (sigma - 3.0d0)
            c3 = sixth  *  sigma          * (sigma - 1.0d0) *
     *                    (sigma - 2.0d0)
            do 30 i = 1 , n1 - 3
               v2(irho * i + j + 1) =  c0 * v1(i  ) + c1 * v1(i+1) +
     *                                 c2 * v1(i+2) + c3 * v1(i+3)
 30         continue
 50      continue
c
         do 60 j = 1 , irho - 1
            sigma = 2.0d0 + rhom1 * j
            c0 = msixth * (sigma - 1.0d0) * (sigma - 2.0d0) *
     *                    (sigma - 3.0d0)
            c1 = half   *  sigma          * (sigma - 2.0d0) *
     *                    (sigma - 3.0d0)
            c2 = mhalf  *  sigma          * (sigma - 1.0d0) *
     *                    (sigma - 3.0d0)
            c3 = sixth  *  sigma          * (sigma - 1.0d0) *
     *                    (sigma - 2.0d0)
            v2(irho * (n1 - 2) + j+1) = c0 * v1(n1-3) + c1 * v1(n1-2) +
     *                                  c2 * v1(n1-1) + c3 * v1(n1  )
 60      continue
c
         return
c
      end
c
c-----------------------------------------------------------------------
c
c     Vector cubic interpolation.
c
c-----------------------------------------------------------------------
c
      subroutine dvi4qv(f0,f1,f2,f3,f,n,sig)
c
         implicit       logical*1 (a-z)
c
         integer        n
         real*8         f(n),     f0(n),    f1(n),    f2(n),    f3(n)
         real*8         sig
c
         real*8         c0,  c1,  c2,  c3
         integer        j
c
         c0 = -(sig - 1.0d0) * (sig - 2.0d0) * (sig - 3.0d0) / 6.0d0
         c1 =  0.5d0 * sig * (sig - 2.0d0) * (sig - 3.0d0)
         c2 = -0.5d0 * sig * (sig - 1.0d0) * (sig - 3.0d0)
         c3 = sig * (sig - 1.0d0) * (sig - 2.0d0) / 6.0d0
         do 10 j = 1 , n
            f(j) = c0 * f0(j) + c1 * f1(j) + c2 * f2(j) + c3 * f3(j)
 10      continue
c
         return
c
      end
c
c-----------------------------------------------------------------------
c
c     Unformatted vector write.
c
c-----------------------------------------------------------------------
c
      subroutine dvoutu(v,n,unit)
c
         integer      n,   unit
         real*8       v(n)
c
         integer      i
c
         write(unit) ( v(i) , i = 1 , n )
c
         return
c
      end
c
c-----------------------------------------------------------------------
c
c     Unformatted vector read.
c
c-----------------------------------------------------------------------
c
      subroutine dvinu(v,n,unit)
c
         integer      n,   unit
         real*8       v(n)
c
         integer      i
c
         read(unit) ( v(i) , i = 1 , n )
c
         return
c
      end
c
c-----------------------------------------------------------------------
c
c     Transfers X(j), Y(j) such that xmin <= X(j) <= xmax to
c     XTO, YTO.
c
c-----------------------------------------------------------------------
c
      subroutine dvrng(x,y,n,xto,yto,nto,xmin,xmax)
c
         implicit     logical*1 (a-z)
c
         integer      n,       nto
         real*8       x(1),    y(1),
     *                xto(1),  yto(1)
         real*8       xmin,    xmax
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
               call dvcopy(x(jst),xto,nto)
               call dvcopy(y(jst),yto,nto)
            end if
c
         else
            nto = 0
         end if

         return

      end
c
c-----------------------------------------------------------------------
c
c     Same as DVRNG except transfer is to original array (destructive).
c
c-----------------------------------------------------------------------
c
      subroutine dvorng(x,y,n,nto,xmin,xmax)
c
         implicit     logical*1 (a-z)
c
         integer      n,       nto
         real*8       x(1),    y(1)
         real*8       xmin,    xmax
c
         call dvrng(x,y,n,x,y,nto,xmin,xmax)
c
         return
c
      end
c
c-----------------------------------------------------------------------
c
c     Given F as representation of piecewise linear function on
c     coordinates X, returns 2nd order approximation of
c     arclength(j) / dx(j) - 1.
c
c-----------------------------------------------------------------------
c
      subroutine dvarc1(ds,f,x,n)
c
         implicit       logical*1 (a-z)
c
         integer        n
         real*8         ds(n),    f(n),     x(n)
c
         integer        j
c
         do 10 j = 2 , n
            ds(j) = sqrt( (x(j) - x(j-1)) ** 2 +
     *                     (f(j) - f(j-1)) ** 2 ) /
     *              (x(j) - x(j-1)) - 1.0d0
 10      continue
         do 20 j = 2 , n - 1
            ds(j) = 0.5d0 * (ds(j) + ds(j+1))
 20      continue
         ds(1) = max( 2.0d0 * ds(2)   - ds(3)   , 0.0d0 )
         ds(n) = max( 2.0d0 * ds(n-1) - ds(n-2) , 0.0d0 )
c
         return
c
      end
c
c-----------------------------------------------------------------------
c
c     Initializes vector to "General Gaussian PRofile"
c
c     X**SCALE * V0 * EXP(- ((X - X0) / XWID)**P)
c
c-----------------------------------------------------------------------
c
      subroutine dvggpr(v,x,v0,x0,xwid,p,scale,n)
c
         implicit       logical*1 (a-z)
c
         real*8         v(1),     x(1)
         real*8         v0,       xwid,     x0
         integer        p,        scale
         real*8         arg
         real*8         argmax 
         parameter    ( argmax = 169.0d0 )
         integer        i,        n
c
         do 10 i = 1 , n
            arg = ((x(i) - x0) / xwid) ** p
            if( arg .lt. argmax ) then
               v(i) = v0 * exp(-arg)
            else
               v(i) = 0.0d0
            end if
            if( scale .ne. 0 ) then
               v(i) = v(i) * x(i) ** scale
            end if
 10      continue
c
         return
c
      end
c
c-----------------------------------------------------------------------
c
c     Builds simple nested grid structure.
c
c-----------------------------------------------------------------------
c
      subroutine dvgrq1(v,n,xmin,xmax,reffct,ncomp)
c
         implicit       logical*1 (a-z)
c
         integer        lnintd
c
         integer        n,        ncomp
         real*8         v(1),
     *                  reffct(ncomp),
     *                  xmax(ncomp),        xmin(ncomp)
c
         integer        mxcomp
         parameter    ( mxcomp = 100 )
c
         real*8         dx(mxcomp)
         integer        ln(mxcomp),
     *                  post(mxcomp),       pre(mxcomp)
c
         integer        i,        lncomp
c
         lncomp = min(ncomp,mxcomp)
c
         n = 0
         do 10 i = 1 , lncomp
            if( xmax(i) .lt. xmin(i) ) return
            if( reffct(i) .le. 0.0d0 ) return
            if( i .lt. lncomp ) then
               if( xmax(i) .lt. xmax(i+1)  .or.
     *             xmin(i) .gt. xmin(i+1)       ) return
            end if
 10      continue
c
         ln(1) = lnintd((xmax(1) - xmin(1)) / reffct(1)) + 1
         dx(1) = (xmax(1) - xmin(1)) / (ln(1) - 1)
c
         do 20 i = 1 , lncomp - 1
            pre(i)    = lnintd((xmin(i+1) - xmin(i)) / dx(i)) + 1
            post(i)   = ln(i) -
     *                  (lnintd((xmax(i+1) - xmin(i)) / dx(i)) + 1)
            dx(i+1)   = dx(i) / int(reffct(i+1))
            xmin(i+1) = xmin(i) + (pre(i) - 1) * dx(i) + dx(i+1)
            ln(i+1)   = (ln(i) - (pre(i) + post(i))) *
     *                  int(reffct(i+1))
            xmax(i+1) = xmin(i+1) + dx(i+1) * (ln(i) - 1)
 20      continue
         pre(lncomp)  = ln(lncomp)
         post(lncomp) = 0
c
         n = 1
         do 30 i = 1 , lncomp
            call dvramp(v(n),xmin(i),dx(i),pre(i))
            n = n + pre(i)
 30      continue
         do 40 i = lncomp - 1 , 1 , -1
            call dvramp(v(n-1),v(n-1),dx(i),post(i)+1)
            n = n + post(i)
 40      continue
         n = n - 1
c
         return
c
      end
c
c-----------------------------------------------------------------------
c
c     Nearest integer (positive, rounds up).
c
c-----------------------------------------------------------------------
c
      integer function lnintd(d)
c
         real*8     d
c
         lnintd = d
         if( (d - lnintd) .ge. 0.5d0 ) then
            lnintd = lnintd + 1
         end if
c
         return
c
      end
c-----------------------------------------------------------------------
c
c     Double precision "entry" to above.
c
c-----------------------------------------------------------------------

      double precision function dnintd(d)
c
         integer   lnintd

         real*8    d
c
         dnintd = lnintd(d)

         return
c
      end
c
c     More local versions of routines in utilmath ...
c

      double precision function ldlog2(x)

         implicit        none

         real*8          x

         real*8          ln2m1
         parameter     ( ln2m1 = 1.442695040888963d0 )

         ldlog2 = ln2m1 * log(x)

         return

      end

      integer function lilog2(x)

         implicit        none

         real*8          ldlog2
         integer         lnintd

         integer         x

         lilog2 = lnintd(ldlog2(1.0d0 * x))

         return

      end
c
c     Predicate: n = 2^p + 1
c
      logical function lp2np1(n)

         implicit        none

         real*8          ldlog2
         integer         lnintd

         integer         n

         lp2np1 = n .eq. (2 ** lnintd(ldlog2(1.0d0 * (n - 1))) + 1)

         return

      end
c
c     Predicate: n = 2^p 
c
      logical function lp2n(n)

         implicit        none

         real*8          ldlog2
         integer         lnintd

         integer         n

         lp2n = n .eq. (2 ** lnintd(ldlog2(1.0d0 * n)))

         return

      end
c
c-----------------------------------------------------------------------
c
c     Vector generalized mean with floor.
c
c     V2(j) := max(FLOOR,mean(V1(j-P),...V1(j),...V1(j+P)))
c
c-----------------------------------------------------------------------
c
      subroutine dvglmf(v1,v2,n,p,floor)
c
         implicit       logical*1 (a-z)
c
         real*8         dvmax
c
         integer        n
         real*8         v1(n),        v2(n)
         real*8         floor
         integer        p
c
         real*8         itwop1
         integer        i,            j
c
         itwop1 = 1.0d0 / (2 * p + 1)
         if( n .ge. (2 * p + 1) ) then
            call dvls(v2,0.0d0,n)
            do 20 i = p + 1 , n - p
               do 10 j = -p , p
                  v2(i) = v2(i) + v1(i+j)
 10            continue
 20         continue
            do 30 i = p + 1 , n - p
               v2(i) = max(floor,v2(i) * itwop1)
 30         continue
            do 40 i = 1 , p
               v2(i) = v2(p+1)
 40         continue
            do 50 i = n - p + 1 , n
               v2(i) = v2(n-p)
 50         continue
         else
            call dvls(v2,max(floor,dvmax(v1,n)),n)
         end if
c
         return
c
      end
c
c-----------------------------------------------------------------------
c
c     Creates a test non-uniform grid.
c
c-----------------------------------------------------------------------

      subroutine dvtnug(v,n,v0,dv,ddv,p)

         implicit       logical*1 (a-z)

         integer        n
         real*8         v(n)

         real*8         v0,       dv,       ddv
         integer        p

         integer        ip,       iq,       nextj,    q,        rem

         q = (n - 1) / (2 * p + 1)
         rem = (n - 1) - q * (2 * p + 1)
         if( rem .ne. 0 ) then
            q = q + 1
         end if

         v(1) = v0
         nextj = 2
         do 20 iq = 1 , q
            do 10 ip = -p , p
               if( nextj .gt. n ) then
                  return
               end if
               v(nextj) = v(nextj-1) + dv + ip * ddv
               nextj = nextj + 1
 10         continue
 20      continue

         return

         end
c
c-----------------------------------------------------------------------
c
c     O(h**2) approximation to dv/dx with nonuniform mesh.
c
c-----------------------------------------------------------------------

      subroutine dvd1nu(v,x,dvdx,n)

         implicit       logical*1 (a-z)

         integer        n
         real*8         x(n),     v(n),     dvdx(n)

         real*8         hm,       hp,       ihhbar,   rho,
     *                  al,       be,       ga
         integer        j

c
c        Left end.
c
         hm = x(2) - x(1)
         hp = x(3) - x(2)
         rho = hp / hm
         ihhbar = 1.0d0 / (hm + hp)
         al = -ihhbar * (hp + 2.0d0 * hm) / hm
         be = 1.0d0 / (ihhbar * hm * hp)
         ga = -ihhbar / rho

         dvdx(1) = al * v(1) + be * v(2) + ga * v(3)

         hp = x(2) - x(1)
         do 10 j = 2 , n - 1
            hm = hp
            hp = x(j+1) - x(j)
            rho = hp / hm
            ihhbar = 1.0d0 / (hm + hp)

            al = -ihhbar * rho
            be =  ihhbar * (hp * hp - hm * hm) / (hp * hm)
            ga =  ihhbar / rho

            dvdx(j) = al * v(j-1) + be * v(j) + ga * v(j+1)
 10      continue
c
c        Right end.
c
         hm = x(n-1) - x(n-2)
         hp = x(n) - x(n-1)
         rho = hp / hm
         ihhbar = 1.0d0 / (hm + hp)
         al = ihhbar * rho
         be = -1.0d0 / (ihhbar * hm * hp)
         ga = ihhbar * (2.0d0 * hp + hm) / hp

         dvdx(n) = al * v(n-2) + be * v(n-1) + ga * v(n)

         return

      end
c
c-----------------------------------------------------------------------
c
c     Computes centred difference approx. of d[LCQ]/dr. Assumes
c     UCQ defined by
c
c     UCQ ** UCN = (LCQ ** LCN) / X
c
c     is smoother than LCQ. Derivative of ln[X] must be supplied
c     as well as storage for UCQ.
c
c-----------------------------------------------------------------------

      subroutine dvd0t0(dlcq,lcq,x,dlnx,ucq,r,nr,lcn,ucn)

         implicit       logical*1 (a-z)

         integer        nr,            lcn,           ucn
         real*8         dlcq(nr),      lcq(nr),       x(nr),
     *                  dlnx(nr),      ucq(nr),       r(nr)

         integer        j
         real*8         lcnpm1

         lcnpm1 = 1.0d0 / lcn

         do 10 j = 1 , nr
            ucq(j) = (lcq(j) ** lcn / x(j)) ** (1.0d0 / ucn)
 10      continue

         do 20 j = 2 , nr - 1
            dlcq(j) = lcq(j) * lcnpm1 *
     *                ( (ucn / ucq(j)) *
     *                  ((ucq(j+1) - ucq(j-1)) / (r(j+1) - r(j-1)))
     *                  + dlnx(j) )
 20      continue

         return

      end
c
c-----------------------------------------------------------------------
c
c     Computes centred difference approx. of d[LCQ0]/dr on *half*
c     lattice (first divided difference). Same assumptions as
c     DVD0T0 but requires additional stuff to be supplied.
c
c-----------------------------------------------------------------------

      subroutine dvddt1(dlcqh,lcq0,x0,xh,dlnxh,lcqh,ucq0,ucqh,r0,
     *                  nr,lcn,ucn)

         implicit       logical*1 (a-z)

         integer        nr,            lcn,           ucn
         real*8         dlcqh(nr),     lcq0(nr),      x0(nr),
     *                  xh(nr),        dlnxh(nr),
     *                  lcqh(nr),      ucq0(nr),      ucqh(nr),
     *                  r0(nr)

         integer        j,             lcnm1,         ucnm1
         real*8         lcnpm1,        ucnpm1,
     *                  t1den,         t1num,         t1

         lcnm1 = lcn - 1
         ucnm1 = ucn - 1
         lcnpm1 = 1.0d0 / lcn
         ucnpm1 = 1.0d0 / ucn

         do 10 j = 1 , nr
            if(      lcn .eq. 1 ) then
               ucq0(j) = lcq0(j) / x0(j)
            else if( lcn .eq. 2 ) then
               ucq0(j) = (lcq0(j) ** 2) / x0(j)
            else
               ucq0(j) = (lcq0(j) ** lcn) / x0(j)
            end if
 10      continue
         call dvav(ucq0,ucqh,nr)
         do 20 j = 1 , nr - 1
            if(      lcn .eq. 1 ) then
               lcqh(j) = xh(j) * ucqh(j)
            else if( lcn .eq. 2 ) then
               lcqh(j) = sqrt(xh(j) * ucqh(j)) * abs(lcq0(j)) / lcq0(j)
            else
               lcqh(j) = (xh(j) * ucqh(j)) ** lcnpm1
            end if
 20      continue
         do 30 j = 1 , nr
            if(       ucn .eq. 2 ) then
               ucq0(j) = sqrt(ucq0(j))
            else if ( ucn .ge. 3 ) then
               ucq0(j) = ucq0(j) ** ucnpm1
            end if
 30      continue
         do 40 j = 1 , nr - 1
            if(       ucn .eq. 2 ) then
               ucqh(j) = sqrt(ucqh(j))
            else if ( ucn .ge. 3 ) then
               ucqh(j) = ucqh(j) ** ucnpm1
            end if
 40      continue

         do 100 j = 1 , nr - 1
            if( lcn .eq. 1 ) then
               t1den = 1.0d0
            else
               t1den = lcn * lcqh(j) ** lcnm1
            end if
            if( ucn .eq. 1 ) then
               t1num = xh(j)
            else
               t1num = ucn * xh(j) * ucqh(j) ** ucnm1
            end if
            t1 = (t1num * (ucq0(j+1) - ucq0(j))) /
     *           (t1den * (r0  (j+1) - r0(j)))
            dlcqh(j) = t1 + lcnpm1 * lcqh(j) * dlnxh(j)
 100     continue

         return

      end
c
c-----------------------------------------------------------------------
c
c     Vector averaging with respect to smoothing function X and power
c     n. Should only be called with n = 1 or 2 currently, and best
c     for definite functions, Divide by 0.0D0 possibility in DO 20
c     so as not to inhibit vectorization.
c
c-----------------------------------------------------------------------

      subroutine dvavt0(q0,qh,x0,xh,nr,n)

         implicit       logical*1 (a-z)

         integer        n,        nr
         real*8         q0(nr),   qh(nr),   x0(nr),   xh(nr)

         integer        j
         if      ( n .eq. 1 ) then
            do 10 j = 1 , nr - 1
               qh(j) = 0.5d0 * xh(j) *
     *                 ( q0(j+1) / x0(j+1) + q0(j) / x0(j) )
 10         continue
         else if ( n .eq. 2 ) then
            do 20 j = 1 , nr - 1
               qh(j) = ( abs(q0(j)) / q0(j) )  *
     *                 sqrt(0.5d0 * xh(j) *
     *                 ( q0(j+1) ** 2 / x0(j+1) + q0(j) ** 2 / x0(j) ))
20          continue
         end if

         return

      end
c
c---------------------------------------------------------------------
c
c     Routines for calculating 2nd order f.d. approx. of first
c     derivative. End formulae give leading order t.e. behavior
c     of central expressions.
c
c---------------------------------------------------------------------
c
      subroutine dvd22 (v,vp,h,n)
c
         integer        n
         real*8         v(n), vp(n)
         real*8         h, hhm1
         integer        i
c
         hhm1 = 0.5d0 / h
         vp(1) = hhm1 * ( -4.0d0 * v(1)     +   7.0d0 * v(2)
     *                    -4.0d0 * v(3)     +           v(4))
         do 10 i = 2 , n - 1
            vp(i) = hhm1 * (v(i+1) - v(i-1))
 10      continue
         vp(n) = hhm1 * ( -        v(n-3)   +   4.0d0 * v(n-2)
     *                    -7.0d0 * v(n-1)   +   4.0d0 * v(n))
c
         return
c
      end
c
c-----------------------------------------------------------------------
c
c     Routine for calculating 2nd order f.d. approximation of second
c     derivative. End formulae give leading order t.e. behavior of
c     centred expressions.
c
c-----------------------------------------------------------------------

      subroutine dvdd22(v,ddv,h,n)

         integer        n
         real*8         v(n),     ddv(n)
         real*8         h,        hm2
         integer        i

         hm2 = 1.0d0 / (h * h)
         ddv(1) = hm2 * (3.0d0 * v(1) - 9.0d0 * v(2) + 10.0d0 * v(3) -
     *                   5.0d0 * v(4) + v(5))
         do 10 i = 2, n - 1
            ddv(i) = hm2 * (v(i-1) - 2.0d0 * v(i) + v(i+1))
 10      continue
         ddv(n) = hm2 * ( v(n-4) - 5.0d0 * v(n-3) + 10.0d0 * v(n-2) -
     *                   9.0d0 * v(n-1) + 3.0d0 * v(n))

         return

      end
c
c-----------------------------------------------------------------------
c                                             2        2
c     Computes centred difference approx. of d [LCQ]/dr . Assumes
c     UCQ defined by
c
c     UCQ ** UCN = (LCQ ** LCN) / X
c
c     is smoother than LCQ. 2nd derivative of ln[X], 1st derivative
c     of LCQ must be supplied as well as storage for UCQ.
c
c-----------------------------------------------------------------------

      subroutine dvd2t0(ddlcq,dlcq,lcq,x,ddlnx,ucq,r,nr,lcn,ucn)

         implicit       logical*1 (a-z)

         integer        nr,            lcn,           ucn
         real*8         ddlcq(nr),
     *                  dlcq(nr),      lcq(nr),       x(nr),
     *                  ddlnx(nr),     ucq(nr),       r(nr)

         integer        j
         real*8         hdrpm1,        drpm2,         ucnpm1,
     *                  ducq,          dducq,         ucqpm1

         hdrpm1 = 0.5d0 / (r(2) - r(1))
         drpm2  = 1.0d0 / ((r(2) - r(1)) * (r(2) - r(1)))
         ucnpm1 = 1.0d0 / ucn

         do 10 j = 2 , nr - 1
            ucq(j) = (lcq(j) ** lcn / x(j)) ** ucnpm1
 10      continue

         do 20 j = 2 , nr - 1
            ducq = hdrpm1 * (ucq(j+1) - ucq(j-1))
            dducq = drpm2 * (ucq(j+1) - 2.0d0 * ucq(j) + ucq(j-1))
            ucqpm1 = 1.0d0 / ucq(j)
            ddlcq(j) = dlcq(j) * dlcq(j) / lcq(j) +
     *                 lcq(j) * (ucn * ucqpm1 *
     *                           (dducq - ducq * ducq * ucqpm1) +
     *                           ddlnx(j)) / lcn
 20      continue

         return

      end
c
c-----------------------------------------------------------------------
c
c     Computes centred difference approx. of d[LCQ]/dr. Assumes
c     UCQ defined by
c
c     UCQ ** UCN = (LCQ ** LCN) / X
c
c     is smoother than LCQ. Derivative of ln[X] must be supplied
c     as well as storage for UCQ.
c
c
c     End formulae give centred leading order truncation errors.
c     Valid only for uniform mesh.
c
c-----------------------------------------------------------------------

      subroutine dvde0 (dlcq,lcq,x,dlnx,ucq,r,nr,lcn,ucn)

         implicit       logical*1 (a-z)

         integer        nr,            lcn,           ucn
         real*8         dlcq(nr),      lcq(nr),       x(nr),
     *                  dlnx(nr),      ucq(nr),       r(nr)

         integer        j
         real*8         ducq,          hdrpm1,        lcnpm1,
     *                  ucnpm1

         hdrpm1 = 0.5d0 / (r(2) - r(1))
         lcnpm1 = 1.0d0 / lcn
         ucnpm1 = 1.0d0 / ucn

         do 10 j = 1 , nr
            ucq(j) = (lcq(j) ** lcn / x(j)) ** ucnpm1
 10      continue

         j = 1
         ducq  = hdrpm1 * (-4.0d0 * ucq(j)   + 7.0d0 * ucq(j+1) -
     *                      4.0d0 * ucq(j+2) +         ucq(j+3) )
         dlcq(j) = lcq(j) * lcnpm1 *
     *             ( (ucn / ucq(j)) * ducq + dlnx(j) )

         do 20 j = 2 , nr - 1
            ducq = hdrpm1 * (ucq(j+1) - ucq(j-1))
            dlcq(j) = lcq(j) * lcnpm1 *
     *                ( (ucn / ucq(j)) * ducq + dlnx(j) )
 20      continue

         j = nr
         ducq  = hdrpm1 * (       - ucq(j-3) + 4.0d0 * ucq(j-2) -
     *                      7.0d0 * ucq(j-1) + 4.0d0 * ucq(j)   )
         dlcq(j) = lcq(j) * lcnpm1 *
     *             ( (ucn / ucq(j)) * ducq + dlnx(j) )

         return

      end
c
c-----------------------------------------------------------------------
c                                             2        2
c     Computes centred difference approx. of d [LCQ]/dr . Assumes
c     UCQ defined by
c
c     UCQ ** UCN = (LCQ ** LCN) / X
c
c     is smoother than LCQ. 2nd derivative of ln[X], 1st derivative
c     of LCQ must be supplied as well as storage for UCQ.
c
c
c     End formulae yield centred leading order truncation error.
c     Valid only for uniform mesh.
c
c-----------------------------------------------------------------------

      subroutine dvdde0(ddlcq,dlcq,lcq,x,ddlnx,ucq,r,nr,lcn,ucn)

         implicit       logical*1 (a-z)

         integer        nr,            lcn,           ucn
         real*8         ddlcq(nr),
     *                  dlcq(nr),      lcq(nr),       x(nr),
     *                  ddlnx(nr),     ucq(nr),       r(nr)

         integer        j
         real*8         hdrpm1,        drpm2,         ucnpm1,
     *                  ducq,          dducq,         ucqpm1

         hdrpm1 = 0.5d0 / (r(2) - r(1))
         drpm2  = 1.0d0 / ((r(2) - r(1)) * (r(2) - r(1)))
         ucnpm1 = 1.0d0 / ucn

         do 10 j = 1 , nr
            ucq(j) = (lcq(j) ** lcn / x(j)) ** ucnpm1
 10      continue

         j = 1
         ducq  = hdrpm1 * (-4.0d0 * ucq(j)   + 7.0d0 * ucq(j+1) -
     *                      4.0d0 * ucq(j+2) +         ucq(j+3) )
         dducq = drpm2  * ( 3.0d0 * ucq(j)   - 9.0d0 * ucq(j+1) +
     *                     10.0d0 * ucq(j+2) - 5.0d0 * ucq(j+3) +
     *                     ucq(j+4) )
         ucqpm1 = 1.0d0 / ucq(j)
         ddlcq(j) = dlcq(j) * dlcq(j) / lcq(j) +
     *              lcq(j) * (ucn * ucqpm1 *
     *                        (dducq - ducq * ducq * ucqpm1) +
     *                        ddlnx(j)) / lcn

         do 20 j = 2 , nr - 1
            ducq = hdrpm1 * (ucq(j+1) - ucq(j-1))
            dducq = drpm2 * (ucq(j+1) - 2.0d0 * ucq(j) + ucq(j-1))
            ucqpm1 = 1.0d0 / ucq(j)
            ddlcq(j) = dlcq(j) * dlcq(j) / lcq(j) +
     *                 lcq(j) * (ucn * ucqpm1 *
     *                           (dducq - ducq * ducq * ucqpm1) +
     *                           ddlnx(j)) / lcn
 20      continue

         j = nr
         ducq  = hdrpm1 * (       - ucq(j-3) + 4.0d0 * ucq(j-2) -
     *                      7.0d0 * ucq(j-1) + 4.0d0 * ucq(j)   )
         dducq = drpm2  * (         ucq(j-4) - 5.0d0 * ucq(j-3) +
     *                     10.0d0 * ucq(j-2) - 9.0d0 * ucq(j-1) +
     *                      3.0d0 * ucq(j)   )
         ucqpm1 = 1.0d0 / ucq(j)
         ddlcq(j) = dlcq(j) * dlcq(j) / lcq(j) +
     *              lcq(j) * (ucn * ucqpm1 *
     *                        (dducq - ducq * ducq * ucqpm1) +
     *                        ddlnx(j)) / lcn

         return

      end
c
c-----------------------------------------------------------------------
c
c     Determines limits of a "pulse" in V. Returns IMIN, IMAX
c     such that
c
c     V(1) ... V(IMIN-1) and V(IMAX+1) ... V(N) are all less
c     than (1 - CUTOFF) times norm of V computed via FNORM.
c
c----------------------------------------------------------------------

      subroutine dvplsl(v,n,fnorm,cutoff,imin,imax)

         implicit       logical*1 (a-z)

         real*8         fnorm
         external       fnorm

         integer        n
         real*8         v(n)

         real*8         cutoff
         integer        imin,     imax

         real*8         ceil

         if( 0.0d0 .lt. cutoff  .and.  cutoff .lt. 1.0d0 ) then
            ceil = (1.0d0 - cutoff) * fnorm(v,n)
            do 10 imin = 1 , n
               if( abs(v(imin)) .ge. ceil )  go to 20
 10         continue
               imin = 0
               imax = 0
               return
 20         continue
            imin = max(1,imin-1)

            do 30 imax = n , 1 , -1
               if( abs(v(imax)) .ge. ceil ) go to 40
 30         continue
               imin = 0
               imax = 0
               return
 40         continue
            imax = min(n,imax+1)
         else
            imin = 0
            imax = 0
            WRITE(*,*) '<<< DVPLSL: Unexpected cut--off scale ',
     *                 CUTOFF,'. >>>'
         end if

         return

      end
c
c-----------------------------------------------------------------------
c
c     Primitive location of left and right edges of a feature.
c
c-----------------------------------------------------------------------

      subroutine dvinle(v,n,vedge,ipos)

         implicit       logical*1 (a-z)

         integer        n
         real*8         v(n)

         real*8         vedge
         integer        ipos

         do 10 ipos = 1 , n
            if( abs(v(ipos)) .ge. vedge ) go to 20
 10      continue
         ipos = 0
         return

 20      continue
         return

      end

      subroutine dvinre(v,n,vedge,ipos)

         implicit       logical*1 (a-z)

         integer        n
         real*8         v(n)

         real*8         vedge
         integer        ipos

         do 10 ipos = n , 1 , -1
            if( abs(v(ipos)) .ge. vedge ) go to 20
 10      continue
         ipos = 0
         return

 20      continue
         return

      end
c
c-----------------------------------------------------------------------
c
c     Determines limits of "ramp" in V assumed monotonic and positive.
c     Returns IMIN, IMAX such tha V(IMIN) ... V(IMAX) range from
c     VH * (1 - CUTOFF) ... VH * CUTOFF for CUTOFF > 0   or
c     VH * |CUTOFF| ... VH * (1 - |CUTOFF|) for CUTOFF < 0.
c     |CUTOFF| should be .GE. 0.5 from symmetry considerations.
c
c-----------------------------------------------------------------------

      subroutine dvrmpl(v,n,vh,cutoff,imin,imax)

         implicit       logical*1 (a-z)

         integer        n
         real*8         v(n)

         real*8         vh,      cutoff
         integer        imin,    imax

         real*8         vkey

         if( abs(cutoff) .ge. 0.5d0 ) then

            if( cutoff .ge. 0.0d0 ) then
               vkey = vh * (1.0d0 - cutoff)
               do 10 imin = 1 , n
                  if( v(imin) .ge. vkey ) go to 20
 10            continue
               go to 100
 20            continue
               vkey = vh * cutoff
               do 30 imax = n , 1 , -1
                  if( v(imax) .le. vkey ) go to 40
 30            continue
               go to 100
 40            continue
            else
               vkey = vh * abs(cutoff)
               do 50 imin = 1 , n
                  if( v(imin) .le. vkey ) go to 60
 50            continue
               go to 100
 60            continue
               vkey = vh * (1.0d0 - abs(cutoff))
               do 70 imax = n , 1 , -1
                  if( v(imax) .ge. vkey ) go to 80
 70            continue
               go to 100
 80            continue
            end if
         else
            WRITE(*,*) '<<< DVRMPL:: Unexpected value of CUTOFF',
     *                 CUTOFF,'.>>>'
            go to 100
         end if
c
c        Kludge.
c
         if( imin .gt. imax ) imin = imax

         return

 100     continue
         imin = 0
         imax = 0
         return

      end
c
c-----------------------------------------------------------------------
c
c     "Thins" (XFR(i),YFR(i)) pairs by scaling vectors to 1.0 x ASPECT
c     box and demanding that arc length between (XTO(i),YTO(i) pairs
c     is at least FRMNDS x length of box diagonal. End points always
c     members of returned vectors. Aborts if no x, y range.
c
c-----------------------------------------------------------------------

      subroutine dvthin(xfr,yfr,nfr, xto,yto,nto, aspect,frmnds)

         implicit       logical*1 (a-z)

         real*8         dvmin,     dvmax

         integer        nfr,       nto
         real*8         xfr(1),    yfr(1),    xto(1),    yto(1)
         real*8         aspect,    frmnds

         real*8         xrange,    yrange,    xscale,    yscale,
     *                  mnds
         integer        ifr

         xrange = dvmax(xfr,nfr) - dvmin(xfr,nfr)
         if( xrange .eq. 0.0d0 ) then
            WRITE(*,*) '<<< DVTHIN: No x--range. >>>'
            nto = 0
            return
         end if

         yrange = dvmax(yfr,nfr) - dvmin(yfr,nfr)
         if( yrange .eq. 0.0d0 ) then
            WRITE(*,*) '<<< DVTHIN: No y--range. >>>'
            nto = 0
            return
         end if
         

         xscale = 1.0d0 / xrange
         yscale = aspect / yrange

         mnds = frmnds * sqrt(1.0d0 + aspect * aspect)

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
c-----------------------------------------------------------------------
c     Version of dvthin which uses supplied bounding box instead of 
c     computing box dimensions from data and aspect ratio.
c-----------------------------------------------------------------------

      subroutine dvthin_bbox(xfr,yfr,nfr, xto,yto,nto, bbox, 
     &                       aspect,frmnds)

         implicit       none

         real*8         dvmin,     dvmax

         integer        nfr,       nto
         real*8         xfr(1),    yfr(1),    xto(1),    yto(1)
         real*8         bbox(*),   aspect,    frmnds

         real*8         xrange,    yrange,    xscale,    yscale,
     *                  mnds
         integer        ifr

         xrange = bbox(2) - bbox(1)
         if( xrange .eq. 0.0d0 ) then
            WRITE(*,*) '<<< dvthin_bbox: No x--range. >>>'
            nto = 0
            return
         end if

         yrange = bbox(4) - bbox(3)
         if( yrange .eq. 0.0d0 ) then
            WRITE(*,*) '<<< dvthin_bbox: No y--range. >>>'
            nto = 0
            return
         end if

         xscale = 1.0d0 / xrange
         yscale = aspect / yrange

         mnds = frmnds * sqrt(1.0d0 + aspect * aspect)

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
c-------------------------------------------------------------------------------
c
c     "Clipping function". Gaussian fall-off after X0.
c
c-------------------------------------------------------------------------------

      subroutine dvclp1(f,x,nx,c,x0,del)

         implicit     logical*1 (a-z)

         real*8       zmxne2
         parameter  ( zmxne2 = 13.42 4539 2937 566 )

         integer       nx
         real*8        f(nx),       x(nx)

         real*8        c,           x0,         del

         real*8        expnt
         integer       j

         do 10 j = 1 , nx
            if( x(j) .le. x0 ) then
               f(j) = c
            else
               expnt = ( (x(j) - x0) / del )
               if ( expnt .le. zmxne2 ) then
                  f(j) = c * exp(- expnt * expnt)
               else
                  f(j) = 0.0d0
               end if
            end if
 10      continue

         return

      end
c
c-------------------------------------------------------------------------------
c     Returns index of last local minimum in V, or 0 if monotonic. Constant
c     function will return N.
c
c-------------------------------------------------------------------------------
      integer function idvllm(v,n)

         implicit      logical*1 (a-z)

         integer       n
         real*8        v(n)

         do 10 idvllm = n - 1 , 2 , -1
            if( v(idvllm+1) .ge. v(idvllm)
     *                      .and.
     *          v(idvllm-1) .ge. v(idvllm) ) return
 10      continue

         idvllm = 0

         return

      end
c
c---------------------------------------------------------------------------
c
c     Returns (first) J such that ABS(V(J)) .GE. V(I) : I = 1 .. N.
c     i.e.  ABS(V(IXVINF(V,N))) = DVLINF(V,N).
c
c---------------------------------------------------------------------------

      integer function ixvinf(v,n)

         implicit      logical (a-z)

         integer       n
         real*8        v(n)

         real*8        vinf,         tvinf
         integer       j

         ixvinf = 1
         vinf = abs(v(ixvinf))
         do 10 j = 2 , n
            tvinf = abs(v(j))
            if( tvinf .gt. vinf ) then
               vinf = tvinf
               ixvinf = j
            end if
 10      continue

         return

       end
c
c-----------------------------------------------------------------------------
c
c     For use in error--norm tracing.
c
c-----------------------------------------------------------------------------

      subroutine nrmmes(v,n,text)

         implicit       logical (a-z)

         real*8         dvnrm1,      dvnrm2
         integer        ixvinf

         integer        n
         real*8         v(n)
         character*(*)  text

         real*8         nrm1,        nrm2
         integer        jmax

         nrm1 = dvnrm1(v,n)
         if( nrm1 .eq. 0.0d0 ) then
            write(*,1000) text
1000        FORMAT(' <<< NRMMES:: ',A,' is 0 vector. >>>')
         else
            nrm2 = dvnrm2(v,n)
            jmax = ixvinf(v,n)
            write(*,1500) text,
     *                    nrm1, nrm2, abs(v(jmax)), jmax, n
1500        FORMAT(' <<< NRMMES:: |',A,'|: ',3(1PE8.2,1X),
     *             '(',I4,'/',I4,'). >>>')
         end if

         return

      end
c
c----------------------------------------------------------------------------
c
c     Vector pronlogation.
c
c----------------------------------------------------------------------------

      subroutine dvprln(v1,v2,inc,n)

         implicit     logical (a-z)

         integer      n,      inc
         real*8       v1(n),  v2(n)

         integer      j

         do 10 j = 0 , n - 1
            v2(inc * j + 1) = v1(j+1)
 10      continue

         return

      end
c
c-----------------------------------------------------------------------------
c
c     Vector in--place averaging. For use in linear interpolation
c     after prolongation for example.
c
c-----------------------------------------------------------------------------

      subroutine dvav2(v,n)

         implicit      logical (a-z)

         integer       n
         real*8        v(n)

         integer       j

         do 10 j = 2 , n - 1 , 2
            v(j) = 0.5d0 * (v(j+1) + v(j-1))
 10      continue

         return

      end
c
c-----------------------------------------------------------------------
c
c     Scales double precision vector (if possible) to integer rep.
c     where range of data corresponds (is binned) to IMAX - IMIN + 1
c     equally spaced sub--ranges.
c
c-----------------------------------------------------------------------

      subroutine dviscl(v,iv,n,imin,imax)

         implicit     logical (a-z)

         integer      n
         real*8       v(n)
         integer      iv(n)
         integer      imin,         imax

         real*8       vmin,         vmax,        dvm1
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
            dvm1 = 1.0d0 * (imax - imin + 1) / (vmax - vmin)
            do 40 j = 1 , n
               iv(j) = min(imax,imin +
     *                          int( dvm1 * (v(j) - vmin) ))
 40         continue
         end if

         return

       end
c
c---------------------------------------------------------------------------
c
c     Encoding routine intended for testing DVECLIB routines.
c
c---------------------------------------------------------------------------

      double precision function dvhash(v,n)

         implicit      logical  (a-z)

         integer       n
         real*8        v(n)

         integer       j

         dvhash = 0.0d0
         do 10 j = 1 , n
            dvhash = dvhash + (v(j) + j ) ** 2
10       continue
         dvhash = sqrt( dvhash / n )

         return

      end
c
c---------------------------------------------------------------------------
c
c     Dumps a double precision number.
c
c---------------------------------------------------------------------------

      subroutine dout(x)

         real*8       x

         write(*,100) x
 100     format(1pe23.15)

         return

      end

c------------------------------------------------------------------------
c
c     Returns aspect ratio defined by x, y.
c
c------------------------------------------------------------------------

      double precision function dvcasp(x,y,nx,ny)

         implicit      logical (a-z)

         real*8        dvmin,       dvmax

         integer       nx,          ny
         real*8        x(nx),        y(ny)

         real*8        xrng,        yrng

         xrng = abs(dvmax(x,nx) - dvmin(x,nx))
         yrng = abs(dvmax(y,ny) - dvmin(y,ny))

         if( xrng .eq. 0.0d0 ) then
            dvcasp = 0.0d0
         else
            dvcasp = yrng / xrng
         end if

         return

      end

c-----------------------------------------------------------------------
c
c     No frills version of DVPDMP (and its "inverse").
c
c-----------------------------------------------------------------------

      subroutine dvpwri(x,yofx,n,unit)

         implicit       logical (a-z)

         integer        n
         real*8         x(n), yofx(n)
         integer        unit

         integer        j

         write(unit,*) n
         do 10 j = 1 , n
            write(unit,1000) x(j), yofx(j)
1000        format(1p,e24.16,1x,e24.16)
 10      continue
         return

      end

      subroutine dvprea(x,yofx,n,unit)

         implicit       logical (a-z)

         integer        n
         real*8         x(n), yofx(n)
         integer        unit

         integer        j

         read(unit,*,end=99,err=99) n
         do 10 j = 1 , n
            read(unit,*,end=99,err=99) x(j), yofx(j)
 10      continue
         return

 99      continue
            write(0,*) '<<< dvprea:: read error. >>>'
         return

      end
c
c     Same as above but <n> not input, output.
c
      subroutine dvpwri1(x,yofx,n,unit)

         implicit       logical (a-z)

         integer        n
         real*8         x(n), yofx(n)
         integer        unit

         integer        j

         do 10 j = 1 , n
            write(unit,1000) x(j), yofx(j)
1000        format(1p,e24.16,1x,e24.16)
 10      continue
         return

      end
c
c     Same as above but format supplied ...
c
      subroutine dvpwri1f(x,yofx,n,sfmt,unit)

         implicit       none

         integer        n
         real*8         x(n), yofx(n)
         character*(*)  sfmt
         integer        unit

         integer        j

         do 10 j = 1 , n
            write(unit,fmt=sfmt) x(j), yofx(j)
 10      continue
         return

      end

      subroutine dvprea1(x,yofx,n,unit)

         implicit       logical (a-z)

         integer        n
         real*8         x(n), yofx(n)
         integer        unit

         integer        j

         n = 0
 100     continue
            read(unit,*,end=200,err=99) x(n+1), yofx(n+1)
            n = n + 1
         go to 100
 200     return
      
 99      continue
            write(0,*) '<<< dvprea1:: read error. >>>'
         return

      end
c
      subroutine dvwri1(v,n,unit)

         implicit       logical (a-z)

         integer        n
         real*8         v(n)
         integer        unit

         integer        j

         do 10 j = 1 , n
            write(unit,1000) v(j)
1000        format(1p,e24.16)
 10      continue 

         return

      end

      subroutine dvrea1(v,n,unit)

         implicit       logical (a-z)

         integer        n
         real*8         v(n)
         integer        unit

         n = 0
         if( unit .eq. 6 ) then
            write(*,*) 'dvrea1: Can not read from unit 6'
            return
         end if

 100     continue
            read(unit,*,end=200,err=99) v(n+1)
            n = n + 1
         go to 100
 200     return
      
 99      continue
            write(0,*) '<<< dvprea:: read error. >>>'
         return

      end

      subroutine dvtwri1(v1,v2,v3,n,unit)

         implicit      none

         integer       n,        unit
         real*8        v1(n),    v2(n),    v3(n)

         integer       j

         do 10 j = 1 , n
            write(unit,1000) v1(j), v2(j), v3(j)
1000        format(1p,e24.16,1x,e24.16,1x,e24.16)
 10      continue

         return

      end

c-----------------------------------------------------------------------
c
c     No frills version of DVPDMP (and its "inverse").
c
c-----------------------------------------------------------------------

      subroutine dvp_dump(x,y_of_x,n,unit)

         implicit       logical (a-z)

         integer        n
         real*8         x(n), y_of_x(n)
         integer        unit

         integer        j

         write(unit,*) n
         do 10 j = 1 , n
            write(unit,*) x(j), y_of_x(j)
 10      continue
         return

      end

      subroutine dvp_load(x,y_of_x,n,unit)

         implicit       logical (a-z)

         integer        n
         real*8         x(n), y_of_x(n)
         integer        unit

         integer        j

         read(unit,*,end=99,err=99) n
         do 10 j = 1 , n
            read(unit,*,end=99,err=99) x(j), y_of_x(j)
 10      continue
         return

 99      continue
            write(0,*) '<<< dvp_load:: read error. >>>'
         return

      end
c
c     Functions for clipping. Add one for true index into V(N).
c
      integer function ix_clip_min(v,n,v_key)

         implicit        logical (a-z)

         integer         n
         real*8          v(0 : n-1)
         real*8          v_key

         integer         lo, mid, hi

         if( v_key .gt. v(n-1)  ) then
            ix_clip_min = -1
            return
         end if

         if( v_key .le. v(0) ) then
            ix_clip_min = 0
            return
         end if

         lo = 1
         hi = n
100      continue
         if( lo .gt. hi ) go to 200
            mid = (lo + hi) / 2
            if( v(mid - 1) .lt. v_key ) then
               if( v_key .le. v(mid) ) then
                  ix_clip_min = mid
                  return
               else
                  lo = mid + 1
               end if
            else
               hi = mid - 1
            end if
         go to 100

200      continue
            write(*,*) '<<< ix_clip_min:: Failure, apparently '//
     *                 'due to non--monotonic v. >>>'
            ix_clip_min = -2
         return

      end

      integer function ix_clip_max(v,n,v_key)

         implicit        logical (a-z)

         integer         n
         real*8          v(0 : n-1)
         real*8          v_key

         integer         lo, mid, hi

         if( v_key .lt. v(0) ) then
            ix_clip_max = -1
            return
         end if

         if( v_key .ge. v(n-1) ) then
            ix_clip_max = n-1
            return
         end if

         lo = 0
         hi = n-2
100      continue
         if( lo .gt. hi ) go to 200
            mid = (lo + hi) / 2
            if( v(mid) .le. v_key )  then
               if( v_key .lt. v(mid+1) ) then
                  ix_clip_max = mid
                  return
               else
                  lo = mid + 1
               end if
            else
               hi = mid - 1
            end if
         go to 100

200      continue
            write(*,*) '<<< ix_clip_max:: Failure, apparently '//
     *                 'due to non--monotonic v. >>>'
            ix_clip_max = -2
         return

      end
c
c     Front ends to clipping routines which allow for fuzzy decisions.
c
      integer function ixminf(v,n,v_key,fuzz)

         integer       ix_clip_min

         integer       n
         real*8        v(0 : n-1)
         real*8        v_key, fuzz

         ixminf = ix_clip_min(v,n,v_key)
         if( ixminf .gt. 0 ) then
            if( abs(v_key - v(ixminf - 1)) .le.
     *          fuzz * (v(ixminf) - v(ixminf-1)) ) then
               ixminf = ixminf - 1
            end if
         else
            if( abs(v(n-1) - v_key) .le.
     *          fuzz * (v(n-1) - v(n-2)) ) then
               ixminf = n-1
            end if
            if( abs(v_key - v(0)) .le.
     *          fuzz * (v(1) - v(0)) ) then
               ixminf = 0
            end if
         end if
         return

      end

      integer function ixmaxf(v,n,v_key,fuzz)

         integer       ix_clip_max

         integer       n
         real*8        v(0 : n-1)
         real*8        v_key, fuzz

         ixmaxf = ix_clip_max(v,n,v_key)
         if( ixmaxf .gt. 0  .and. ixmaxf .lt. n-1 ) then
            if( abs(v(ixmaxf + 1) - v_key ) .le.
     *          fuzz * (v(ixmaxf+1) - v(ixmaxf)) ) then
               ixmaxf = ixmaxf + 1
            end if
         else
            if( abs(v(n-1) - v_key) .le.
     *          fuzz * (v(n-1) - v(n-2)) ) then
               ixmaxf = n-1
            end if
            if( abs(v_key - v(0)) .le.
     *          fuzz * (v(1) - v(0)) ) then
               ixmaxf = 0
            end if
         end if
         return

      end

c---------------------------------------------------------------------
c
c     Make Integer MasK.
c
c---------------------------------------------------------------------
c
      subroutine dvmimk(v,mask,n,vmin,vmax)

         implicit     logical   (a-z)

         integer      n
         real*8       v(n)
         integer      mask(n)
         real*8       vmin,     vmax

         integer      j
         real*8       vj

         do 10 j = 1 , n
            vj = v(j)
            if( vj .ge. vmin  .and.  vj .le. vmax ) then
               mask(j) = 1
            else
               mask(j) = 0
            end if
 10      continue

         return

      end

c---------------------------------------------------------------------
c
c     Use Integer MasK.
c
c---------------------------------------------------------------------
c
      subroutine dvuimk(vin,mask,vout,nin,nout)

         implicit     logical   (a-z)

         integer      nin
         real*8       vin(nin),  vout(*)
         integer      mask(nin)

         integer      nout,     jin

         do 10 jin = 1 , nin
            if( mask(jin) .ne. 0 ) then
               nout = nout + 1
               vout(nout) = vin(jin)
            end if
 10      continue

         return

      end

c---------------------------------------------------------------------
c
c     Make Logical MasK.
c
c---------------------------------------------------------------------
c
      subroutine dvmlmk(v,mask,n,vmin,vmax)

         implicit     logical   (a-z)

         integer      n
         real*8       v(n)
         logical      mask(n)
         real*8       vmin,     vmax

         integer      j
         real*8       vj

         do 10 j = 1 , n
            vj = v(j)
            mask(j) =  vj .ge. vmin  .and.  vj .le. vmax
 10      continue

         return

      end

c---------------------------------------------------------------------
c
c     Use Integer MasK.
c
c---------------------------------------------------------------------
c
      subroutine dvulmk(vin,mask,vout,nin,nout)

         implicit     logical   (a-z)

         integer      nin
         real*8       vin(nin),  vout(*)
         logical      mask(nin)

         integer      nout,     jin

         do 10 jin = 1 , nin
            if( mask(jin) ) then
               nout = nout + 1
               vout(nout) = vin(jin)
            end if
 10      continue

         return

      end

c-----------------------------------------------------------------------
c
c     History: IDVSCN, tests for equality with fractional fuzziness.
c
c     Returns index of first element of V equal to, less than or
c     greater than VKEY for CODE = 1, 2 or 3 respectively; returns
c     0 if condition not met. If CODE < 0, sets FUZZ value.
c
c-----------------------------------------------------------------------
c
      integer function idvscf(v,n,vkey,code)
c
         real*8       v(1)
         real*8       vkey
         integer      code, i, n
c
         real*8       fuzz
         data         fuzz / 1.0d-05 /
         save         fuzz
c
         if( code .lt. 0 ) then
            fuzz = vkey
            write(*,1500) fuzz
1500        FORMAT(' <<< IDVSCF:: Fuzz set to ',1PE12.2,'. >>>')
            idvscf = -1
            return
         end if
         go to ( 100, 200, 300 ), code
c
 100     continue
            idvscf = 0
            do 110 i = 1 , n
               if( v(i) .eq. 0.0d0 ) then
                  if( v(i) .eq. vkey ) then
                     idvscf = i
                     go to 1000
                  end if
               else
                  if( abs(v(i) - vkey) / abs(vkey) .le. fuzz ) then
                     idvscf = i
                     go to 1000
                  end if
               end if
 110        continue
         go to 1000
c
 200     continue
            idvscf = 0
            do 210 i = 1 , n
               if( v(i) .lt. vkey ) then
                  idvscf = i
                  go to 1000
               end if
 210        continue
         go to 1000
c
 300     continue
            idvscf = 0
            do 310 i = 1 , n
               idvscf = 0
               if( v(i) .gt. vkey ) then
                  idvscf = i
                  go to 1000
               end if
 310        continue
         go to 1000
c
1000     continue
c
         return
c
      end
c
c--------------------------------------------------------------------
c
c     Another thinning routine; primarily for data which will
c     subsequently be plotted.
c     May need to  add aspect parameter.
c
c--------------------------------------------------------------------

      subroutine dvthpl( xfr, yfr, nfr,  xto, yto, nto,  thetac )

         implicit        logical (a-z)

         real*8          dvmin,        dvmax,
     *                   dotarg,       cdotarg

         integer         nfr,          nto
         real*8          xfr(nfr),     yfr(nfr),
     *                   xto(1),       yto(1)
         real*8          thetac

         real*8          xrange,       yrange,
     *                   scale,        theta
         integer         ilast,        ifr

         xrange = abs(dvmax(xfr,nfr) - dvmin(xfr,nfr))
         yrange = abs(dvmax(yfr,nfr) - dvmin(yfr,nfr))
         if( xrange .eq. 0.0d0 ) then
            write(*,*) '<<< dvthpl:: No x--range. >>>'
            nto = 0
            return
         end if

         nto = 2
         call dvcopy(xfr,xto,nto)
         call dvcopy(yfr,yto,nto)
         if( yrange .eq. 0.0d0 ) then
            return
         end if
         scale = xrange / yrange

         do 100 ifr = 3 , nfr
            theta = dotarg(         xfr(ifr) - xfr(ifr-1),
     *                     scale * (yfr(ifr) - yfr(ifr-1)),
     *                              xto(nto) - xto(nto-1),
     *                     scale * (yto(nto) - yto(nto-1)))
            if( abs(theta) .ge. thetac ) then
               nto = nto + 1
            end if
            xto(nto) = xfr(ifr)
            yto(nto) = yfr(ifr)
 100     continue
         if( xto(nto) .ne. xfr(nfr) ) then
            nto = nto + 1
            xto(nto) = xfr(nfr)
            yto(nto) = yfr(nfr)
         end if

         return

      end

c--------------------------------------------------------------------
c
c     Angle between vectors.
c
c--------------------------------------------------------------------

      double precision function dotarg(x1,y1,x2,y2)

         real*8     x1, y1, x2, y2
         real*8     arg

         arg = (x1 * x2 + y1 * y2) /
     *         sqrt((x1 * x1 + y1 * y1) * (x2 * x2 + y2 * y2))
c
c        This seems to be needed when some of the x, y are tiny ...
c
         dotarg = acos(min(arg,1.0d0))

         return

      end

c--------------------------------------------------------------------
c
c     A different type of ramp function ...
c     Assumes v1 > vn.
c
c--------------------------------------------------------------------

      subroutine dvinex(v,v1,vn,dv1,n)

         implicit       logical   (a-z)

         integer        n
         real*8         v(n)
         real*8         v1, vn, dv1, fact

         real*8         epsi
         parameter    ( epsi = 1.0d-13 )
         integer        mxiter
         parameter    ( mxiter = 50 )

         real*8         dv, fact_lo, fact_hi, fact_tol
         parameter    (                       fact_tol = 1.0d-012 )
         integer        j, iter

         fact = ((v1 - vn) / dv1 * (2.0d0 / n))  ** (1.0d0 / n)
         fact_hi = 2.0d0 * fact
         fact_lo = 0.5d0 * fact

         iter = 0
100      continue
            v(1) = v1
            dv = dv1
            do 10 j = 2 , n
                v(j) = v(j-1) - dv
               dv = dv * fact
 10         continue
            iter = iter + 1
            if( (abs(v(n) - vn) / abs(vn)) .le. fact_tol  .or.
     *          iter .gt. mxiter ) then
               v(n) = vn
               return
            end if
            if( v(n) .gt. vn ) then
               fact_lo = fact
            else
               fact_hi = fact
            end if
            fact = 0.5d0 * (fact_hi + fact_lo)
c            write(*,*) v(n), vn, fact_lo, fact, fact_hi
         go to 100

      end

c--------------------------------------------------------------------
c
c     Reverses dvinex's output with a more logical calling sequence. 
c     Assumes v1 < vn.
c
c--------------------------------------------------------------------

      subroutine dvinexf(v,v1,vn,dvn,n)

         implicit       none

         integer        n
         real*8         v(n)
         real*8         v1, vn, dvn

         call dvinex(v,vn,v1,dvn,n)
         call dvrevp(v,n)

         return

      end 


c--------------------------------------------------------------------
c
c     "Reflection" of V1 about VREF.
c
c--------------------------------------------------------------------

      subroutine dvrflc(v1,v2,n,vref)

         integer       n
         real*8        v1(n),      v2(n)
         real*8        vref

         integer       j

         do 10 j = 1 , n
            v2(j) = 2.0d0 * vref - v1(n - j + 1)
 10      continue

         return

      end

c--------------------------------------------------------------------
c
c     Predicate: Is V uniform 1Grid (up to fuzz)?
c
c--------------------------------------------------------------------

      logical function isu1g(v,n,fuzz)
                 
         implicit      none
         
         real*8        dfuzz
         parameter   ( dfuzz = 1.0d-5 )

         integer       n
         real*8        v(n)
         real*8        fuzz

         real*8        dv,        fdv
         integer       i

         if( fuzz .le. 0.0d0  .or.  fuzz .ge.  1.0d0 ) then
            fuzz = dfuzz
         end if

         isu1g = .true.
         if( n .gt. 2 ) then
            dv = v(2) - v(1)
            fdv = fuzz * dv
            do 10 i = 3 , n
               if( abs(dv - (v(i) - v(i-1))) .gt. fdv ) then
                  isu1g = .false.
         return      
               end if
 10         continue
         end if
         return

         end

c--------------------------------------------------------------------
c
c     Integer version of above function ... 1 true, 0 false ...
c
c--------------------------------------------------------------------

      integer function iisu1g(v,n,fuzz)
                 
         implicit      none
         
         real*8        dfuzz
         parameter   ( dfuzz = 1.0d-5 )

         integer       n
         real*8        v(n)
         real*8        fuzz

         real*8        dv,        fdv
         integer       i

         if( fuzz .le. 0.0d0  .or.  fuzz .ge.  1.0d0 ) then
            fuzz = dfuzz
         end if

         iisu1g = 1
         if( n .gt. 2 ) then
            dv = v(2) - v(1)
            fdv = fuzz * dv
            do 10 i = 3 , n
               if( abs(dv - (v(i) - v(i-1))) .gt. fdv ) then
                  iisu1g = 0
         return      
               end if
 10         continue
         end if
         return

         end

c--------------------------------------------------------------------
c
c     Logarithmic transformation for use in biasing small r 
c     truncation error estimates in adaptive regridding programs.
c
c--------------------------------------------------------------------

      subroutine dvlntb(v1,v2,n,vsc,p)

         implicit       none

         integer        n
         real*8         v1(n),       v2(n)
         real*8         vsc
         integer        p

         integer        i

         if( vsc .le. 0.0d0  .or.  p .le. 0 ) then
            call dvls(v2,1.0d0,n)
            return
         end if

         if( p .eq. 1 ) then
            do 10 i = 1 , n
               if( v1(i) .gt. 0.0d0 ) then
                  v2(i) = 1.0d0 + log(vsc / v1(i))
               else
                  v2(i) = 1.0d0
               end if
 10         continue
         else
            do 20 i = 1 , n
               if( v1(i) .gt. 0.0d0 ) then
                  v2(i) = (1.0d0 + log(vsc / v1(i))) ** p
               else
                  v2(i) = 1.0d0
               end if
 20         continue
         end if

         return

      end

c--------------------------------------------------------------------
c
c     Computes ratio of succesive differences.
c   
c
c--------------------------------------------------------------------

      subroutine dvrsd(v1,v2,n)

         implicit      none

         integer       n
         real*8        v1(n),       v2(n)
   
         real*8        undef
         parameter   ( undef = 0.0d0 )

         real*8        den,         num 
         integer       j

         if( n .lt. 3 ) then
            call dvls(v2,undef,n)
            return
         end if

         den = v1(2) - v1(1)
         do 10 j = 3 , n 
            num = v1(j) - v1(j-1)
            if( den .ne. 0.0d0 ) then
               v2(j-2) = num / den
            else
               v2(j-2) = undef
            end if
            den = num
 10      continue

         return

      end

c--------------------------------------------------------------------
c
c     Linear transformation.
c     
c     v2   = v20 + m (v1   -  v10).
c       j               j
c
c--------------------------------------------------------------------

      subroutine dvlt(v1,v2,n,v20,m,v10)

         implicit        none
         
         integer         n
         real*8          v1(n),         v2(n)
         real*8          v20,           m,          v10

         integer         j

         do 10 j = 1 , n
            v2(j) = v20 + m * (v1(j) - v10)
 10      continue

         return

      end

c--------------------------------------------------------------------
c
c     Maps x, y onto unit square.
c
c--------------------------------------------------------------------

      subroutine dvxyun(x,y,x1,y1,n,n1)

         implicit         none

         real*8           dvmin,        dvmax

         integer          n,            n1
         real*8           x(n),         y(n),
     *                    x1(n),        y1(n)

         real*8           xmin,         xmax,
     *                    ymin,         ymax

         xmin = dvmin(x,n)
         xmax = dvmax(x,n)
         ymin = dvmin(y,n)
         ymax = dvmax(y,n)

         if( ymax .gt. ymin ) then
            if( xmax .gt. xmin ) then
               call dvlt(x,x1,n,0.0d0,1.0d0 / (xmax - xmin),xmin)
               call dvlt(y,y1,n,0.0d0,1.0d0 / (ymax - ymin),ymin)
               n1 = n
            else
               n1 = 0
            end if
         else
            n1 = 0
         end if

         return

      end

c--------------------------------------------------------------------
c
c     Rescales x and y so that x is on unit interval, y(1) = 0.
c
c--------------------------------------------------------------------

      subroutine dvxun(x,y,x1,y1,n,n1)

         implicit         none

         real*8           dvmin,        dvmax

         integer          n,            n1
         real*8           x(n),         y(n),
     *                    x1(n),        y1(n)

         real*8           xmin,         xmax,
     *                    ymin,         ymax

         xmin = dvmin(x,n)
         xmax = dvmax(x,n)
         ymin = dvmin(y,n)
         ymax = dvmax(y,n)

         if( ymax .gt. ymin ) then
            if( xmax .gt. xmin ) then
               call dvlt(x,x1,n,0.0d0,1.0d0 / (xmax - xmin),xmin)
               call dvlt(y,y1,n,0.0d0,1.0d0 / (xmax - xmin),ymin)
               n1 = n
            else
               n1 = 0
            end if
         else
            n1 = 0
         end if

         return

      end
c
c     Front ends to *slow* clipping routines which allow for fuzzy decisions.
c     Return "sensible" thing for non--montonic data.
c
      integer function ixminf_slow(v,n,v_key,fuzz)

         implicit      none 

         integer       ix_clip_min_slow

         integer       n
         real*8        v(0 : n-1)
         real*8        v_key, fuzz

         ixminf_slow = ix_clip_min_slow(v,n,v_key)
         if( ixminf_slow .gt. 0 ) then
            if( abs(v_key - v(ixminf_slow - 1)) .le.
     *          fuzz * (v(ixminf_slow) - v(ixminf_slow-1)) ) then
               ixminf_slow = ixminf_slow - 1
            end if
         else
            if( abs(v(n-1) - v_key) .le.
     *          fuzz * (v(n-1) - v(n-2)) ) then
               ixminf_slow = n-1
            end if
            if( abs(v_key - v(0)) .le.
     *          fuzz * (v(1) - v(0)) ) then
               ixminf_slow = 0
            end if
         end if
         return

      end

      integer function ixmaxf_slow(v,n,v_key,fuzz)

         implicit      none 

         integer       ix_clip_max_slow

         integer       n
         real*8        v(0 : n-1)
         real*8        v_key, fuzz

         ixmaxf_slow = ix_clip_max_slow(v,n,v_key)
         if( ixmaxf_slow .gt. 0  .and. ixmaxf_slow .lt. n-1 ) then
            if( abs(v(ixmaxf_slow + 1) - v_key ) .le.
     *          fuzz * (v(ixmaxf_slow+1) - v(ixmaxf_slow)) ) then
               ixmaxf_slow = ixmaxf_slow + 1
            end if
         else
            if( abs(v(n-1) - v_key) .le.
     *          fuzz * (v(n-1) - v(n-2)) ) then
               ixmaxf_slow = n-1
            end if
            if( abs(v_key - v(0)) .le.
     *          fuzz * (v(1) - v(0)) ) then
               ixmaxf_slow = 0
            end if
         end if
         return

      end

c
c     Functions for clipping. Add one for true index into V(N).
c
      integer function ix_clip_min_slow(v,n,v_key)

         implicit        none 

         integer         n
         real*8          v(0 : n-1)
         real*8          v_key

         do 100 ix_clip_min_slow = 0 , n - 1
            if( v_key .le. v(ix_clip_min_slow) ) return
 100     continue

         ix_clip_min_slow = -1

         return

      end

      integer function ix_clip_max_slow(v,n,v_key)

         implicit        none 

         integer         n
         real*8          v(0 : n-1)
         real*8          v_key

         do 100 ix_clip_max_slow = n - 1 , 0 , -1
            if( v_key .ge. v(ix_clip_max_slow) ) return
 100     continue

         ix_clip_max_slow = -1

         return

      end

c---------------------------------------------------------------------
c
c     What DVRNG probably should have been. Will run considerably 
c     slower that DVRNG for long monotonic vectors.
c
c---------------------------------------------------------------------

      subroutine dvfilt(xfrom,yfrom,nfrom,xto,yto,nto,xmin,xmax)

         implicit       none

         integer        nfrom,          nto
         real*8         xfrom(nfrom),   yfrom(nfrom),
     *                  xto(nto),       yto(nto)
         real*8         xmin,           xmax

         integer        i
         real*8         xfromi

         nto = 0
         if( xmax .le. xmin ) return

         do 100 i = 1 , nfrom
            xfromi = xfrom(i)
            if( xmin .le. xfromi  .and.  xfromi .le. xmax ) then
               nto = nto + 1
               xto(nto) = xfromi
               yto(nto) = yfrom(i)
            end if
 100     continue

         return
      
      end
c
c     Chebyshev polynomials ...
c
      double precision function dcheby(x,n)

         implicit      none

         real*8        dacos, dcos
         real*8        x,     n

         dcheby = dcos(n * dacos(max(min(x,1.0d0),-1.0d0)))

         return

      end
c
c     Chebyshev polynomials using recurrence relation.
c
      double precision function dcheb1(x,n)

         implicit      none

         real*8        x,            n

         real*8        dcheb_m1,     dcheb_m2,        twox
         integer       i,            in

         in = n
         if(      in .eq. 0 ) then
            dcheb1 = 1.0d0
         else if( in .eq. 1 ) then
            dcheb1 = x
         else 
            dcheb_m2 = 1.0d0
            dcheb_m1 = x
            twox     = 2.0d0 * x
            do 100 i = 2 , in
               dcheb1 = twox * dcheb_m1 - dcheb_m2
               dcheb_m2 = dcheb_m1
               dcheb_m1 = dcheb1
 100        continue
         end if

         return

      end

      subroutine dvcheb(x,p,n,ord)

         implicit      none

         integer       n,     ord
         real*8        x(n),  p(n)

         external      dcheby,      dcheb1
         real*8        dcheby,      dcheb1

c        call dvfap2(x,p,dcheby,1.0d0 * ord,n)
         call dvfap2(x,p,dcheb1,1.0d0 * ord,n)

         return

      end

c
c     Trapeozidal rule integration ... O(h^2) ...
c
      double precision function dvtrap(x,fofx,n)

         implicit      none

         integer       n
         real*8        x(n),       fofx(n)

         integer       i

         dvtrap = 0.0d0
         do 100 i = 1 , n - 1
            dvtrap = dvtrap + (x(i+1) - x(i)) * (fofx(i+1) + fofx(i))
 100     continue
         dvtrap = 0.5d0 * dvtrap

         return

      end
c
c     ``Chebyshev weighted '' trapeozidal rule integration ... O(h^2) ...
c
      double precision function dvtrpc(x,fofx,n)

         implicit      none

         integer       n
         real*8        x(n),       fofx(n)

         real*8        midx,       w
         integer       i

         dvtrpc = 0.0d0
         do 100 i = 1 , n - 1
            midx = 0.5d0 * (x(i) + x(i+1))
            w = 1.0d0 / sqrt(1.0d0 - midx * midx)
            dvtrpc = dvtrpc + 
     *               w * (x(i+1) - x(i)) * (fofx(i+1) + fofx(i))
 100     continue
         dvtrpc = 0.5d0 * dvtrpc

         return

      end
c
c     Legendre polynomials using recurrence relation.
c
      double precision function dleg(x,n)

         implicit      none

         real*8        x,            n

         real*8        dleg_m1,    dleg_m2,
     *                 ri
         integer       i,            in

         in = n
         if(      in .eq. 0 ) then
            dleg = 1.0d0
         else if( in .eq. 1 ) then
            dleg = x
         else 
            dleg_m2 = 1.0d0
            dleg_m1 = x
            do 100 i = 2 , in
               ri = i
               dleg = ( (2.0d0 * ri - 1.0d0) * x * dleg_m1 -
     *                    (ri - 1.0d0) * dleg_m2 ) / ri
               dleg_m2 = dleg_m1
               dleg_m1 = dleg
 100        continue
         end if

         return

      end
c
c     Vector Legendre poly application.
c
      subroutine dvleg(x,p,n,ord)

         implicit      none

         integer       n,     ord
         real*8        x(n),  p(n)

         external      dleg
         real*8        dleg

         call dvfap2(x,p,dleg,1.0d0 * ord,n)

         return

      end
c
c     Ranging predicate ...
c
      logical function drange(xmin,x,xmax,fuzz)

         implicit      none
         
         real*8        xmin,       x,       xmax,
     *                 fuzz
         real*8        dx

         dx = abs(fuzz * (xmax - xmin))
         if( xmin .le. xmax ) then
            drange = xmin  - dx  .le. x  .and.  x .le. xmax + dx
         else
            drange = xmax  - dx  .le. x  .and.  x .le. xmin + dx
         end if

         return
   
      end
c
c     Vector chebyshev representation evaluation---uses standard
c     trick for summing over functions satisfying recurrence 
c     relation.
c
      subroutine dvechb(x,f,n, xmin,xmax, c,nc)

         implicit       none
   
         logical        drange

         integer        n
         real*8         x(n),        f(n)
         real*8         xmin,        xmax
         integer        nc
         real*8         c(0:nc)

         real*8         delm1,       sigma,
     *                  xx,          twox,
     *                  uk,          ukp1,        ukp2
         integer        j,           k

         delm1 = 1.0d0 / (xmax - xmin)
         sigma = xmax + xmin
         do 200 j = 1 , n
            if( drange(xmin,x(j),xmax,1.0d-10) ) then
               xx = delm1 * (2.0d0 * x(j) - sigma)
               twox = 2.0d0 * xx
               ukp2 = 0.0d0
               ukp1 = 0.0d0
               do 100 k = nc , 0 , -1 
                  uk   = c(k) + twox * ukp1 - ukp2
                  ukp2 = ukp1
                  ukp1 = uk
 100           continue
               f(j) = ukp1 - xx * ukp2
            end if
 200     continue

         return

      end
c
c     Vector legendre representation evaluation.
c
      subroutine dveleg(x,f,n, xmin,xmax, c,nc)

         implicit       none
   
         logical        drange

         integer        n
         real*8         x(n),        f(n)
         real*8         xmin,        xmax
         integer        nc
         real*8         c(0:nc)

         real*8         delm1,       sigma,
     *                  xx,          twox,
     *                  uk,          ukp1,        ukp2
         integer        j,           k

         delm1 = 1.0d0 / (xmax - xmin)
         sigma = xmax + xmin
         do 200 j = 1 , n
            if( drange(xmin,xx,xmax,1.0d-10) ) then
               xx = delm1 * (2.0d0 * x(j) - sigma)
               ukp2 = 0.0d0
               ukp1 = 0.0d0
               do 100 k = nc , 0 , -1 
                  uk = 
     *             c(k) + 
     *             (2.0d0 * k + 1.0d0) * xx * ukp1 / (k + 1.0d0) -
     *             (k + 1.0d0) * ukp2 / (k + 2.0d0)
                  ukp2 = ukp1
                  ukp1 = uk
 100           continue
               f(j) = ukp1
            end if
 200     continue

         return

      end
c
c     Chebyshev fitting routine ... given x(1..n), f(1..n), nc,
c     returns c(0..nx), xmin, xmax.
c
      subroutine dvchft(x,f,xw,fw,n,c,nc,xmin,xmax)

         implicit      none

         real*8        pi
         parameter   ( pi = 3.14159265358979323846d0 )

         real*8        dvmin,      dvmax,     dvtrpc

         integer       n
         real*8        x(n),       f(n),      
     *                 xw(n),      fw(n)
         
         integer       nc
         real*8        c(0:nc)

         real*8        xmin,       xmax

         integer       ic

         xmin = dvmin(x,n)
         xmax = dvmax(x,n)
         if( xmax .eq. xmin ) then
            write(*,*) '<<< dvchft:: No x--range: Aborting. >>>'
            return
         end if

         call dvlt(x,xw,n,-1.0d0,2.0d0 / (xmax - xmin),xmin)
         do 100 ic = 0 , nc
            call dvcheb(xw,fw,n,ic)
            call dvvm(f,fw,fw,n)
            c(ic) = dvtrpc(xw,fw,n) / pi
            if( ic .ne. 0 ) then
               c(ic) = 2.0d0 * c(ic)
            end if
 100     continue
         
         return

      end
c
c     Legendre fitting routine ... given x(1..n), f(1..n), nc,
c     returns c(0..nx), xmin, xmax.
c
      subroutine dvleft(x,f,xw,fw,n,c,nc,xmin,xmax)

         implicit      none

         real*8        dvmin,      dvmax,     dvtrap

         integer       n
         real*8        x(n),       f(n),      
     *                 xw(n),      fw(n)
         
         integer       nc
         real*8        c(0:nc)

         real*8        xmin,       xmax

         integer       ic

         xmin = dvmin(x,n)
         xmax = dvmax(x,n)
         if( xmax .eq. xmin ) then
            write(*,*) '<<< dvleft:: No x--range: Aborting. >>>'
            return
         end if

         call dvlt(x,xw,n,-1.0d0,2.0d0 / (xmax - xmin),xmin)
         do 100 ic = 0 , nc
            call dvleg(xw,fw,n,ic)
            call dvvm(f,fw,fw,n)
            c(ic) = 0.5d0 * (2.0d0 * ic + 1.0d0 ) * dvtrap(xw,fw,n) 
 100     continue
         
         return

      end
c
c     Smallest and largest abs(succesive difference).
c
      double precision function dvdmin(v,n)

         implicit      none

         integer       n
         real*8        v(n)

         integer       j

         dvdmin = 0.0d0
         if( n .gt. 1 ) then
            dvdmin = abs(v(2) - v(1))
            do 10 j = 3 , n
               dvdmin = min(dvdmin,abs(v(j) - v(j-1)))
  10        continue
         end if
         return

      end
 
      double precision function dvdmax(v,n)

         implicit      none

         integer       n
         real*8        v(n)

         integer       j

         dvdmax = 0.0d0
         do 10 j = 2 , n
            dvdmax = max(dvdmax,abs(v(j) - v(j-1)))
  10     continue

         return

      end
c
c     Extrapolated trapeozidal rule integration ... O(h^2) ...
c 
c     Nowhere near bullet--proof and has debugging code ...
c
      double precision function dvxtrap1(x,fofx,n,nl)

         implicit      none

         real*8        dvmin,      dvmax,
     *                 dvdmin,     dvdmax
         integer       lnintd

         integer       n,              nl
         real*8        x(n),           fofx(n)

         integer       max_lg2_n,      min_lg2_n,
     *                 max_l,          max_n
         parameter   ( max_lg2_n = 16, min_lg2_n = 4,
     *                 max_l = 6,      max_n = 65537 )

         real*8        xx(max_n),      ff(max_n)
         real*8        result(max_l)

         real*8        xmin,           xmax,       xrange,
     *                 n_max,          lg2,
     *                 dx_min
         integer       nmax

         logical       ltrace 
         parameter   ( ltrace = .false. )
         integer       rc,             lnl,         l,
     *                 j,              inc

         dvxtrap1 = 0.0d0

         xmin = dvmin(x,n)
         xmax = dvmax(x,n)
         xrange = xmax - xmin
         if( xrange .eq. 0.0d0 ) then  
            if( ltrace ) then 
               write(*,*) '<<< dvxtrap1:: Warning ... no x--range. >>>'
            end if
            return
         end if
         dx_min = dvdmin(x,n)
         if( dx_min .eq. 0.0d0 ) then
            if( ltrace ) then 
               write(*,*) '<<< dvxtrap1:: Warning ... degenerate x '//   
     *                    'values. >>>'
               return
            end if
         end if

         n_max   = abs(xrange / dx_min)
         lg2     = min(lnintd(log(1.0d0 * n_max) / log(2.0d0)) * 1.0d0,
     *                 max_lg2_n * 1.0d0)
         lg2     = max(lg2,min_lg2_n * 1.0d0)
         n_max   = lnintd(2.0d0 ** lg2) + 1.0d0
         if( ltrace ) then
            write(*,*) '>>> n, n_max'
            write(*,*) n, n_max
            write(*,*) '>>> xrange, dx_min, lg2 ...'
            write(*,*) xrange, dx_min, lg2
         end if
         dx_min = xrange / (n_max - 1.0d0)
         if( ltrace ) then
            write(*,*) '>>> dx_min ...'
            write(*,*) dx_min
         end if

         nmax = n_max
         call dvramp(xx,xmin,dx_min,nmax)
         xx(nmax) = xmax
c        call dvinqn(fofx,x,ff,xx,n,nmax,0.0d0,0.0d0,2*nl+1)
         call dvinqn(fofx,x,ff,xx,n,nmax,0.0d0,0.0d0,2*nl)

         lnl = min(lg2,1.0d0 * nl)
         inc = 1
         do 200 l = lnl , 1 , -1
            result(l) = 0.0d0
            do 100 j = 1 , nmax - inc , inc
               result(l) = result(l) + (ff(j) + ff(j+inc))
 100        continue
            result(l) = result(l) * 0.5d0 * (xx(1+inc) - xx(1))
            inc = 2 * inc
 200     continue

         if( ltrace ) then
            call dvdump(result,lnl,'results',6) 
            if( lnl .gt. 2 ) then
               call dvrsd(result,ff,lnl)
               call dvdump(ff,lnl-2,'convergence',6)
            end if
         end if
         call dvrex(result,lnl,2.0d0)
         if( ltrace ) then
            call dvdump(result,1,'extrapolated',6)
         end if
         dvxtrap1 = result(1)

         return

      end
c
c     Legendre fitting routine ... given x(1..n), f(1..n), nc,
c     returns c(0..nx), xmin, xmax. Uses extrapolated trapezoidal
c     integrator.
c
      subroutine dvleftx(x,f,xw,fw,n,nl,c,nc,xmin,xmax)

         implicit      none

         real*8        dvmin,      dvmax,     dvxtrap1

         integer       n,          nl
         real*8        x(n),       f(n),      
     *                 xw(n),      fw(n)
         
         integer       nc
         real*8        c(0:nc)

         real*8        xmin,       xmax

         integer       ic

         xmin = dvmin(x,n)
         xmax = dvmax(x,n)
         if( xmax .eq. xmin ) then
            write(*,*) '<<< dvleft:: No x--range: Aborting. >>>'
            return
         end if

         call dvlt(x,xw,n,-1.0d0,2.0d0 / (xmax - xmin),xmin)
         do 100 ic = 0 , nc
            call dvleg(xw,fw,n,ic)
            call dvvm(f,fw,fw,n)
            c(ic) = 0.5d0 * (2.0d0 * ic + 1.0d0 ) * 
     *              dvxtrap1(xw,fw,n,nl) 
 100     continue
         
         return

      end
c
c     Chebyshev fitting routine ... given x(1..n), f(1..n), nc,
c     returns c(0..nx), xmin, xmax. Interpolates to uniform
c     grid in th = cos x.
c
c     Attempt at Richardson extrapolation probably misguided here ...
c
      subroutine dvchftx(x,f,xw,fw,n,nl,c,nc,xmin,xmax)

         implicit      none

         real*8        pi
         parameter   ( pi = 3.14159265358979323846d0 )

         real*8        dvmin,      dvmax,     dvtrpc,
     *                 dvdmin,     dvdmax
         integer       lnintd

         intrinsic     dcos

         integer       n,          nl
         real*8        x(n),       f(n),      
     *                 xw(n),      fw(n)
         
         integer       nc
         real*8        c(0:nc)

         real*8        xmin,       xmax

         integer       max_lg2_n,      min_lg2_n,
     *                 max_l,          max_n
         parameter   ( max_lg2_n = 16, min_lg2_n = 4,
     *                 max_l = 6,      max_n = 65537 )
   
         real*8        costh(max_n),   ff(max_n),
     *                 t_n(max_n)
         real*8        result(max_l),  rsd(max_l)

         real*8        dx_min,         lg2,
     *                 dth_min,        dth,           ldth,
     *                 n_max,          xrange
         integer       nn,             nmax
         integer       rc,             lnl,           l,
     *                 j,              inc,           ic

         
         logical       ltrace
         parameter   ( ltrace = .false. )

         xmin = dvmin(x,n)
         xmax = dvmax(x,n)
         xrange = xmax - xmin
         if( xrange .eq. 0.0d0 ) then
            write(*,*) '<<< dvchftx:: No x--range: Aborting. >>>'
            return
         end if
         call dvlt(x,xw,n,-1.0d0,2.0d0 / (xmax - xmin),xmin)
         call techek(xw,n,ltrace)

         dx_min = dvdmax(xw,n)
         if( dx_min .eq. 0.0d0 ) then
            write(*,*) '<<< dvchftx:: Degenerate x--values: '//
     *                 'Aborting. >>>'
            return
         end if

         dth_min = asin(min(1.0d0,dx_min))
         n_max = lnintd(pi / dth_min)
         lg2   = min(lnintd(log(1.0d0 * n_max) / log(2.0d0)) * 1.0d0,
     *               max_lg2_n * 1.0d0)
         lg2   = max(lg2,min_lg2_n * 1.0d0)
         n_max = lnintd(2.0d0 ** lg2) + 1.0d0
         dth   = pi / (n_max - 1.0d0)
         nmax = n_max

         if( ltrace ) then
            write(*,*) '>>> n, nmax'
            write(*,*) n, nmax
            write(*,*) '>>> xrange, dx_min, lg2 ...'
            write(*,*) xrange, dx_min, lg2
            write(*,*) '>>> dthmin, dth ...'
            write(*,*) dth_min, dth
         end if
   
         call dvramp(costh,pi,-dth,nmax)
         call dvfapl(costh,costh,dcos,nmax)
         call techek(costh,nmax,ltrace)

         call dvinqn(f,xw,ff,costh,n,nmax,0.0d0,0.0d0,2*(nl+1))

         lnl = min(int(lg2),nl)
         do 300 ic = 0 , nc
            inc = 1
            ldth = dth

             call dvcheb(costh,t_n,nmax,ic)
            call dvvm(t_n,ff,t_n,nmax)
            do 200 l = lnl , 1 , -1
               result(l) = 0.0d0
               do 100 j = 1 , nmax - inc , inc
                  result(l) = result(l) + (t_n(j) + t_n(j+inc))
 100           continue
               result(l) = result(l) * 
     *                     0.5d0 * ldth
               inc = 2 * inc
               ldth = 2.0d0 * ldth
 200        continue 
            if( ltrace ) then
               call dvdump(result,lnl,'results',6)
               if( lnl .gt. 2 ) then
                  call dvrsd(result,rsd,lnl)
                  call dvdump(rsd,lnl-2,'convergence',6)
               end if
            end if
            call dvrex(result,lnl,2.0d0)
            if( ltrace ) then
               call dvdump(result,1,'extrapolated',6)
            end if
            c(ic) = result(1) / pi
            if( ic .ne. 0 ) then
               c(ic) = 2.0d0 * c(ic)
            end if
 300     continue
         
         return

      end
c
c     Checks/imposes [-1 ... 1] ...
c
      subroutine techek(v,n,verbose)

         implicit      none

         integer       n
         real*8        v(n)

         logical       verbose

         if( v(1) .ne. -1.0d0 ) then
            if( verbose ) then
               write(*,*) '>>> techek:: setting left value ...'
            end if
            v(1) = -1.0d0
         end if
         if( v(n) .ne. +1.0d0 ) then
            if( verbose ) then
               write(*,*) '>>> techek:: setting right value ...'
            end if
            v(n) = +1.0d0
         end if

         return

      end
c
c     Returns +1, -1 if v strictly non--decreasing, non--increasing,
c     0, otherwise
c
      integer function isdvmono(v,n)
      
         implicit        none

         integer         n
         real*8          v(n)

         integer         j

         isdvmono = 0
         if( n .ge. 2 ) then
            if(    v(2) .gt. v(1) ) then
               do 100 j = 3 , n
                  if( v(j) .le. v(j-1) ) then
                     return
                  end if
 100           continue
               isdvmono = 1
            else if( v(2) .lt. v(1) ) then
               do 200 j = 3 , n
                  if( v(j) .ge. v(j-1) ) then
                     return
                  end if
 200           continue
               isdvmono = -1
            end if
         end if

         return

      end
c
c     Multi--segment Chebyshev fitting ... 
c
c     Assumes monotonically increasing x ...
c
      subroutine dvmchft(x,f,xw,fw,n,c,max_c,nc,xseg,nseg)

         implicit        none

         real*8          dvmin,      dvmax,     drange
         integer         isdvmono

         integer         n,          nc,        nseg,
     *                   max_c
         real*8          x(n),       f(n),
     *                   xw(n),      fw(n),
     *                   c(0:max_c,*),    
     *                   xseg(nseg+1)

         integer         maxseg
         parameter     ( maxseg = 100 )
         integer         indseg(maxseg+1)
         real*8          seg_fuzz
         parameter     ( seg_fuzz = 1.0d-10 )

         real*8          xmin,       xmax,      xrange,
     *                   dx,         lxmin,     lxmax

         integer         iseg,       j,         nn
   
         logical         ltrace
         parameter     ( ltrace = .false. )

         if( 1 .le. nseg   .and.   nseg .le. maxseg ) then
            if( isdvmono(x,n) .ne. 1 ) then 
               write(*,*) '<<< dvmchft:: x(...) must be '//
     *                    'non--decreasing. >>>'
               if( ltrace ) then
                  call dvdump(x,n,'x(...) supplied to dvmchft',6)
               end if
               nseg = 0 
               return
            end if
            xmin = dvmin(x,n)
            xmax = dvmax(x,n)
            xrange = xmax - xmin
            xseg(1) = xmin
            indseg(1) = 1
            xseg(nseg + 1) = xmax
            indseg(nseg + 1) = n 
            if( isdvmono(xseg,nseg+1) .ne. 1 ) then
               write(*,*) '<<< dvmchft:: xseg(...) must be '//  
     *                    'non--decreasing. Aborting ... >>>'
               if( ltrace ) then
                  call dvdump(xseg,nseg+1,'xseg(...) in dvmchft',6)
               end if
               nseg = 0
               return
            end if
            iseg = 2
            do 100 j = 2 , n - 1
               if( iseg .gt. nseg ) go to 200
               dx = x(j) - xseg(iseg)
               if( dx .ge. 0.0d0  .or.  
     *             abs(dx) .le. seg_fuzz * (x(j+1) - x(j)) ) then
                  indseg(iseg) = j
                  xseg(iseg)   = x(j)
                  iseg = iseg + 1
               end if
 100        continue
 200        continue
            do 300 iseg = 1 , nseg
               j = indseg(iseg)
               nn = indseg(iseg+1) - j + 1
               call dvchftx(x(j),f(j),xw,fw,nn,1,c(0,iseg),nc,
     *                      lxmin,lxmax)
               if( ltrace ) then
                  write(*,*) 'segment ',iseg
                  call dvdump(c(0,iseg),nc+1,'coefficients',6)
               end if
 300        continue
         else  
            write(*,*) '<<< dvmchft:: Too many or few segments: ',
     *                 nseg,'. >>>'
         end if
            
         return

      end
c
c     Multi segment Chebyshev evaluator ...
c
      subroutine dvmechb(x,f,n,xseg,nseg,c,max_c,nc)

         implicit        none

         logical         drange

         integer         n,        nseg,     max_c,      nc
         real*8          x(n),     f(n),     xseg(nseg+1),
     *                   c(0:max_c,*)

         integer         iseg,     j

         logical         ltrace
         parameter     ( ltrace = .false. )

         if( ltrace ) then
            call dvdump(xseg,nseg+1,'xseg',6)
         end if
         do 200 j = 1 , n
            do 100 iseg = 1 , nseg
               if( drange(xseg(iseg),x(j),xseg(iseg+1),1.0d-10) )  
     *            then
                  call dvechb(x(j),f(j),1,xseg(iseg),xseg(iseg+1),
     *                        c(0,iseg),nc)
                  go to 200
               end if
 100        continue
 200     continue

         return

      end
c
c     Basic logarithmic transform ...
c
      subroutine dvlnt(v1,v2,x0,n)

         implicit      none

         integer       n
         real*8        v1(n),       v2(n)
         real*8        x0

         integer       j

         do 100 j = 1 , n
            v2(j) = log(v1(j) - x0)
 100     continue

         return

      end 
c
c     Signum (characteristic function) ... this version uses intrinsic
c     sign and thus maps 0.0 -> 1.0.
c
      subroutine dvsig(v1,v2,n)

         implicit    none

         integer     n
         real*8      v1(n),      v2(n)
   
         integer     j

         do 10 j = 1 , n
            v2(j) = sign(1.0d0,v1(j))
 10      continue 

         return

      end
c
c     Signum (characteristic function) ... maps 0.0 to 0.0.
c
      subroutine dvsig0(v1,v2,n)

         implicit    none

         integer     n
         real*8      v1(n),      v2(n)
   
         integer     j

         do 10 j = 1 , n
            if(      v1(j) .lt. 0.0d0 ) then
               v2(j) = -1.0d0 
            else if( v1(j) .gt. 0.0d0 ) then
               v2(j) =  1.0d0
            else
               v2(j) =  0.0d0 
            end if
 10      continue 

         return

      end
c
c     Sign of successive differences ...
c
      subroutine dvdsig(v1,v2,n)

         implicit       none

         integer        n
         real*8         v1(n),       v2(n)

         real*8         v1j,         rdv
         integer        j

         real*8         fuzz
         parameter    ( fuzz = 1.0d-12 )

         do 10 j = 1 , n - 1
            v1j = v1(j)
            rdv = v1(j+1) - v1j
            if( rdv .ne. 0.0d0 ) then
               rdv = rdv / abs(v1j) 
            end if
            if( abs(rdv) .ge. fuzz ) then
               v2(j) = sign(1.0d0,rdv)
            else
               v2(j) = 0.0d0
            end if
 10      continue
         v2(n) = 0.0d0 

         return

      end
c
c     "Automatic" break-point setter for oscillatory functions.
c
c     x assumed monotonically increasing.
c
      subroutine axseg0(x,y,w1,w2,n,xseg,nseg)
      
         implicit       none

         intrinsic      dabs

         real*8         dvsum

         integer        n,          nseg

         real*8         x(n),       y(n),       w1(n),
     *                  w2(n)
         real*8         xseg(*)

         integer        i,          j,
     *                  nnext,      nprev  
         logical        mergef,     mergeb

         integer        max_nseg,                min_per_seg
         parameter    ( max_nseg = 1000,         min_per_seg =  8 )

         logical        ltrace
         parameter    ( ltrace = .false. )

         call dvd1nu(x,y,w1,n)
         call dvsig(w1,w1,n)
         call dvdsig(w1,w2,n)
         call dvfapl(w2,w1,dabs,n)

         nseg = 1
         xseg(nseg) = x(1)
         w2(nseg) = 1   
         do 100 j =  2 , n - 1
            if( w1(j) .eq. 1.0d0 ) then
               nseg = nseg + 1
               if( nseg .eq. max_nseg ) then
                  if( ltrace ) then
                     write(*,*) '>>> axseg0:: Too many segments ...'
                  end if
                  nseg = 0
                  return
               end if
               xseg(nseg) = x(j)
               w2(nseg)   = j   
            end if
 100     continue
         xseg(nseg+1) = x(n)
         w2(nseg+1)   = n
         call dvdd1(w2,w1,1.0d0,nseg+1)

         if( ltrace ) then
            write(*,*) 
     *         '>>> axseg0: Before compression',nseg,' segments'
c           call dvdump(xseg,nseg+1,'xseg',6)
c           call dvdump(w2,nseg+1,'xseg indices',6)
c           call dvdump(w1,nseg,'points per segment',6)
         end if

         i = 1
 200     if( i .gt. nseg ) go to 400
            if( w1(i) .lt. min_per_seg ) then
               if( i .gt. 1 ) then
                  nprev = w1(i-1)
               else 
                  nprev = n
               end if
               if( i .lt. nseg ) then
                  nnext = w1(i+1)
               else
                  nnext = n
               end if
               if( nprev .le. nnext ) then
c
c                 Merge backward ...
c
c                 if( ltrace ) then
c                    write(*,*) '>>> Merging backwards ...'
c                 end if

                  xseg(i) = xseg(i+1)
                  w1(i-1) = w1(i-1) + w1(i)
                  do 300 j = i + 1 , nseg
                     xseg(j) = xseg(j+1) 
                     w1(j-1) = w1(j)
 300              continue
                  i = i - 1
               else 
c
c                 Merge forward.
c
c                 if( ltrace ) then
c                    write(*,*) '>>> Merging forwards ...'
c                 end if

                  xseg(i+1) = xseg(i+2)
                  w1(i)     = w1(i) + w1(i+1)
                  do 350 j = i + 2 , nseg 
                     xseg(j) = xseg(j+1) 
                     w1(j-1) = w1(j)
 350              continue
               end if
               nseg = nseg - 1
            else
               i = i + 1
            end if
c           if( ltrace ) then
c              write(*,*) '>>> i = ',i
c              write(*,*)
c              call dvdump(xseg,nseg+1,'xseg',6)
c              call dvdump(w1,nseg,'points per segment',6)
c           end if
         go to 200
 400     continue
         if( ltrace ) then
            write(*,*) 
     *         '>>> axseg0: After compression',nseg,' segments'
c           call dvdump(xseg,nseg+1,'xseg',6)
c           call dvdump(w2,nseg+1,'xseg indices',6)
c           call dvdump(w1,nseg,'points per segment',6)
         end if
         if( dvsum(w1,nseg) + 1 .ne. n ) then
            write(*,*) '>>> axseg0:: confusion in routine ...'
            nseg = 0
         end if

         return

      end

c-----------------------------------------------------------------------
c
c     Given cheby expansion coefficients for normalized interval
c     (-1 : 1), returns derivative coefficients for also for 
c     evaluation on normalized interval ... 
c
c-----------------------------------------------------------------------

      subroutine dvchbdc(xmin,xmax,c,nc,dc,ndc)

         implicit         none

         integer          nc,           ndc
         real*8           c(0:nc),      dc(0:nc)
         real*8           xmin,         xmax

         integer          k

         dc(nc) = 0.0d0
         dc(nc-1) = 2.0d0 * nc * c(nc)
         do 100 k = nc - 1 , 1 , -1
            dc(k-1) = dc(k+1) + 2.0d0 * k * c(k)
 100     continue
         dc(0) = 0.5d0 * dc(0)
         ndc = nc - 1
         call dvsm(dc(0),2.0d0 / (xmax - xmin),dc(0),ndc+1)

         return

      end


c-----------------------------------------------------------------------
c
c     Computes multi-segment derivative coefficients ...
c
c-----------------------------------------------------------------------

      subroutine dvmchbdc(c,max_c,nc,dc,ndc,xseg,nseg)

         implicit         none
         integer          max_c,        nc,      ndc,
     *                    nseg
         real*8           c(0:max_c,*),
     *                    dc(0:max_c,*), 
     *                    xseg(nseg+1)

         integer          iseg

         do 100 iseg = 1 , nseg
            call dvchbdc(xseg(iseg),xseg(iseg+1),c(0,iseg),nc,
     *                   dc(0,iseg),ndc)
 100     continue

         return

      end

c-----------------------------------------------------------------------
c
c     Computes cheby rep of d_ord'th derivative of Cheby rep in c. 
c
c-----------------------------------------------------------------------

      subroutine dvchbdnc(xmin,xmax,c,nc,dc,ndc,d_ord)

         implicit         none

         integer          nc,       ndc,       d_ord
         real*8           c(0:nc),  dc(0:nc)
         real*8           xmin,     xmax

         integer          maxc,     ntc
         parameter      ( maxc = 100 )
         real*8           tc(0:maxc)

         integer          i_ord

            
         if(      d_ord .eq. 0 ) then
            ndc = nc
            call dvcopy(c,dc,ndc+1)
         else if( d_ord .gt. 0  .and.  d_ord .le. nc  .and.  
     *       d_ord .le. maxc ) then
            call dvcopy(c,tc,nc+1)
            ntc = nc
            do 100 i_ord = 1 , d_ord
               call dvchbdc(xmin,xmax,tc,ntc,dc,ndc)
               if( i_ord .lt. d_ord ) then
                  ntc = ndc
                  call dvcopy(dc,tc,ndc+1)
               end if
 100        continue
         else
            write(*,*) '<<< dvchbdnc: d_ord bounds error. >>>'
         end if

         return

      end

c-----------------------------------------------------------------------
c
c     Computes multi-segment d_ord'th order derivative coefficients ...
c
c-----------------------------------------------------------------------

      subroutine dvmchbdnc(c,max_c,nc,dc,ndc,xseg,nseg,d_ord)

         implicit         none
         integer          max_c,        nc,      ndc,
     *                    nseg,         d_ord
         real*8           c(0:max_c,*),
     *                    dc(0:max_c,*), 
     *                    xseg(nseg+1)

         integer          iseg

         do 100 iseg = 1 , nseg
            call dvchbdnc(xseg(iseg),xseg(iseg+1),c(0,iseg),nc,
     *                    dc(0,iseg),ndc,d_ord)
 100     continue

         return

      end

c-----------------------------------------------------------------------
c
c     Admittedly somewhat special purpose for inclusion in dveclib.   
c
c-----------------------------------------------------------------------

      subroutine dvmofa(r,a,m,n)

         implicit      none

         integer       n
         real*8        r(n),       a(n),       m(n)

         integer       j
         
          do 10 j = 1 , n
            m(j) = 0.5d0 * r(j) * (1.0d0 - 1.0d0 / (a(j) * a(j)))
 10      continue

         return

      end

c-----------------------------------------------------------------------
c
c     Exponentially damp tail of vector.
c
c-----------------------------------------------------------------------

      subroutine dvedtl(x,y1,y2,n,xcut,dx)

         implicit       none

         integer        n
         real*8         x(n),      y1(n),      y2(n)

         real*8         xcut,      dx

         real*8         xj
         integer        j

         if( dx .gt. 0.0d0 ) then
            do 100 j = 1 , n
               xj = x(j)
               if( xj .gt. xcut ) then
                  y2(j) = y1(j) * exp( - (xj - xcut) / dx )
               else 
                  y2(j) = y1(j)
               end if
 100        continue
         else
            call dvcopy(y1,y2,n)
         end if

         return

      end

c-----------------------------------------------------------------------
c
c     Exponentially damp head of vector.
c
c-----------------------------------------------------------------------

      subroutine dvedhd(x,y1,y2,n,xcut,dx)

         implicit       none

         integer        n
         real*8         x(n),      y1(n),      y2(n)

         real*8         xcut,      dx

         real*8         xj
         integer        j

         if( dx .gt. 0.0d0 ) then
            do 100 j = 1 , n
               xj = x(j)
               if( xj .lt. xcut ) then
                  y2(j) = y1(j) * exp(  (xj - xcut) / dx )
               else 
                  y2(j) = y1(j)
               end if
 100        continue
         else
            call dvcopy(y1,y2,n)
         end if

         return

      end

c-----------------------------------------------------------------------
c
c     Computes a(r) from mass aspect.
c
c-----------------------------------------------------------------------

      subroutine dvaofm(r,m,a,n)

         implicit      none

         integer       n
         real*8        r(n),       m(n),       a(n)

         real*8        rj
         integer       j
         
          do 10 j = 1 , n
            rj = r(j)
            if( rj .eq. 0.0d0 ) then
               a(j) = 1.0d0
            else
               a(j) = 1.0d0 / sqrt(1.0d0 - 2.0d0 * m(j) / rj)
            end if
 10      continue

         return

      end

c-----------------------------------------------------------------------
c
c     Computes "Complex Coordinate Representation" of sc. field vbls.
c
c     dm/dr = (x + i y) (x - i y).
c
c-----------------------------------------------------------------------

      subroutine   dvct1(pp,pi,a,r,x,y,nr)
      
         implicit       none

         integer        nr
         real*8         pp(nr),       pi(nr),
     *                  a(nr),        r(nr),
     *                  x(nr),        y(nr) 

         real*8         rt2pi
         parameter    ( rt2pi = 2.506628274631000d0 )

         real*8         c1
         integer        i
      
         do 10 i = 1 , nr
            c1   = rt2pi * r(i) / a(i)
            x(i) = c1 * pp(i)
            y(i) = c1 * pi(i)
 10      continue

         return

      end

c-----------------------------------------------------------------------
c
c     Computes m(r) from X(r), Y(r).
c
c-----------------------------------------------------------------------

      subroutine dvmofxy(r,x,y,m,n)

         implicit     none

         integer      n
         real*8       r(n),     x(n),     y(n),     m(n)

         real*8       xjmh,     yjmh
         integer      j

         real*8       tol
         parameter  ( tol = 1.0d-15 )

         if( abs(r(1)) .le. tol ) then
            m(1) = 0.0d0
            do 100 j = 2 , n
               xjmh = 0.5d0 * (x(j) + x(j-1))
               yjmh = 0.5d0 * (y(j) + y(j-1))
               m(j) = m(j-1) + (r(j) - r(j-1)) * (xjmh * xjmh +
     *                                            yjmh * yjmh)
 100        continue
         else
            write(*,*) '<<< dvmofxy:: Error: r(1) .ne. 0.0d0. >>>'
            call dvls(m,0.0d0,n)
         end if

         return

      end

c-----------------------------------------------------------------------
c
c     Computes pp, pi from X, Y.
c
c-----------------------------------------------------------------------

      subroutine dvppfx(x,a,r,pp,n)

         implicit       none

         integer        n
         real*8         x(n),       a(n),        r(n),       pp(n)

         real*8         rt2pim1
         parameter    ( rt2pim1 = 0.3989422804014327d0 )

         integer        j,          j1

         if( r(1) .eq. 0.0d0 ) then
            j1 = 2
            pp(1) = 0.0d0
         else
            j1 = 1
         end if
         do 100 j = j1 , n
            pp(j) = rt2pim1 * a(j) * x(j) / r(j)
 100     continue

         return

      end

      subroutine dvpify(y,a,r,pi,n)

         implicit       none

         real*8         dqfit

         integer        n
         real*8         y(n),       a(n),        r(n),       pi(n)

         real*8         rt2pim1
         parameter    ( rt2pim1 = 0.3989422804014327d0 )

         integer        j,          j1

         if( r(1) .eq. 0.0d0 ) then
            j1 = 2
         else
            j1 = 1
         end if
         do 100 j = j1 , n
            pi(j) = rt2pim1 * a(j) * y(j) / r(j)
 100     continue
         if( r(1) .eq. 0.0d0 ) then
            pi(1) = dqfit(pi,r)
         end if

         return

      end

      double precision function dqfit(y,r)
      
         implicit       none

         real*8         y(*),        r(*)

         real*8         dr1sq,       dr2sq,       diff

         dr1sq = r(2) - r(1)
         dr1sq = dr1sq * dr1sq
         dr2sq = r(3) - r(1)
         dr2sq = dr2sq * dr2sq
         diff  = dr2sq - dr1sq
         if( diff .ne. 0.0d0 ) then
            dqfit = (dr2sq * y(2) - dr1sq * y(3)) / diff
         else
            dqfit = 0.0d0
         end if

         return 

      end

c-----------------------------------------------------------------------
c
c     Mid--point first integral ...
c
c-----------------------------------------------------------------------

      subroutine dvmint(y,x,res,n)

         implicit     none

         integer      n
         real*8       y(n),      x(n),        res(n)
         
         integer      j

         if( n .lt. 1 ) return

         res(1) = 0.0d0
         do 10 j = 2 , n
            res(j) = res(j-1) + 0.5d0 * (x(j) - x(j-1)) * 
     *                                  (y(j) + y(j-1))
 10      continue

         return

      end

c-----------------------------------------------------------------------
c
c     Reverse mid--point first integral ...
c
c-----------------------------------------------------------------------

      subroutine dvrmint(y,x,res,n)

         implicit     none

         integer      n
         real*8       y(n),      x(n),        res(n)
         
         integer      j

         if( n .lt. 1 ) return

         res(n) = 0.0d0
         do 10 j = n - 1 , 1 , -1
            res(j) = res(j+1) - 0.5d0 * (x(j+1) - x(j)) * 
     *                                  (y(j) + y(j+1))
 10      continue

         return

      end

c-----------------------------------------------------------------------
c
c     Computes lapse(r) up to normalization from a.
c
c-----------------------------------------------------------------------

      subroutine dvlofa(r,a,l,w,n)

         implicit      none

         intrinsic     dexp

         integer       n
         real*8        r(n),       a(n),       l(n),         w(n)

         call dvvm(a,a,w,n)
         call dvsa(w,-1.0d0,w,n)
         call dvvdx(w,r,w,n)
         call dvmint(w,r,l,n)
         call dvfapl(l,l,dexp,n)
         call dvvm(l,a,l,n)

         return

      end
c
c     Imposes one representation of periodic boundary conditions ...
c
      subroutine dvpbc(v,n)

         implicit       none

         integer        n
         real*8         v(*)

         v(n+1) = v(2)
         v(1)   = v(n)

         return

      end
c
c     Predicate for pbc representation ... 
c
      logical function haspbc(v,n)

         implicit        none

         real*8          v(*)
         integer         n

         haspbc = v(1) .eq. v(n)  .and.  v(n+1) .eq. v(2)

         return

      end
c
c     Returns "number" of nearest 2^p length pbc vector (indices run from
c     1 ... nnpbc() + 1
c
      integer function nnpbc(n)

         implicit        none

         real*8          ldlog2
         integer         lnintd

         integer         n

         nnpbc = 2 ** lnintd(ldlog2(1.0d0 * (n - 1))) + 1

         return

      end
c
c     Uniform trapezioidal (mid--point) rule integrator ...
c
      double precision function dvuti(v,n,d)

         implicit        none
   
         real*8          dvsum

         integer         n
         real*8          v(n)
         real*8          d

         if( n .ge. 2 ) then
            dvuti = d * (dvsum(v(2),n-2) + 0.5d0 * (v(1) + v(n)))
         else 
            dvuti = 0.0d0
         end if

         return

      end

c-----------------------------------------------------------------------
c
c     y2 = y1 * unit roll-off function (polymomial, degree alpha)
c     on a , b.
c
c-----------------------------------------------------------------------

      subroutine dvrolldown(y1,y2,x,n,a,b,alpha)

         implicit      none

         integer       n

         real*8        y1(n),       y2(n),       x(n)
         real*8        a,           b

         integer       alpha

         real*8        dalpha,      tdalm1

         real*8        xi,          zeta,        zetac
         integer       i

         logical       ltrace 
         parameter   ( ltrace = .false. )

         if( alpha .lt. 1 ) then
            if( ltrace ) then
               write(*,*) '>>> dvrolldown:: Specify alpha > 0'
            end if
            call dvcopy(y1,y2,n)
            return
         end if

         if( a .eq. b ) then
            if( ltrace ) then
               write(*,*) '>>> dvrolldown:: Warning: Degenerate '//
     *                    'roll off interval'
            end if
            do 100 i = 1 , n
               if( x(i) .le. a ) then
                  y2(i) = y1(i)
               else
                  y2(i) = 0.0d0
               end if
 100        continue
         else 
            zetac = 1.0d0 / (b - a)
            dalpha = alpha
            if( dalpha .gt. 1.0d0 ) then
               tdalm1 = 2.0d0 ** (dalpha - 1.0d0)
            else 
               tdalm1 = 1.0d0
            end if
            do 200 i = 1 , n
               xi = x(i)
               if(      xi .lt. a ) then
                  y2(i) = y1(i) 
               else if( xi .gt. b ) then
                  y2(i) = 0.0d0
               else
                  zeta = zetac * (xi - a)
                  if( zeta .le. 0.5d0 ) then
                     y2(i) = y1(i) * (1.0d0 - tdalm1 * zeta ** dalpha)
                  else 
                     y2(i) = y1(i) * tdalm1 * (1.0d0 - zeta) ** dalpha
                  end if
               end if
 200        continue
         end if

         return

      end

      subroutine dvrollup(y1,y2,x,n,a,b,alpha)

         implicit      none

         integer       n

         real*8        y1(n),       y2(n),       x(n)
         real*8        a,           b

         integer       alpha

         real*8        dalpha,      tdalm1

         real*8        xi,          zeta,        zetac
         integer       i

         logical       ltrace 
         parameter   ( ltrace = .false. )

         if( alpha .lt. 1 ) then
            if( ltrace ) then
               write(*,*) '>>> dvrollup:: Specify alpha > 0'
            end if
            call dvcopy(y1,y2,n)
            return
         end if

         if( a .eq. b ) then
            if( ltrace ) then
               write(*,*) '>>> dvrollup:: Warning: Degenerate '//
     *                    'roll off interval'
            end if
            do 100 i = 1 , n
               if( x(i) .le. a ) then
                  y2(i) = 0.0d0
               else
                  y2(i) = y1(i)
               end if
 100        continue
         else 
            zetac = 1.0d0 / (b - a)
            dalpha = alpha
            if( dalpha .gt. 1.0d0 ) then
               tdalm1 = 2.0d0 ** (dalpha - 1.0d0)
            else 
               tdalm1 = 1.0d0
            end if
            do 200 i = 1 , n
               xi = x(i)
               if(      xi .lt. a ) then
                  y2(i) = 0.0d0
               else if( xi .gt. b ) then
                  y2(i) = y1(i) 
               else
                  zeta = zetac * (xi - a)
                  if( zeta .le. 0.5d0 ) then
                     y2(i) = y1(i) * tdalm1 * zeta ** dalpha
                  else 
                     y2(i) = y1(i) * (1.0d0 - tdalm1 * 
     *                                (1.0d0 - zeta) ** dalpha)
                  end if
               end if
 200        continue
         end if

         return

      end

c-----------------------------------------------------------------------
c
c    Roll-off at both ends ...
c
c-----------------------------------------------------------------------

      subroutine dvrollboth(v1,v2,x,n,alo,blo,alphlo,ahi,bhi,alphhi)

         implicit          none

         integer           n
         real*8            v1(n),       v2(n),       x(n)
         real*8            alo,         blo,
     *                     ahi,         bhi
         integer           alphlo,      alphhi

         call dvrollup(v1,v2,x,n,alo,blo,alphlo)
         call dvrolldown(v2,v2,x,n,ahi,bhi,alphhi)

         return

      end

c-----------------------------------------------------------------------
c
c    Computes arc length (normalized to 0..1) and curvature for
c    X, Y parametric curve.
c
c-----------------------------------------------------------------------

      subroutine dvxycv(x,y,s,curv,xd,xdd,yd,ydd,n)

         implicit        none 

         integer         n
         real*8          x(n),         y(n),
     *                   s(n),         curv(n),
     *                   xd(n),        xdd(n),
     *                   yd(n),        ydd(n)

         real*8          dx,           dy
         integer         j

         s(1) = 0.0d0
         do 10 j = 2 , n
            dx = x(j) - x(j-1)
            dy = y(j) - y(j-1)
            s(j) = s(j-1) + sqrt(dx * dx + dy * dy)
 10      continue
         if( s(n) .eq. 0.0d0 ) then
            write(*,*) '>>> dvxycv:: Warning: Degenerate curve.'
            call dvls(curv,0.0d0,n)
            return
         else
            call dvsm(s,1.0d0 / s(n),s,n)
         end if

         call dvd1nu(x,s,xd,n)
         call dvd1nu(xd,s,xdd,n)
         call dvd1nu(y,s,yd,n)
         call dvd1nu(yd,s,ydd,n)

         call dvpyth(xd,yd,curv,n)

         call dvvm(xd,ydd,xd,n)
         call dvvsm(yd,xdd,yd,-1.0d0,n)
         call dvva(xd,yd,xd,n)
         call dvvd(xd,curv,curv,n)

         return

      end
c
c     Chebyshev fit--and--replace routine ... needs length 3n work
c     array.
c
      subroutine dvcfar(x,y,w,n)

         implicit       none

         integer        n

         real*8         x(*),       y(*),       w(*)

         integer        max_c,      max_nseg
         parameter    ( max_c = 30, max_nseg  = 1000 )
         real*8         c(0:max_c,max_nseg)

         integer        w1,         w2,         wxseg,        nseg

         w1 = 1
         w2 = w1 + n
         wxseg = w2 + n

         call axseg0(x,y,w(w1),w(w2),n,w(wxseg),nseg)
         call dvmchft(x,y,w(w1),w(w2),n,c,max_c,max_c,w(wxseg),nseg)
         call dvmechb(x,y,n,w(wxseg),nseg,c,max_c,max_c)

         return
      
      end
c
c     Cheby evaluated nth derivative ... needs 3 n length work array ...
c

      subroutine dvchebdn(x,y,dnydxn,w,n,d_ord)

         implicit       none

         integer        n
         integer        d_ord

         real*8         x(*),       y(*),       dnydxn(*),      w(*)

         integer        max_c,      max_nseg
         parameter    ( max_c = 30, max_nseg  = 1000 )
         real*8         c(0:max_c,max_nseg),
     *                  dc(0:max_c,max_nseg)

         integer        w1,         w2,         wxseg,        nseg,
     *                  ndc

         w1 = 1
         w2 = w1 + n
         wxseg = w2 + n

         call axseg0(x,y,w(w1),w(w2),n,w(wxseg),nseg)
         call dvmchft(x,y,w(w1),w(w2),n,c,max_c,max_c,w(wxseg),nseg)
         call dvmchbdnc(c,max_c,max_c,dc,ndc,w(wxseg),nseg,d_ord)
         call dvmechb(x,dnydxn,n,w(wxseg),nseg,dc,max_c,ndc)

         return
      
      end

c-----------------------------------------------------------------------
c
c    Fits values with varying numbers (min_nc ... max_nc) of Cheby
c    coefficients and returns index of best fit (l-inf. and l-2 
c    weighted with w_inf and w_2).
c
c-----------------------------------------------------------------------

      integer function dvchmcft(x,f,xw,fw,n,c,min_nc,max_nc,
     *                          xmin,xmax,f_nrm,w_2,w_inf,fit)

         implicit      none

         real*8        dvnrm2,   dvlinf
         integer       idvmin

         integer       n
         real*8        x(n),     f(n),     xw(n),     fw(n)
         real*8        xmin,     xmax

         integer       min_nc,   max_nc
         real*8        c(0:max_nc)

         real*8        f_nrm,    w_inf,    w_2
         real*8        fit(0:max_nc)

         real*8        fnrm2
         integer       nc

         logical       ltrace
         parameter   ( ltrace = .false. ) 

         if( n .lt. 4 ) then
            write(*,*) '>>> dvchmcft:: Need at least 4 points.'
            dvchmcft = -1
            return
         end if

         fnrm2 = (w_inf + w_2) * f_nrm
         if( fnrm2 .le. 0.0d0  .or.  
     *       w_inf .lt. 0.0d0  .or.  w_2 .lt. 0.0d0 ) then
            dvchmcft = -1
            return
         end if
         if( ltrace ) then
            write(*,*) 'dvchmcft:: Fitting on ',sngl(x(1)),' ... ',
     *                 sngl(x(n)),' with ',min_nc,' ... ',max_nc
            call dvdump(x,n,'x',6)
            call dvdump(f,n,'f',6)
         end if
         do 100 nc = min_nc , max_nc
            call dvchftx(x,f,xw,fw,n,1,c,nc,xmin,xmax)
            call dvechb(x,fw,n,xmin,xmax,c,nc)
            call dvvs(f,fw,fw,n)
            fit(nc) = (w_inf * dvlinf(fw,n) + 
     *                 w_2 * dvnrm2(fw,n)) / fnrm2
 100     continue
         dvchmcft = min_nc + 
     *              idvmin(fit(min_nc),max_nc - min_nc + 1) - 1

         return

      end
c
c     Slower Cheby evaluated nth derivative ... uses routines in 
c     nestcf.c
c

      subroutine dvnestcfdn(x,y,dnydxn,n,d_ord)

         implicit       none

         integer        dvnestcf

         integer        n
         integer        d_ord

         real*8         x(*),       y(*),       dnydxn(*)

         integer        max_c,      max_nseg
         parameter    ( max_c = 8, max_nseg  = 1000 )
         real*8         c(0:max_c,max_nseg),
     *                  dc(0:max_c,max_nseg),
     *                  xseg(max_nseg+1)

         integer        nseg,       ndc

         if( dvnestcf(x,y,n,c,max_c,xseg,nseg) .ne. 0 ) then
            call dvmchbdnc(c,max_c,max_c,dc,ndc,xseg,nseg,d_ord)
            call dvmechb(x,dnydxn,n,xseg,nseg,dc,max_c,ndc)
         else 
            write(*,*) '>>> dvnestcfdn: dvnestcf failed. Returning '//
     *                 'original function.'
            call dvcopy(y,dnydxn,n)
         end if

         return
      
      end

c-----------------------------------------------------------------------
c
c    Computes arc length (normalized to 0..1) and curvature for
c    X, Y parametric curve. Uses Cheby ``smoothing'' at each stage.
c
c-----------------------------------------------------------------------

      subroutine dvxycvs(x,y,s,curv,xd,xdd,yd,ydd,w,n)

         implicit        none 

         integer         n
         real*8          x(n),         y(n),
     *                   s(n),         curv(n),
     *                   xd(n),        xdd(n),
     *                   yd(n),        ydd(n),
     *                   w(*)

         real*8          dx,           dy
         integer         j

         logical         ltrace
         parameter     ( ltrace = .false. )

         s(1) = 0.0d0
         do 10 j = 2 , n
            dx = x(j) - x(j-1)
            dy = y(j) - y(j-1)
            s(j) = s(j-1) + sqrt(dx * dx + dy * dy)
 10      continue
         if( s(n) .eq. 0.0d0 ) then
            write(*,*) '>>> dvxycv:: Warning: Degenerate curve.'
            call dvls(curv,0.0d0,n)
            return
         else
            call dvsm(s,1.0d0 / s(n),s,n)
         end if

         call dvcfar(s,x,w,n)
         if( ltrace ) then
            write(*,*) '>>> Smoothed X ...'
         end if
         call dvcfar(s,y,w,n)
         if( ltrace ) then
            write(*,*) '>>> Smoothed Y ...'
         end if

         call dvd1nu(x,s,xd,n)
         call dvcfar(s,xd,w,n)
         if( ltrace ) then
            write(*,*) '>>> Smoothed dX/ds ...'
         end if
         call dvd1nu(xd,s,xdd,n)
         call dvcfar(s,xdd,w,n)
         if( ltrace ) then
            write(*,*) '>>> Smoothed d2X/ds2 ...'
         end if
         call dvd1nu(y,s,yd,n)
         call dvcfar(s,yd,w,n)
         if( ltrace ) then
            write(*,*) '>>> Smoothed dY/ds ...'
         end if
         call dvd1nu(yd,s,ydd,n)
         call dvcfar(s,ydd,w,n)
         if( ltrace ) then
            write(*,*) '>>> Smoothed d2Y/ds2 ...'
         end if

         call dvpyth(xd,yd,curv,n)

         call dvvm(xd,ydd,xd,n)
         call dvvsm(yd,xdd,yd,-1.0d0,n)
         call dvva(xd,yd,xd,n)
         call dvvd(xd,curv,curv,n)

         return

      end
c
c     1--D wavelet transform ... n = 2^p only 
c
c     v1: input/transform, v2: transform/input, v3: work
c

c     Press routine

      subroutine dv2wt1(v1,v2,v3,n,isign,wtstep)

         implicit      none

         logical       lp2n

         integer       n,       isign
         real*8        v1(n),   v2(n),    v3(n)
         
         external      wtstep

         integer       nn

         call dvcopy(v1,v2,n)

         if( n .lt. 4 ) then
            write(*,*) '>>> dv2wt1:: Vector length ', 
     *                 n, ' insufficient for transform ...'
            return
         end if

         if( .not. lp2n(n) ) then
            write(*,*) '>>> dv2wt1:: Vector length ',
     *                 n, ' not 2^p ...'
            return
         end if

         if( isign .ge. 0 ) then
            nn = n
 100        if( nn .ge. 4 ) then
               call wtstep(v2,v3,nn,isign)
               nn = nn / 2
            go to 100
            end if
         else
            nn = 4
 200        if( nn .le. n ) then
               call wtstep(v2,v3,nn,isign)
               nn = nn * 2
            go to 200
            end if
         end if
          
         return

      end

c     Press routine

      subroutine daub4(v1,v2,n,isign)

         real*8      v1(*),   v2(*)
         integer     n,       isign

         real*8      c0,      c1,      c2,      c3
         parameter ( 
     *               c0 =  0.4829629131445341d0,
     *               c1 =  0.8365163037378079d0,
     *               c2 =  0.2241438680420134d0,
     *               c3 = -0.1294095225512604d0 
     *             )
 
         integer nh,nh1,i,j

         if( n .lt. 4 ) then
            write(*,*) '>>> daub4:: Vector length ', 
     *                 n, ' insufficient for transform ...'
            return
         end if

         nh = n/2
         nh1 = nh+1
         if (isign.ge.0) then
            i = 1
            do j = 1,n-3,2
               v2(i) = c0*v1(j)+c1*v1(j+1)+c2*v1(j+2)+c3*v1(j+3)
               v2(i+nh) = c3*v1(j)-c2*v1(j+1)+c1*v1(j+2)-c0*v1(j+3)
               i = i+1
            enddo
            v2(i) = c0*v1(n-1)+c1*v1(n)+c2*v1(1)+c3*v1(2)
            v2(i+nh) = c3*v1(n-1)-c2*v1(n)+c1*v1(1)-c0*v1(2)
         else
            v2(1) = c2*v1(nh)+c1*v1(n)+c0*v1(1)+c3*v1(nh1)
            v2(2) = c3*v1(nh)-c0*v1(n)+c1*v1(1)-c2*v1(nh1)
            j = 3
            do i = 1,nh-1
              v2(j) = c2*v1(i)+c1*v1(i+nh)+c0*v1(i+1)+c3*v1(i+nh1)
              v2(j+1) = c3*v1(i)-c0*v1(i+nh)+c1*v1(i+1)-c2*v1(i+nh1)
              j = j+2
            enddo
         endif
         call dvcopy(v2,v1,n)

         return

      end
c
c     Front--ends ...
c
      subroutine dv2daub4f(v1,v2,v3,n)

         implicit        none

         real*8          v1(*),       v2(*),       v3(*)
         integer         n

         external        daub4
      
         call dv2wt1(v1,v2,v3,n, 1,daub4)

         return

      end
 
      subroutine dv2daub4r(v1,v2,v3,n)

         implicit        none

         real*8          v1(*),       v2(*),       v3(*)
         integer         n

         external        daub4

         call dv2wt1(v1,v2,v3,n,-1,daub4)

         return

      end
c
c     Top level routines ... input vectors must be in standard pbc
c     form and output vectors must be large enough to accomodate 
c     nearest power of two mesh with periodic extension.
c     (nout = nnpbc(nin))
c
c     Takes advantage of fact that Press's routines are in place ...
c
      subroutine dvdaub4r(yin,xin,yout,xout,wout,nin,nout)

         implicit        none

         logical         haspbc

         real*8          xin(*),        yin(*),
     *                   xout(*),       yout(*),      wout(*)
         integer         nin,           nout

         integer         int_ord 
         parameter     ( int_ord = 4)

         logical         ltrace
         parameter     ( ltrace = .true. )

         if( haspbc(xin,nin)  .and.  haspbc(yin,nin) ) then
            call dvinq2n(yin,xin,yout,xout,nin,nout,int_ord)
            call dv2daub4r(yout(2),yout(2),wout,nout-1)
            call dvpbc(yout,nout)
         else
            if( ltrace ) then
                 write(*,*) '>>> dvdaub4r:: Input vector(s) not in '//
     *                    'standard pbc form ...'
            end if
            nout = -1
         end if

         return

      end 

      subroutine dvdaub4f(yin,xin,yout,xout,wout,nin,nout)

         implicit        none

         logical         haspbc

         real*8          xin(*),        yin(*),
     *                   xout(*),       yout(*),      wout(*)
         integer         nin,           nout

         integer         int_ord 
         parameter     ( int_ord = 4)

         logical         ltrace
         parameter     ( ltrace = .true. )

         if( haspbc(xin,nin)  .and.  haspbc(yin,nin) ) then
            call dvinq2n(yin,xin,yout,xout,nin,nout,int_ord)
            call dv2daub4f(yout(2),yout(2),wout,nout-1)
            call dvpbc(yout,nout)
         else
            if( ltrace ) then
                 write(*,*) '>>> dvdaub4f:: Input vector(s) not in '//
     *                    'standard pbc form ...'
            end if
            nout = -1
         end if

         return

      end 
c
c     Replaces array elements with mags. below relative threshold
c     with 0.0 
c
      subroutine dvthresh(v1,v2,n,thresh)

         implicit         none

         real*8           dvlinf

         real*8           v1(*),      v2(*)
         integer          n
         real*8           thresh

         real*8           cutoff 
         integer          j

         cutoff = abs(thresh) * dvlinf(v1,n)
         do 10 j = 1 , n
            if( abs(v1(j)) .lt. cutoff ) then
               v2(j) = 0.0d0
            else
               v2(j) = v1(j)
            end if
 10      continue

         return

      end
c
c     F F T   Routines.
c

c     Press et al routine.

c
c     Replaces <data> by its discrete Fourier transform, if <isign>
c     is input as 1; or replaces <data> ny <nn> times its inverse 
c     discrete Fourier transform, if <isign> is input as -1. 
c
      subroutine lfour1(data,nn,isign)

         implicit       none

         logical        lp2n
   
         real*8         data(*)
         integer        nn,           isign

         real*8         wr,           wi,
     *                  wpr,          wpi,
     *                  wtemp,        theta,
     *                  tempr,        tempi

         integer        i,            j,
     *                  n,            m,
     *                  istep,        mmax

         if( .not. lp2n(nn) ) then
            write(*,*) '>>> lfour1:: Vector length ',nn,     
     *                 ' not a power of 2.'
            return
         end if

         n=2*nn
         j=1
         do 11 i=1,n,2
           if(j.gt.i)then
             tempr=data(j)
             tempi=data(j+1)
             data(j)=data(i)
             data(j+1)=data(i+1)
             data(i)=tempr
             data(i+1)=tempi
           endif
           m=n/2
1          if ((m.ge.2).and.(j.gt.m)) then
             j=j-m
             m=m/2
           go to 1
           endif
           j=j+m
11       continue
         mmax=2
2        if (n.gt.mmax) then
           istep=2*mmax
           theta=6.28318530717959d0/(isign*mmax)
           wpr=-2.d0*sin(0.5d0*theta)**2
           wpi=sin(theta)
           wr=1.d0
           wi=0.d0
           do 13 m=1,mmax,2
             do 12 i=m,n,istep
               j=i+mmax
               tempr=wr*data(j)-wi*data(j+1)
               tempi=wr*data(j+1)+wi*data(j)
               data(j)=data(i)-tempr
               data(j+1)=data(i+1)-tempi
               data(i)=data(i)+tempr
               data(i+1)=data(i+1)+tempi
12           continue
             wtemp=wr
             wr=wr*wpr-wi*wpi+wr
             wi=wi*wpr+wtemp*wpi+wi
13         continue
           mmax=istep
         go to 2
         endif

         return

      end

c     Press et al routine ...
      
c     Calculates the FT of a set of 2<n> real--valued data points.
c     Replaces <data> by the positive frequency half of its complex 
c     FT. The real-valued first and last components of the complex 
c     transform are returned as elements <data(1)> and <data(2)>
c     respectively. N must be a power of 2. This routine also calculates
c     the inverse transform of a complex data array if it is the           
c     transform of real data. (Result in this case must be multiplied
c     by 1/<n>).

      subroutine lrft (data,n,isign)

         implicit       none

         logical        lp2n
   
         real*8         data(*)
         integer        n,            isign

         real*8         wr,           wi,          wpr,
     *                  wpi,          wtemp,       theta,
     *                  c1,           c2,
     *                  h1r,          h1i,         h2r,
     *                  h2i          
         integer        i1,           i2,          i3,
     *                  i4,           i,           n2p3 

         if( .not. lp2n(n) ) then
            write(*,*) '>>> realft:: Vector length ',n,
     *                 ' not a power of 2.'
         end if

         theta=6.28318530717959d0/2.0d0/dble(n)
         wr=1.0d0
         wi=0.0d0
         c1=0.5d0
         if (isign.eq.1) then
           c2=-0.5d0
           call lfour1(data,n,+1)
           data(2*n+1)=data(1)
           data(2*n+2)=data(2)
         else
           c2=0.5d0
           theta=-theta
           data(2*n+1)=data(2)
           data(2*n+2)=0.0d0
           data(2)=0.0d0
         endif
         wpr=-2.0d0*sin(0.5d0*theta)**2
         wpi=sin(theta)
         n2p3=2*n+3
         do 11 i=1,n/2+1
           i1=2*i-1
           i2=i1+1
           i3=n2p3-i2
           i4=i3+1
           h1r=c1*(data(i1)+data(i3))
           h1i=c1*(data(i2)-data(i4))
           h2r=-c2*(data(i2)+data(i4))
           h2i=c2*(data(i1)-data(i3))
           data(i1)=h1r+wr*h2r-wi*h2i
           data(i2)=h1i+wr*h2i+wi*h2r
           data(i3)=h1r-wr*h2r+wi*h2i
           data(i4)=-h1i+wr*h2i+wi*h2r
           wtemp=wr
           wr=wr*wpr-wi*wpi+wr
           wi=wi*wpr+wtemp*wpi+wi
11       continue
         if (isign.eq.1) then
           data(2)=data(2*n+1)
         else
           call lfour1(data,n,-1)
         endif
         return

      end
c
c     Front--ends ... data arrays must be <n + 2> in length to work
c     properly with Press et al routines. Standard pbc rep. with
c     appropriate right/left circular shifts should work OK.
c
      subroutine dv2rfftf(v1,v2,n)

         implicit        none

         logical         lp2n

         real*8          v1(*),       v2(*)
         integer         n

         call dvcopy(v1,v2,n+2)

         if( n .lt. 4 ) then
            write(*,*) '>>> dv2rfftf:: Input vector too short.'
            return
         end if

         if( lp2n(n) ) then
            call lrft(v2,n / 2,+1)
         else 
            write(*,*) '>>> dv2rfftf:: Vector length ',n, 
     *                 ' not 2 ^ p.'
         end if

         return

      end

      subroutine dv2rfftr(v1,v2,n)

         implicit        none

         logical         lp2n

         real*8          v1(*),       v2(*)
         integer         n

         call dvcopy(v1,v2,n+2)

         if( n .lt. 4 ) then
            write(*,*) '>>> dv2rfftf:: Input vector too short.'
            return
         end if

         if( lp2n(n) ) then
            call lrft(v2,n / 2,-1)
            call dvsm(v2,2.0d0 / n,v2,n)
         else 
            write(*,*) '>>> dv2rfftr:: Vector length ',n, 
     *                 ' not 2 ^ p.'
         end if

         return

      end
c
c
c     Top level FFT routines ... input vectors must be in standard pbc
c     form and output vectors must be large enough to accomodate 
c     nearest power of two mesh with periodic extension.
c     (nout = nnpbc(nin))
c
      subroutine dvfftr(yin,xin,yout,xout,nin,nout)

         implicit        none

         logical         haspbc

         real*8          xin(*),        yin(*),
     *                   xout(*),       yout(*)
         integer         nin,           nout

         integer         int_ord
         parameter     ( int_ord = 4 )

         logical         ltrace
         parameter     ( ltrace = .true. )

         if( haspbc(xin,nin)  .and.  haspbc(yin,nin) ) then
            call dvinq2n(yin,xin,yout,xout,nin,nout,int_ord)
            call dvuclsh(yout,nout+1)
            call dv2rfftr(yout,yout,nout-1)
            call dvucrsh(yout,nout+1)
            call dvpbc(yout,nout)
         else
            if( ltrace ) then
                 write(*,*) '>>> dvfftr:: Input vector(s) not in '//
     *                    'standard pbc form ...'
            end if
            nout = -1
         end if

         return

      end 
c
      subroutine dvfftf(yin,xin,yout,xout,nin,nout)

         implicit        none

         logical         haspbc

         real*8          xin(*),        yin(*),
     *                   xout(*),       yout(*)
         integer         nin,           nout

         integer         int_ord
         parameter     ( int_ord = 4 )

         logical         ltrace,        full_trace
         parameter     ( ltrace = .true.,
     *                   full_trace = .false. )

         if( haspbc(xin,nin)  .and.  haspbc(yin,nin) ) then
            call dvinq2n(yin,xin,yout,xout,nin,nout,int_ord)
            if( full_trace ) then
               write(*,*) '>>> dvfftf:: nin ',nin,' nout ',nout
            end if

            call dvuclsh(yout,nout+1)
            call dv2rfftf(yout,yout,nout-1)
            call dvucrsh(yout,nout+1)
            call dvpbc(yout,nout)
         else
            if( ltrace ) then
                 write(*,*) '>>> dvfftf:: Input vector(s) not in '//
     *                    'standard pbc form ...'
            end if
            nout = -1
         end if

         return

      end 
c     
c     Assumes input vector is dft of real function in standard periodic
c     form, returns vector of amplitudes of fourier coefficents (power 
c     spectrum?). v2 *not* in standard pbc form.
c
      subroutine dvfftm(v1,v2,n1,n2)

         implicit      none

         logical       haspbc

         real*8        v1(*),        v2(*)
         integer       n1,           n2

         integer       j

         logical       ltrace 
         parameter   ( ltrace = .true. )

         if( n1 .lt. 4  .or.  mod((n1 - 1) / 2,2) .ne. 0 ) then
            if( ltrace ) then
               write(*,*) '>>> dvfftm:: Invalid vector length ',
     *                    n1
            end if
            n2 = -1
         end if

         if( haspbc(v1,n1) ) then
            n2 = (n1 - 1) / 2
            do 10 j = 1 , n2
               v2(j) = sqrt(v1(2 * j) ** 2 + 
     *                      v1(2 * j + 1) ** 2)
 10         continue 
         else 
            if( ltrace ) then
               write(*,*) '>>> dvfftm:: Input vector not in '//
     *                   'standard pbc form ...'
            end if
            n2 = -1
         end if

         return

      end

c-----------------------------------------------------------------------
c
c     Generates uniform mesh ... essentially front end to dvramp.
c
c-----------------------------------------------------------------------

      subroutine dvumsh(v,n,v1,vn)

         implicit       none

         integer        n
         real*8         v(*)
         real*8         v1,      vn

         if( n .gt. 0 ) then
            if( n .eq. 1 ) then
               v(1) = v1
            else 
               call dvramp(v,v1,(vn - v1) / (n - 1),n)
               v(n) = vn
            end if
         end if

         return

      end

c-----------------------------------------------------------------------
c
c     Generates geometric (graded) mesh ...
c
c     rho  >  0  -->   grading factor
c     rho  <  0  -->   abs(rho) = dv(n-1) / dv(1)
c
c-----------------------------------------------------------------------

      subroutine dvgmsh(v,n,v1,vn,rho)

         implicit       none

         integer        n
         real*8         v(*)
         real*8         v1,      vn,      rho

         integer        j
         real*8         lrho,    dv

         if(      rho .gt. 0.0d0 ) then
            lrho = rho
         else if( rho .lt. 0.0d0 ) then
            lrho = abs(rho) ** (1 / (n - 2.0d0))
         else
            write(0,*) '<<< dvgmsh:: Error: routine invoked with '//
     *                 'rho = 0.0. >>>'
            return
         end if

         if( n .lt. 0 ) then
               write(0,*) '<<< dvgmsh:: Error routine invoked with '//
     *                    'n < 0. >>>'
         else if( n .eq. 0 ) then
         else if( n .eq. 1 ) then
            v(1) = v1
         else if( lrho .eq. 1.0d0 ) then
            call dvramp(v,v1,(vn - v1) / (n - 1),n)
            v(n) = vn
         else
            dv = (lrho - 1.0d0) * (vn - v1) / 
     *           (lrho ** (n - 1.0d0) - 1.0d0)
            v(1) = v1 
            do 10 j = 2 , n - 1
               v(j) = v(j-1) + dv
               dv = dv * lrho
 10         continue
            v(n) = vn
         end if

         return

      end

c-----------------------------------------------------------------------
c
c     Double precision vector/vector divide with check for zero
c     and evaluation through interpolation/extrapolation in 
c     evaluable results on basis of Taylor expansion of V1(V2).
c
c-----------------------------------------------------------------------

      subroutine dvvdxx(v1,v2,v3,n)

         implicit       none

         integer        n
         real*8         v1(*),      v2(*),       v3(*)

         real*8         dv21,       dv22,        v2j
         integer        off1,       off2,        j

         do 10 j = 1 , n
            if( v2(j) .ne. 0.0d0 ) then
               v3(j) = v1(j) / v2(j)
            end if
 10      continue

         do 20 j = 1 , n
            v2j = v2(j)
            if( v2j .eq. 0.0d0 ) then
               if(      j .eq. 1 ) then
                  off1 = +1
                  off2 = +2
               else if( j .eq. n ) then
                  off1 = -1
                  off2 = -2
               else
                  off1 = -1
                  off2 = +1
               end if
               dv21 = v2(j + off1) - v2j
               dv22 = v2(j + off2) - v2j
               v3(j) = (dv22 * v3(j + off1) - dv21 * v3(j + off2)) /
     *                 (dv22 - dv21)
            end if
 20      continue

         return

      end

c-----------------------------------------------------------------------
c
c     Utility routine for flow--field plotting.  Renormalizes dx, dy
c     arrays so that sqrt(dx^2 + dy^2) = scale in normalized dely 
c     dely units. 
c
c-----------------------------------------------------------------------

      subroutine dvnpyth(dxin,dyin,dxout,dyout,n,delx,dely,scale)

         implicit        none

         integer         n
         real*8          dxin(*),     dyin(*),      
     *                   dxout(*),    dyout(*)
         real*8          delx,        dely,        scale

         real*8          alpha
         integer         j

         do 10 j = 1 , n
            alpha = scale / sqrt( (dxin(j) / delx) ** 2 +
     *                            (dyin(j) / dely) ** 2 )
            dxout(j) = alpha * dxin(j)
            dyout(j) = alpha * dyin(j)
 10      continue

         return

      end

c-----------------------------------------------------------------------
c     
c    Computes zeta[phi] for use in non-minimally coupled investigation.
c
c-----------------------------------------------------------------------

      subroutine dvzeta_of_phi(phi,zeta,xi,n)

         implicit      none        

         real*8        qpi8
         parameter   ( qpi8 =  25.13 2741 2287 1834 d0 )

         integer       n
         real*8        phi(*),     zeta(*)
         real*8        xi

         integer       j
         real*8        m8piz

         m8piz = -qpi8 * xi
         do 10 j = 1 , n
            zeta(j) = 1.0d0 / (1.0d0 + m8piz * phi(j) * phi(j))
 10      continue

         return

      end

c-----------------------------------------------------------------------
c     
c    Computes kappa[ph,pp,zeta] ...
c
c-----------------------------------------------------------------------

      subroutine dvkappa_of_ph_pp_zeta(ph,pp,zeta,r,kappa,xi,n)

         implicit      none        

         real*8        qpi8
         parameter   ( qpi8 =  25.13 2741 2287 1834 d0 )

         integer       n
         real*8        ph(*),     pp(*),     zeta(*),
     *                 r(*),      kappa(*)
         real*8        xi

         integer       j
         real*8        eta2

         eta2 = qpi8 * xi
         do 10 j = 1 , n
            kappa(j) = 1.0d0 / 
     *         (1.0d0 - eta2 * r(j) * zeta(j) * ph(j) * pp(j))
 10      continue

         return

      end

c---------------------------------------------------------------------
c
c     Vector | maximum |.
c
c---------------------------------------------------------------------
c
      double precision function dvamax(v1,n)
c
         real*8      v1(1)
         integer     i, n
c
         dvamax = v1(1)
         do 10 i = 2 , n
            if( abs(v1(i)) .gt. abs(dvamax) ) then
               dvamax = v1(i)
            end if
 10      continue
c
         return
c
      end

c-----------------------------------------------------------------------
c
c     2:1 cubic interpolation of VC to VF ...
c
c-----------------------------------------------------------------------

      subroutine dv2i4(vc,vf,nc)
         
         implicit      none

         integer       nc
         real*8        vc(nc),     vf(*)

         integer       i,          nf

         real*8        lc(4),      cc(4),      rc(4) 
         data          
     *     lc / 0.31250d0,  0.93750d0, -0.31250d0,  0.06250d0 /,
     *     cc /-0.06250d0,  0.56250d0,  0.56250d0, -0.06250d0 /,
     *     rc / 0.06250d0, -0.31250d0,  0.93750d0,  0.31250d0 /

         if( nc .ge. 4 ) then
            nf = 2 * nc - 1
            call dvprln(vc,vf,2,nc)
            vf(2) = lc(1) * vf(1) + lc(2) * vf(3) + 
     *              lc(3) * vf(5) + lc(4) * vf(7)
            do 10 i = 4 , nf - 3 , 2
               vf(i) = cc(1) * vf(i-3) + cc(2) * vf(i-1) +
     *                 cc(3) * vf(i+1) + cc(4) * vf(i+3)
 10         continue
            vf(nf-1) = rc(1) * vf(nf-6) + rc(2) * vf(nf-4) +
     *                 rc(3) * vf(nf-2) + rc(4) * vf(nf)
         else
            write(*,*) '>>> dv2i4:: Too few points for cubic '//
     *                 'interpolation ...'
         end if

         return

      end

c-----------------------------------------------------------------------
c
c     V5 :=   Sum  [ S(i) * Vi ] ... used by multidimensional 2:1 cubic
c           i=1..4                   interpolation ...
c
c-----------------------------------------------------------------------

      subroutine dvsma4(v1,v2,v3,v4,v5,s,n)

         implicit       none

         integer        n
         real*8         v1(n),       v2(n),      v3(n),      v4(n),
     *                  v5(n)
         real*8         s(4)

         integer        j

         do 10 j = 1 , n
            v5(j) = s(1) * v1(j) + s(2) * v2(j) + 
     *              s(3) * v3(j) + s(4) * v4(j)
 10      continue

         return

      end

c-----------------------------------------------------------------------
c
c     Vector-(vector scalar add) multiply ...
c
c-----------------------------------------------------------------------

      subroutine dvsavm(v1,v2,v3,s1,n)

         implicit       none

         integer        n
         real*8         v1(n),    v2(n),     v3(n)
         real*8         s1

         integer        j

         do 10 j = 1 , n
            v3(j) = v1(j) * (v2(j) + s1)
 10      continue

         return

      end

c-----------------------------------------------------------------------
c
c     Vector polynomial evaluation using horner's rule ...
c
c-----------------------------------------------------------------------

      subroutine dvpoly(x,v,n,c,nc,c0)

         implicit       none

         integer        n,        nc
         real*8         x(n),     v(n),      c(nc)
         real*8         c0

         integer        ic

         if( nc .ge. 1 ) then
            call dvsm(x,c(nc),v,n)
            do 100 ic = nc - 1 , 1 , -1
               call dvsavm(x,v,v,c(ic),n)
 100        continue
            if( c0 .ne. 0.0d0 ) then
               call dvsa(v,c0,v,n)
            end if
         end if

         return

      end

c-----------------------------------------------------------------------
c
c     Second and fourth order central filters ...
c
c     Presumably eps should be in range 0 .. 1 ...
c
c-----------------------------------------------------------------------

      subroutine dvcf2(v1,v2,n,eps)

         implicit      none

         integer       n
         real*8        v1(n),       v2(n)
         real*8        eps

         real*8        c0,          cpm1
         integer       j

         cpm1 = 0.25d0 * eps 
         c0 = 1.0d0 - 2.0d0 * cpm1
         do 10 j = 2 , n - 1
            v2(j) = c0 * v1(j) + cpm1 * (v1(j+1) + v1(j-1))
 10      continue

         v2(1) = v1(1)
         v2(n) = v1(n)

         return

      end

      subroutine dvcf4(v1,v2,n,eps)

         implicit      none

         integer       n
         real*8        v1(n),       v2(n)
         real*8        eps

         real*8        c0,          cpm1,        cpm2
         integer       j

         cpm2 = eps / 16.0d0 
         cpm1 = -4.0d0 * cpm2
         c0   =  1.0d0 + 6.0d0 * cpm2
         do 10 j = 3 , n - 2
            v2(j) = c0 * v1(j) + cpm1 * (v1(j+1) + v1(j-1)) +
     *                           cpm2 * (v1(j+2) + v1(j-2))
 10      continue

         v2(1)   = v1(1)
         v2(2)   = v1(2)
         v2(n-1) = v1(n-1)
         v2(n)   = v1(n)

         return

      end

c-----------------------------------------------------------------------
c
c     Returns mesh sizes and starting indices of uniform components 
c     of general non uniform mesh ...
c
c-----------------------------------------------------------------------

      subroutine dvumfcn(v,n,dv,st,nm,fuzz)

         implicit        none

         integer         n
         real*8          v(n)
         integer         nm
         real*8          dv(*)
         integer         st(*)
         real*8          fuzz

         real*8          fdv
         integer         j

         if( n .gt. 1 ) then
            nm = 1
            st(nm) = 1
            dv(nm) = v(2) - v(1)
            fdv = fuzz * dv(nm)
            do 10 j = 2 , n - 1
               if( abs(dv(nm) - (v(j+1) - v(j))) .gt. fdv ) then
                  nm = nm + 1
                  st(nm) = j
                  dv(nm) = v(j+1) - v(j)
               end if
 10         continue
            st(nm + 1) = n
         else 
            nm = 0
         end if

         return

      end

c-----------------------------------------------------------------------
c
c     "Power law" transformation.  Intended for strictly monotonic x1.
c
c-----------------------------------------------------------------------

      subroutine dvpowtr(x1,x2,n,p)

         implicit      none

         real*8        dvmin,     dvmax

         integer       n
         real*8        x1(n),     x2(n)
         real*8        p

         real*8        xmin,      xmax,      lp,      dxm1,     
     *                 xp,        xpp
         integer       j

         xmin = dvmin(x1,n)
         xmax = dvmax(x1,n)
         if( xmax .le. xmin ) then
            write(*,*) '>>> dvpowtr: No x-range.'
            call dvcopy(x1,x2,n)
            return
         end if
         dxm1 = 1.0d0 / (xmax - xmin)
         if(      p .gt. 0.0d0 ) then
            lp =  p
         else if( p .lt. 0.0d0 ) then
            lp = -p
         else
            write(*,*) '>>> dvpowtr: Exponent (p) must be non zero.'
            call dvcopy(x1,x2,n)
            return
         end if
    
         do 10 j = 1 , n
            xp = dxm1 * (x1(j) - xmin)
            if( p .gt. 0.0d0 ) then
               xpp = xp ** lp
            else 
               xpp = 1.0d0 - (1.0d0 - xp) ** lp
            end if
            x2(j) = xpp * (xmax - xmin) + xmin
 10      continue

         return

      end

c-----------------------------------------------------------------------
c
c    Power law transformation on selected x-range followed by interp-
c    olation (hard-wired order) to original x-values.
c
c-----------------------------------------------------------------------

      subroutine dvvpowtr(y1,x1,y2,x2,n,p,rx1min,rx1max)

         implicit         none

         real*8           dvmin,        dvmax
      
         integer          n
         real*8           y1(n),        x1(n),
     *                    y2(n),        x2(n)
         real*8           p,            rx1min,        rx1max

         real*8           x1min,        x1max,         dxm1,
     *                    lp,           xp,            xpp
         integer          j

         integer          nintrp
         parameter      ( nintrp = 4 )


         x1min = dvmin(x1,n)
         x1max = dvmax(x1,n)
         if( rx1min .lt. x1min   .or.   rx1max .gt. x1max ) then
            write(*,*) '>>> dvvpowtr: Range error.'
            write(*,*) '>>> dvvpowtr: Actual, requested limits follow.'
            write(*,*) sngl(x1min),  sngl(x1max)
            write(*,*) sngl(rx1min), sngl(rx1max)
         go to 90
         end if
         dxm1 = 1.0d0 / (rx1max - rx1min)
         if(      p .gt. 0.0d0 ) then
            lp = p
         else if( p .lt. 0.0d0 ) then
            lp = -p
         else 
            write(*,*) '>>> dvvpowtr: Exponent (p) must be non zero.'
         go to 90
         end if
         
         do 10 j = 1 , n
            if( x1(j) .le. rx1min  .or.  x1(j) .ge.  rx1max ) then
               x2(j) = x1(j)
            else   
               xp = dxm1 * (x1(j) - rx1min)
               if( p .gt. 0.0d0 ) then
                  xpp = xp ** lp
               else
                  xpp = 1.0d0 - (1.0d0 - xp) ** lp
               end if
               x2(j) = xpp * (rx1max - rx1min) + rx1min
            end if
 10      continue

         call dvinqn(y1,x2,y2,x1,n,n,0.0d0,0.0d0,nintrp)
         call dvcopy(x1,x2,n)

         return

 90      continue
            call dvcopy(x1,x2,n)
            call dvcopy(y1,y2,n)
         return

      end

c-----------------------------------------------------------------------
c
c     Front-end to DVINQN for interpolation of single value ...
c
c-----------------------------------------------------------------------

      double precision function fdvinqn(v,x,n,xbar,nintrp)

         implicit       none

         integer        n,            nintrp
         real*8         v(n),         x(n),
     *                  xbar

         real*8         default_value
         parameter    ( default_value = 0.0d0 )

         real*8         vbarl(1),     xbarl(1)
   
         xbarl(1) = xbar
         call dvinqn(v,x,vbarl,xbarl,n,1,default_value,default_value,
     *               nintrp)
         fdvinqn = vbarl(1)

         return

      end

c-----------------------------------------------------------------------
c
c     Computes best fit coefficients ... negative return value 
c     indicates singular fit.
c
c-----------------------------------------------------------------------

      double precision function clls(x,y,w,n_in,a,b)

         implicit       none

         real*8         dvsum

         integer        n_in
         real*8         y(*),        x(*),        w(*)
         real*8         a,           b

         integer        n
         real*8         sx,          sy,          sxx,
     *                  sxy,         disc,        discm1

         n = abs(n_in)
         sx = dvsum(x,n)
         sy = dvsum(y,n)
         call dvvm(x,x,w,n)
         sxx = dvsum(w,n)
         call dvvm(x,y,w,n)
         sxy = dvsum(w,n)
         disc = n * sxx - sx * sx
         if( disc .ne. 0 ) then
            discm1 = 1.0d0 / disc
            a = discm1 * (sxx * sy - sxy * sx)
            b = discm1 * (n * sxy - sx * sy)
            if( n_in .gt. 0 ) then
               write(*,*) 'clls: a, b: ',a, b
            end if
            clls =  1.0d0
         else
            clls = -1.0d0
         end if 
   
         return

      end

c-----------------------------------------------------------------------
c
c    Returns least-squares linear fit evaluated at x(i) ...
c    No real error checking ... singular fit will return original data.
c
c-----------------------------------------------------------------------

      subroutine dvlls(x,y,yfit,n)

         implicit       none

         real*8         clls

         integer        n
         real*8         y(n),        x(n),        yfit(n)

         real*8         a,           b

         if( clls(x,y,yfit,n,a,b) .ge. 0.0d0 ) then 
            call dvsm(x,b,yfit,n)
            call dvsa(yfit,a,yfit,n)
         else
            call dvcopy(y,yfit,n)
         end if 
   
         return

      end

c-----------------------------------------------------------------------
c
c    Computes deviation from least-squares linear fit.  
c    dy := y - fit.
c
c-----------------------------------------------------------------------

      subroutine dvdlls(x,y,dy,n)

         implicit       none

         real*8         clls

         integer        n
         real*8         y(n),        x(n),        dy(n)

         real*8         a,           b

         if( clls(x,y,dy,n,a,b) .ge. 0.0d0 ) then 
            call dvsm(x,b,dy,n)
            call dvsa(dy,a,dy,n)
            call dvvs(y,dy,dy,n)
         else
            call dvcopy(y,dy,n)
         end if 
   
         return

      end

c---------------------------------------------------------------------
c
c     Given uniform mesh origin, spacing and length, x0, dx and nx,
c     and target coordinate xp, returns grid index, kl, of left
c     edge of interpolation molecule and offsets, s(0:p-1), of 
c     "knot" points from xp.
c
c---------------------------------------------------------------------

      subroutine cmposo(x0,dx,nx,xp,p,kl,s)

         implicit       none

         logical        rngfuzz
         integer        lnintd 

         integer        nx,        p,         i
         real*8         x0,        dx,        xp,        xl

         integer        kl
         real*8         s(0:p-1)

         real*8         fuzz
         parameter    ( fuzz = 1.0d-8 )

         if( .not. rngfuzz(x0,xp,x0 + (nx - 1) * dx,fuzz)  .or. 
     *       p .gt. nx ) then
            kl = 0
            return
         end if

         xl = min(max(x0,xp - 0.5d0 * (p - 1) * dx),x0 + (nx - p) * dx)
         kl = lnintd((xl - x0) / dx + 1.0d0)
         xl = x0 + (kl - 1) * dx
         do i = 0 , p - 1
            s(i) = (xp - xl) / dx - i
         end do

         return

      end

c---------------------------------------------------------------------
c
c     Fuzzy range predicate ...
c
c---------------------------------------------------------------------

      logical function rngfuzz(xmin,x,xmax,fuzz)

         implicit       none

         real*8         xmin,    x,    xmax,    fuzz

         if(  (xmin .le. x  .and.  x .le. xmax)      .or.
     *        abs(xmin - x) .le. max(fuzz,fuzz * xmin)  .or.
     *        abs(x - xmax) .le. max(fuzz,fuzz * xmax)    ) then
            rngfuzz = .true.
         else
            rngfuzz = .false.
         end if

         return

      end

c---------------------------------------------------------------------
c
c     pth-order polynomial interpolation in uniform mesh (origin x0,
c     spacing dx, length nx) function y to coordinate xp.  ydef 
c     returned if xp not in mesh domain.
c
c---------------------------------------------------------------------

      double precision function fdvumint(y,nx,x0,dx,xp,p,ydef)

         implicit      none

         real*8        uflipn

         integer       nx
         real*8        y(nx)

         real*8        x0,         dx,        xp,      ydef
         integer       p
         
         integer       pmax
         parameter   ( pmax = 100 )
         real*8        s(0:pmax-1)

         integer       kl
         
         call cmposo(x0,dx,nx,xp,p,kl,s)
         if( kl .le. 0 ) then
            fdvumint = ydef
         else
            fdvumint = uflipn(y(kl),s,p)
         end if

         return

      end

c---------------------------------------------------------------------
c
c     History: FLIPN 
c
c     Low-level routine for polynomial interpolation in uniform
c     mesh.  Straightforward implementation of Nevilles' algorithm.
c     Input y(0:p-1), s(0:p-1), where s(i) = (xp - x(0)) / dx
c     for interpolation to xp.
c
c---------------------------------------------------------------------

      double precision function uflipn(y,s,p)

         implicit      none

         integer       p
         real*8        y(0:p-1),       s(0:p-1)

         integer       pmax
         parameter   ( pmax = 100 )
         real*8        pp(0:pmax-1)

         real*8        mkm1 
         integer       j,               k

         do k = 0 , p - 1
            pp(k) = y(k)
         end do
         do k = 1 , p - 1
            mkm1 = -1.0d0 / k
            do j = 0 , p - (k + 1)
               pp(j) = mkm1 * (s(j+k) * pp(j) - s(j) * pp(j+1))
            end do
         end do

         uflipn = pp(0)

         return

      end

c---------------------------------------------------------------------
c
c     Forward and backward quadratic extrapolation routines ...
c
c---------------------------------------------------------------------

      subroutine dvquadexf(v,n)

         implicit         none

         integer          n
         real*8           v(n)

         if( n .lt. 4 ) then
            write(0,*) 'dvquadexf: Warning---n < 4.'
         else
            v(n) = 3.0d0 * (v(n-1) - v(n-2)) + v(n-3)
         end if

         return
      
      end

      subroutine dvquadexb(v,n)

         implicit         none

         integer          n
         real*8           v(n)

         if( n .lt. 4 ) then
            write(0,*) 'dvquadexb: Warning---n < 4.'
         else
            v(1) = 3.0d0 * (v(2) - v(3)) + v(4)
         end if

         return
      
      end

      subroutine dvquadex(v,n)

         implicit         none

         integer          n
         real*8           v(n)

         if( n .lt. 4 ) then
            write(0,*) 'dvquadex: Warning---n < 4.'
         else
            call dvquadexf(v,n)
            call dvquadexb(v,n)
         end if

         return
      
      end

c---------------------------------------------------------------------
c
c     Vector-vector border copy ...
c
c---------------------------------------------------------------------

      subroutine dvbcopy(v1,v2,n)

         implicit        none

         integer         n
         real*8          v1(n),      v2(n)


         v2(1) = v1(1)
         v2(n) = v1(n)

         return

      end 

c---------------------------------------------------------------------
c
c     Simple-minded maximum locator ...
c
c---------------------------------------------------------------------

      subroutine dvlocmx(x,v,n,xmx,vmx,nmx) 

         implicit        none

         real*8          dvlinf

         integer         n,                nmx
         real*8          x(n),             v(n),             
     *                   xmx(*),           vmx(*)

         real*8          dvmn
         real*8          fuzz 
         parameter     ( fuzz = 1.0d-4 )

         integer         increasing,       decreasing
         parameter     ( increasing = 0,   decreasing = 1 )
         integer         imx,              j,
     *                   curr_state,       prev_state

         if( n .eq. 0 ) then
            nmx = 0
            return
         end if
         if( n .eq. 1 ) then
            nmx = 1
            xmx(1) = x(1)
            vmx(1) = v(1)
            return
         end if

         dvmn = fuzz * dvlinf(v,n)
         nmx = 0
         prev_state = increasing
         do j = 2 , n
            if( abs(v(j) - v(j-1)) .le. dvmn ) then
               curr_state = prev_state
            else if(  v(j) .lt. v(j-1) ) then
               curr_state = decreasing
            else 
               curr_state = increasing
            end if
            if( curr_state .eq. decreasing   .and. 
     *          prev_state .eq. increasing ) then
               nmx = nmx + 1
               xmx(nmx) = x(j - 1)
               vmx(nmx) = v(j - 1)
               if( (j - 1) .gt. 1  .and. (j - 1) .lt. n ) then
                  call quadex(x(j-2),v(j-2),xmx(nmx),vmx(nmx))
               end if
            end if
            prev_state = curr_state
         end do
         if( curr_state .eq. increasing ) then
            nmx = nmx + 1
            xmx(nmx) = x(n)
            vmx(nmx) = v(n)
         end if

         return

      end

c---------------------------------------------------------------------
c
c     Simple-minded miniumum locator ...
c
c---------------------------------------------------------------------

      subroutine dvlocmn(x,v,n,xmn,vmn,nmn)

         implicit        none

         real*8          dvlinf

         integer         n,                nmn
         real*8          x(n),             v(n),             
     *                   xmn(*),           vmn(*)

         real*8          dvmn
         real*8          fuzz 
         parameter     ( fuzz = 1.0d-4 )

         integer         increasing,       decreasing
         parameter     ( increasing = 0,   decreasing = 1 )
         integer         imn,              j,
     *                   curr_state,       prev_state

         if( n .eq. 0 ) then
            nmn = 0
            return
         end if
         if( n .eq. 1 ) then
            nmn = 1
            vmn(1) = v(1)
            return
         end if

         dvmn = fuzz * dvlinf(v,n)
         nmn = 0
         prev_state = decreasing
         do j = 2 , n
            if( abs(v(j) - v(j-1)) .le. dvmn ) then
               curr_state = prev_state
            else if(  v(j) .lt. v(j-1) ) then
               curr_state = decreasing
            else 
               curr_state = increasing
            end if
            if( curr_state .eq. increasing   .and. 
     *          prev_state .eq. decreasing ) then
               nmn = nmn + 1
               xmn(nmn) = x(j - 1)
               vmn(nmn) = v(j - 1)
               if( (j - 1) .gt. 1  .and. (j - 1) .lt. n ) then
                  call quadex(x(j-2),v(j-2),xmn(nmn),vmn(nmn))
               end if
            end if
            prev_state = curr_state
         end do
         if( curr_state .eq. decreasing ) then
            nmn = nmn + 1
            xmn(nmn) = x(n)
            vmn(nmn) = v(n)
         end if

         return

      end

c---------------------------------------------------------------------
c
c     Performs quadratic fit to (x(i),y(i)), i = 1, 2, 3
c     (xex,yex) of extrema of quadratic ...
c
c---------------------------------------------------------------------

      subroutine quadex(x,y,xex,yex)

         implicit    none

         real*8      x(*),      y(*)
         real*8      xex,       yex

         real*8      m,         p,      den

         m = x(1) - x(2) 
         p = x(3) - x(2)
         if( m .ne. 0.0d0  .and.  p .ne. 0.0d0  .and. 
     *      (m - p) .ne. 0 ) then
            den = (y(1) - y(2)) * p + (y(2) - y(3)) * m
            if( den .ne. 0.0d0 ) then
               xex = 0.5d0 * ((y(1) - y(2)) * p * p + 
     *                        (y(2) - y(3)) * m * m) /
     *               den
               yex = y(1) * xex * (xex - p) / (m * (m - p)) +
     *               y(2) * (xex - m) * (xex - p) / (m * p) +
     *               y(3) * (xex - m) * xex / ((p - m) * p)
               xex = x(2) + xex
            else 
            write(0,*) 'quadex: Degenerate y values ...', 
     *                 y(1), y(2), y(3)
            end if
         else
            write(0,*) 'quadex: Degenerate x values ...', 
     *                 x(1), x(2), x(3)
         end if

         return

      end

c---------------------------------------------------------------------
c
c     V2 = Linear(V1) such that V2(1) = V2MIN and V2(N) = V2MAX.
c
c---------------------------------------------------------------------

      subroutine dvscale(v1,v2,n,v2min,v2max)

         implicit    none

         real*8      dvmin,     dvmax

         integer     n
         real*8      v1(n),     v2(n)
         real*8      v2min,     v2max

         real*8      v1min,     v1max,    v1range

         v1min = dvmin(v1,n)
         v1max = dvmax(v1,n)
         v1range = v1max - v1min
         if( v1range .eq. 0.0d0 ) then
            call dvcopy(v1,v2,n)
         else
            call dvlt(v1,v2,n,
     *                v2min,(v2max - v2min) / v1range,v1min)
         end if
         return

      end

c-----------------------------------------------------------------------
c
c     Tanh-based remapping routine ...
c
c-----------------------------------------------------------------------

      subroutine dvtanhremap(x,v,n,x0,xepsi,xdelta)

         implicit        none

         integer         n
         real*8          x(n),        v(n)
         real*8          x0,          xepsi,        xdelta

         real            rescale
         integer         i

         if( xepsi .gt. 0.0d0  .and.  xdelta .gt. 0.0d0 ) then
            rescale = log(xepsi) / xdelta
            do i = 1 , n
               v(i) = 0.5d0 * (1.0d0 - tanh(rescale * (x(i) - x0)))
            end do
         else 
            call dvcopy(x,v,n)
         end if

         return

      end 

c---------------------------------------------------------------------
c
c     O(h^2)  and  O(h^4) half-step extrapolation routines ...
c
c---------------------------------------------------------------------

      subroutine dvhsex2(vn,vnm1,vnph,n)

         implicit        none

         integer         n
         real*8          vn(n),       vnm1(n),      vnph(n)

         real*8          cn,          cnm1
         parameter     ( cn = 1.5d0,  cnm1 = -0.5d0 )

         integer         j

         do j = 1 , n
            vnph(j) = cn * vn(j) + cnm1 * vnm1(j)
         end do

         return

      end

c---------------------------------------------------------------------

      subroutine dvhsex4(vn,vnm1,vnm2,vnm3,vnph,n)

         implicit        none

         integer         n
         real*8          vn(n),       vnm1(n),      vnm2(n),
     *                   vnm3(n),     vnph(n)

         real*8          cn,          cnm1,         cnm2,       cnm3
         parameter     ( cn   =  2.1875d0,
     *                   cnm1 = -2.1875d0,
     *                   cnm2 =  1.3125d0,
     *                   cnm3 = -0.3125d0
     *                 )

         integer         j

         do j = 1 , n
            vnph(j) = cn   * vn(j)   + cnm1 * vnm1(j) + 
     *                cnm2 * vnm2(j) + cnm3 * vnm3(j)
         end do

         return

      end

c---------------------------------------------------------------------
c
c     O(h^2)  and  O(h^4) full-step extrapolation routines ...
c
c---------------------------------------------------------------------

      subroutine dvfsex2(vn,vnm1,vnp1,n)

         implicit        none

         integer         n
         real*8          vn(n),       vnm1(n),      vnp1(n)

         real*8          cn,          cnm1
         parameter     ( cn = 2.0d0,  cnm1 = -1.0d0 )

         integer         j

         do j = 1 , n
            vnp1(j) = cn * vn(j) + cnm1 * vnm1(j)
         end do

         return

      end

c---------------------------------------------------------------------

      subroutine dvfsex4(vn,vnm1,vnm2,vnm3,vnp1,n)

         implicit        none

         integer         n
         real*8          vn(n),       vnm1(n),      vnm2(n),
     *                   vnm3(n),     vnp1(n)

         real*8          cn,          cnm1,         cnm2,       cnm3
         parameter     ( cn   =  4.0d0,
     *                   cnm1 = -6.0d0,
     *                   cnm2 =  4.0d0,
     *                   cnm3 = -1.0d0
     *                 )

         integer         j

         do j = 1 , n
            vnp1(j) = cn   * vn(j)   + cnm1 * vnm1(j) + 
     *                cnm2 * vnm2(j) + cnm3 * vnm3(j)
         end do

         return

      end

c---------------------------------------------------------------------
c
c     Returns maximum deviation between two vectors relativized 
c     by maximum l-inf norm.
c
c---------------------------------------------------------------------

      double precision function dvmaxrd(v1,v2,n)

         implicit        none

         real*8          dvlinf

         integer         n
         real*8          v1(n),         v2(n)

         real*8          nrmv12,        nrmdev

         integer         i

         logical         ltrace
         parameter     ( ltrace = .false. )

         if( n .gt. 1 ) then
            nrmv12 = max(dvlinf(v1,n),dvlinf(v2,n))
            nrmdev = abs(v1(1) - v2(1))
            do i = 2 , n 
               nrmdev = max(nrmdev,abs(v1(i) - v2(i)))
            end do
            if( nrmv12 .eq. 0.0d0 ) then
               dvmaxrd = nrmdev
            else
               dvmaxrd = nrmdev / nrmv12
            end if
         else 
            dvmaxrd = 0.0d0
         end if
   
         if( ltrace ) then      
            write(0,*) 'dvmaxrd: Returning ', dvmaxrd
         end if

         return

      end
