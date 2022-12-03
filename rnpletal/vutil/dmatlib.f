C-----------------------------------------------------------------------
C
C     D O U B L E    P R E C I S I O N    M A T R I X    R O U T I N E S
C
C-----------------------------------------------------------------------

C-----------------------------------------------------------------------
C
C     Matrix-matrix add.
C
C-----------------------------------------------------------------------
C
      SUBROUTINE DMMA(A1,A2,A3,M,N)
C
         IMPLICIT       NONE
C
         INTEGER        M,        N
         REAL*8         A1(M,N),  A2(M,N),  A3(M,N)
C
         INTEGER        I,        J
C
         DO 20 J = 1 , N
            DO 10 I = 1 , M
               A3(I,J) = A1(I,J) + A2(I,J)
 10         CONTINUE
 20      CONTINUE
C
         RETURN
C
      END
C
C-----------------------------------------------------------------------
C
C     Matrix-matrix subtract.
C
C-----------------------------------------------------------------------
C
      SUBROUTINE DMMS(A1,A2,A3,M,N)
C
         IMPLICIT       NONE
C
         INTEGER        M,        N
         REAL*8         A1(M,N),  A2(M,N),  A3(M,N)
C
         INTEGER        I,        J
C
         DO 20 J = 1 , N
            DO 10 I = 1 , M
               A3(I,J) = A1(I,J) - A2(I,J)
 10         CONTINUE
 20      CONTINUE
C
         RETURN
C
      END
C
C-----------------------------------------------------------------------
C
C     Matrix-matrix multiply.
C
C-----------------------------------------------------------------------
C
      SUBROUTINE DMMM(A1,A2,A3,M,N)
C
         IMPLICIT       NONE
C
         INTEGER        M,        N
         REAL*8         A1(M,N),  A2(M,N),  A3(M,N)
C
         INTEGER        I,        J
C
         DO 20 J = 1 , N
            DO 10 I = 1 , M
               A3(I,J) = A1(I,J) * A2(I,J)
 10         CONTINUE
 20      CONTINUE
C
         RETURN
C
      END
C
C-----------------------------------------------------------------------
C
C     Matrix-matrix divide.
C
C-----------------------------------------------------------------------
C
      SUBROUTINE DMMD(A1,A2,A3,M,N)
C
         IMPLICIT       NONE
C
         INTEGER        M,        N
         REAL*8         A1(M,N),  A2(M,N),  A3(M,N)
C
         INTEGER        I,        J
C
         DO 20 J = 1 , N
            DO 10 I = 1 , M
               A3(I,J) = A1(I,J) / A2(I,J)
 10         CONTINUE
 20      CONTINUE
C
         RETURN
C
      END
C
C-----------------------------------------------------------------------
C
C     Matrix l2-norm.
C
C-----------------------------------------------------------------------
C
      DOUBLE PRECISION FUNCTION DMNRM2(A,M,N)
C
         IMPLICIT       NONE
C
         INTEGER        M,        N
         REAL*8         A(M,N)
C
         INTEGER        I,        J
C
         DMNRM2 = 0.0D0
         DO 20 J = 1 , N
            DO 10 I = 1 , M
               DMNRM2 = DMNRM2 + A(I,J) * A(I,J)
 10         CONTINUE
 20      CONTINUE
         DMNRM2 = SQRT(DMNRM2 / (M * N))
C
         RETURN
C
      END
C
C-----------------------------------------------------------------------
C
C     Load matrix with scalar.
C
C-----------------------------------------------------------------------
C
      SUBROUTINE DMLS(A,S,M,N)
C
         IMPLICIT       NONE
C
         INTEGER        M,        N
         REAL*8         A(M,N)
         REAL*8         S
C
         INTEGER        I,        J
C
         DO 20 J = 1 , N
            DO 10 I = 1 , M
               A(I,J) = S
 10         CONTINUE
 20      CONTINUE
C
         RETURN
C
      END
C
C-----------------------------------------------------------------------
C
C     Dumps matrix labelled with LABEL on UNIT.
C
C-----------------------------------------------------------------------
C
      SUBROUTINE DMDUMP(A,M,N,LABEL,UNIT)
C
         IMPLICIT       NONE
C
         INTEGER        M,        N,        UNIT
         REAL*8         A(M,N)
         CHARACTER*(*)  LABEL
         INTEGER        I,        J,        ST
C
         IF( N .GT. 0  .AND.  M .GT. 0 ) THEN
            WRITE(UNIT,100) LABEL
 100        FORMAT( /' <<< ',A,' >>>'/)
            DO 200 J = 1 , N
               ST = 1
 110           CONTINUE
                  WRITE(UNIT,120) ( A(I,J) , I = ST , MIN(ST+3,M))
 120              FORMAT(' ',4(1PE19.10))
                  ST = ST + 4
               IF( ST .LE. M ) GO TO 110
               IF( J .LT. N ) THEN
                  WRITE(UNIT,140)
 140              FORMAT(' ')
               END IF
 200        CONTINUE
C
         END IF
C
         RETURN
C
      END
C
C-----------------------------------------------------------------------
C
C     Loads vector V into Ith row ( A(I,1) ... A(I,N) ) of matrix A.
C
C-----------------------------------------------------------------------
C
      SUBROUTINE DMRLOD(A,V,M,N,I)
C
         IMPLICIT       NONE
C
         INTEGER        M,        N,        I
         REAL*8         A(M,N),   V(N)
C
         INTEGER        J
C
         DO 10 J = 1 , N
            A(I,J) = V(J)
 10      CONTINUE
C
         RETURN
C
      END
C
C-----------------------------------------------------------------------
C
C     Stores Ith row of A in V.
C
C-----------------------------------------------------------------------
C
      SUBROUTINE DMRSTO(A,V,M,N,I)
C
         IMPLICIT       NONE
C
         INTEGER        M,        N,        I
         REAL*8         A(M,N),   V(N)
C
         INTEGER        J
C
         DO 10 J = 1 , N
            V(J) = A(I,J)
 10      CONTINUE
C
         RETURN
C
      END
C
C-----------------------------------------------------------------------
C
C     (Matrix scalar multiply) add (matrix scalar multiply).
C
C-----------------------------------------------------------------------
C
      SUBROUTINE DMSMSA(A1,A2,A3,S1,S2,M,N)
C
         IMPLICIT       NONE
C
         INTEGER        M,        N
         REAL*8         A1(M,N),  A2(M,N),  A3(M,N)
         REAL*8         S1,       S2
C
         INTEGER        I,        J
C
         DO 20 J = 1 , N
            DO 10 I = 1 , M
               A3(I,J) = S1 * A1(I,J) + S2 * A2(I,J)
 10         CONTINUE
 20      CONTINUE
C
         RETURN
C
      END
C
C-----------------------------------------------------------------------
C
C     Matrix-matrix copy.
C
C-----------------------------------------------------------------------
C
      SUBROUTINE DMCOPY(A1,A2,M,N)
C
         IMPLICIT       NONE
C
         INTEGER        M,        N
         REAL*8         A1(M,N),  A2(M,N)
C
         INTEGER        I,        J
C
         DO 20 J = 1 , N
            DO 10 I = 1 , M
               A2(I,J) = A1(I,J)
 10         CONTINUE
 20      CONTINUE
C
         RETURN
C
      END

C-----------------------------------------------------------------------
C
C     Matrix-matrix border copy.
C
C-----------------------------------------------------------------------
C
      SUBROUTINE DMBCOPY(A1,A2,M,N)
C
         IMPLICIT       NONE
C
         INTEGER        M,        N
         REAL*8         A1(M,N),  A2(M,N)
C
         INTEGER        I
C
         DO 10 I = 1 , M
            A2(I,1) = A1(I,1)
            A2(I,N) = A1(I,N)
 10      CONTINUE
         DO 20 I = 1 , N
            A2(1,I) = A1(1,I)
            A2(M,I) = A1(M,I)
 20      CONTINUE
C
         RETURN
C
      END
C
C-----------------------------------------------------------------------
C
C     Load matrix with checkerboard scalar.
C
C-----------------------------------------------------------------------
C
      SUBROUTINE DMZLS(A,S1,S2,M,N)
C
         IMPLICIT       NONE
C
         INTEGER        M,        N
         REAL*8         A(M,N)
         REAL*8         S1,       S2
C
         REAL*8         LS(2)
C
         INTEGER        I,        ISW,
     *                  J,        JSW,        PASS
C
         LS(1) = S1
         LS(2) = S2

         JSW = 1
         DO 30 PASS = 1 , 2
            ISW = JSW
            DO 20 J = 1 , N
               DO 10 I = ISW , M , 2
                  A(I,J) = LS(PASS)
 10            CONTINUE
               ISW = 3 - ISW
 20         CONTINUE
            JSW = 3 - JSW
 30      CONTINUE
C
         RETURN
C
      END
C
C-----------------------------------------------------------------------
C
C     Computes global and red/black l2 norms. (normalized).
C
C-----------------------------------------------------------------------
C
      DOUBLE PRECISION FUNCTION DMZNR2(RBNR2,A,M,N)
C
         IMPLICIT       NONE
C
         INTEGER        M,        N
         REAL*8         A(M,N)
         REAL*8         RBNR2(2)
C
         INTEGER        NRB(2)

         INTEGER        I,        ISW,
     *                  J,        JSW,        PASS
C
         RBNR2(1) = 0.0D0
         RBNR2(2) = 0.0D0
         NRB(1) = ((N+1)/2) * ((M+1)/2) + (N/2) * (M/2)
         NRB(2) = ((N+1)/2) * (M/2) + (N/2) * ((M+1)/2)

         JSW = 1
         DO 30 PASS = 1 , 2
            ISW = JSW
            DO 20 J = 1 , N
               DO 10 I = ISW , M , 2
                  RBNR2(PASS) = RBNR2(PASS) + A(I,J) * A(I,J)
 10            CONTINUE
               ISW = 3 - ISW
 20         CONTINUE
            JSW = 3 - JSW
 30      CONTINUE
C
         IF( M * N .GT. 0 ) THEN
            DMZNR2 = SQRT((RBNR2(1) + RBNR2(2)) / (M * N) )
         END IF
         DO 40 PASS = 1 , 2
            IF( NRB(PASS) .GT. 0 ) THEN
               RBNR2(PASS) = SQRT(RBNR2(PASS)  / NRB(PASS) )
            ELSE
               RBNR2(PASS) = 0.0D0
            END IF
 40      CONTINUE

         RETURN
C
      END

C----------------------------------------------------------------------------
C
C     Intializes matrix to "ramp" function (bi--linear function of
C     indices).
C
C----------------------------------------------------------------------------

      SUBROUTINE DMRAMP(A,D1,D2,X10,DX1,X20,DX2)

         IMPLICIT     NONE

         INTEGER      D1, D2

         REAL*8       A(D1,D2)

         REAL*8       X10, DX1, X20, DX2

         REAL*8       BASE
         INTEGER      I1, I2

         BASE = X10 + X20
         DO 20 I2 = 1 , D2
            DO 10 I1 = 1 , D1
               A(I1,I2) = BASE + (I1 - 1) * DX1
 10         CONTINUE
            BASE = BASE + DX2
 20      CONTINUE

         RETURN

      END
C
C-----------------------------------------------------------------------
C
C     Matrix function application.
C
C-----------------------------------------------------------------------

      SUBROUTINE DMFAPL(A,FOFA,M,N,F)

         INTEGER      M, N
         REAL*8       A(M,N), FOFA(M,N)

         EXTERNAL     F
         REAL*8       F

         INTEGER      I, J

         DO 20 J = 1 , N
            DO 10 I = 1 , M
               FOFA(I,J) = F(A(I,J))
 10         CONTINUE
 20      CONTINUE

         RETURN

      END

C-----------------------------------------------------------------------
C
C     Matrix sum.
C
C-----------------------------------------------------------------------
C
      DOUBLE PRECISION FUNCTION DMSUM(A,M,N)
C
         IMPLICIT       NONE
C
         INTEGER        M,        N
         REAL*8         A(M,N)
C
         INTEGER        I,        J
C
         DMSUM = 0.0D0
         DO 20 J = 1 , N
            DO 10 I = 1 , M
               DMSUM = DMSUM + A(I,J)
 10         CONTINUE
 20      CONTINUE
C
         RETURN
C
      END
C
C-----------------------------------------------------------------------
C
C     Matrix minimum/maximum and l--infinity norm.
C
C-----------------------------------------------------------------------

      DOUBLE PRECISION FUNCTION DMMIN(A,M,N)

         IMPLICIT     NONE

         INTEGER      M, N

         REAL*8       A(M,N)

         INTEGER      I, J

         DMMIN = A(1,1)
         DO 20 J = 1 , N
            DO 10 I = 1 , M
              DMMIN = MIN(DMMIN,A(I,J))
 10         CONTINUE
 20      CONTINUE

         RETURN

      END

      DOUBLE PRECISION FUNCTION DMMAX(A,M,N)

         IMPLICIT     NONE

         INTEGER      M, N

         REAL*8       A(M,N)

         INTEGER      I, J

         DMMAX = A(1,1)
         DO 20 J = 1 , N
            DO 10 I = 1 , M
              DMMAX = MAX(DMMAX,A(I,J))
 10         CONTINUE
 20      CONTINUE

         RETURN

      END

      DOUBLE PRECISION FUNCTION DMLINF(A,M,N)

         IMPLICIT     NONE

         INTEGER      M, N

         REAL*8       A(M,N)

         INTEGER      I, J

         DMLINF = ABS(A(1,1))
         DO 20 J = 1 , N
            DO 10 I = 1 , M
              DMLINF = MAX(DMLINF,ABS(A(I,J)))
 10         CONTINUE
 20      CONTINUE

         RETURN

      END
C
C-----------------------------------------------------------------------
C
C     Double to single conversion.
C
C-----------------------------------------------------------------------

      SUBROUTINE DMDTOS(DM,SM,D1,D2)

         IMPLICIT      NONE

         INTEGER       D1, D2

         REAL*8        DM(D1,D2)
         REAL*4        SM(D1,D2)

         INTEGER       I1, I2

         DO 20 I2 = 1 , D2
            DO 10 I1 = 1 , D1
               SM(I1,I2) = DM(I1,I2)
 10         CONTINUE
 20      CONTINUE

         RETURN

      END

C-----------------------------------------------------------------------
C
C     Scales double precision matrix (if possible) to integer rep.
C     where range of data corresponds (is binned) to IMAX - IMIN + 1
C     equally spaced sub--ranges.
C
C-----------------------------------------------------------------------

      SUBROUTINE DMISCL(A,IA,M,N,IMIN,IMAX)

         IMPLICIT     NONE

         INTEGER      M,            N
         REAL*8       A(M,N)
         INTEGER      IA(M,N)
         INTEGER      IMIN,         IMAX

         REAL*8       AMIN,         AMAX,        DAM1
         INTEGER      I,            J

         IF( IMAX .EQ. IMIN ) THEN
            DO 20 J = 1 , N
               DO 10 I = 1 , M
                  IA(I,J) = IMIN
 10            CONTINUE
 20         CONTINUE
            RETURN
         END IF

         AMIN = A(1,1)
         AMAX = A(1,1)

         DO 40 J = 1 , N
            DO 30 I = 1 , M
               AMIN = MIN(A(I,J),AMIN)
               AMAX = MAX(A(I,J),AMAX)
 30         CONTINUE
 40      CONTINUE

         IF( AMIN .EQ. AMAX ) THEN
            DO 60 J = 1 , N
               DO 50 I = 1 , M
                  IA(I,J) = (IMAX - IMIN) / 2
 50            CONTINUE
 60         CONTINUE
         ELSE
            DAM1 = 1.0D0 * (IMAX - IMIN + 1) / (AMAX - AMIN)
            DO 80 J = 1 , N
               DO 70 I = 1 , M
                  IA(I,J) = MIN(IMAX,IMIN +
     *                             INT( DAM1 * (A(I,J) - AMIN) ))
 70            CONTINUE
 80         CONTINUE
         END IF

         RETURN

       END

C---------------------------------------------------------------------------
C
C     Defines A as F(x,y) on cartesian product of X and Y.
C
C---------------------------------------------------------------------------

      subroutine dmfcp(a,x,y,nx,ny,f)

         implicit      NONE

         real*8        f
         external      f

         integer       nx,      ny
         real*8        a(nx,ny),
     *                 x(nx),   y(ny)

         integer       i,       j

         do 20 j = 1 , ny
            do 10 i = 1 , nx
               a(i,j) = f(x(i),y(j))
 10         continue
 20      continue

         return

      end

C-----------------------------------------------------------------------
C
C     2--array prolongation.
C
C-----------------------------------------------------------------------

      SUBROUTINE D2PRLN(A1,A2,D1INC,D2INC,D1,D2)

         IMPLICIT       NONE

         INTEGER        D1,        D2,
     *                  D1INC,     D2INC
         REAL*8         A1(D1,D2),
     *                  A2(D1INC * (D1 - 1) + 1,
     *                     D2INC * (D2 - 1) + 1)

         INTEGER        J

         DO 10 J = 0 , D2 - 1
            CALL DVPRLN(A1(1,J+1),A2(1,D2INC*J+1),D1INC,D1)
 10      CONTINUE

         RETURN

      END
C
C-----------------------------------------------------------------------
C
C     2--array injection.
C
C-----------------------------------------------------------------------

      SUBROUTINE D2INJ(A1,A2,D1INC,D2INC,D1,D2)

         IMPLICIT      NONE

         INTEGER       D1,        D2,
     *                 D1INC,     D2INC
         REAL*8        A1(D1INC * (D1 - 1) + 1,
     *                    D2INC * (D2 - 1) + 1),
     *                 A2(D1,D2)

         INTEGER       J,         D1A2

         D1A2 = D1INC * (D1 - 1) + 1
         DO 10 J = 0 , D2 - 1
            CALL DVINJ(A1(1,D2INC*J+1),A2(1,J+1),D1INC,D1A2)
 10      CONTINUE

         RETURN

      END
c
c     2--array uniform bi--trapezoidal integrator ...
c
      double precision function d2uti(a,v,d1,d2,dd1,dd2)

         implicit      none

         real*8        dvuti

         integer       d1,     d2
         real*8        a(d1,d2),
     *                 v(d2)
         real*8        dd1,    dd2

         integer       i2

         if( d1 .ge. 2  .and.  d2 .ge. 2 ) then
            do 100 i2 = 1 , d2
               v(i2) = dvuti(a(1,i2),d1,dd1)
 100        continue 
            d2uti = dvuti(v,d2,dd2)
         else
            d2uti = 0.0d0
         end if

         return
      
      end
c
c     Romberg integration (maxl levels) on uniform 
c     2^n1 + 1  x   2^n2 + 1 mesh.
c
      double precision function d2rom(a,wa,wv2,d1,d2,dd1,dd2,wvl,maxl)

         implicit      none

         real*8        d2uti
         logical       dmp2np1
         integer       dmilog2

         integer       d1,       d2,       maxl
         real*8        a(d1,d2), wa(d1,d2),
     *                 wv2(d2),  wvl(maxl)
         real*8        dd1,      dd2

         real*8        ldd1,     ldd2
         integer       l,        nl,       mnln2, 
     *                 ld1,      ld2

         logical       ltrace
         parameter   ( ltrace = .false. )

         if( dmp2np1(d1) .and. dmp2np1(d2) ) then
            ld1  = d1
            ld2  = d2
            ldd1 = dd1 
            ldd2 = dd2
            call dmcopy(a,wa,d1,d2)
            mnln2 = min(dmilog2(d1 - 1),dmilog2(d2 - 1))
            nl     = min(mnln2 + 1,maxl)
            if( ltrace ) then
               write(*,*) '>>> d2rom: Minimum log2:',mnln2
               write(*,*) '>>> d2rom: Number of levels:',nl
            end if
            do 100 l = nl , 1 , -1
               wvl(l) = d2uti(wa,wv2,ld1,ld2,ldd1,ldd2)
               if( ltrace ) then
                  write(*,*) '>>> d2rom: Level',nl-l+1,
     *                       ': ',sngl(wvl(l))
               end if
               if( l .gt. 1 ) then
                  ld1 = (ld1 - 1) / 2 + 1 
                  ld2 = (ld2 - 1) / 2 + 1 
                  call d2inj(wa,wa,2,2,ld1,ld2)
                  ldd1 = 2.0d0 * ldd1
                  ldd2 = 2.0d0 * ldd2
               end if
 100        continue
            call dvrex(wvl,nl,2.0d0)
            d2rom = wvl(1)
         else
            d2rom = 0.0d0
         end if

         return

      end

c-----------------------------------------------------------------------
c
c     Bi-quadratic interplotation of uniform 2-mesh function, origin
c     (orig1,orig2), mesh increments (h1, h2) to vector fv with 
c     associated coordinates (c1v(i),c2v(i)).  fv(i) = fvdef if 
c     point i is out of bounds ...
c
c-----------------------------------------------------------------------

      subroutine dm2vqi(fm,dm1,dm2,orig1,orig2,h1,h2,fvdef,
     *                  fv,c1v,c2v,dv1)

         implicit       none

         real*8         dm2fqi

         integer        dm1,   dm2,   dv1
         real*8         fm(dm1,dm2),  fv(dv1),  c1v(dv1), c2v(dv1)
         real*8         orig1, orig2, h1,  h2,  fvdef

         integer        iv

         do iv = 1 , dv1
            fv(iv) = dm2fqi(fm,dm1,dm2,orig1,orig2,h1,h2,fvdef,
     *                      c1v(iv),c2v(iv))
         end do 

         return

      end

c-----------------------------------------------------------------------
c
c     Returns single interpolated value ...
c   
c-----------------------------------------------------------------------

      double precision function dm2fqi(fm,dm1,dm2,orig1,orig2,h1,h2,
     *                                 fdef,c1,c2)

         implicit       none

         real*8         f2quai
         integer        lnintd

         integer        dm1,   dm2
         real*8         fm(dm1,dm2)
         real*8         orig1, orig2, h1,  h2,  fdef,  c1,   c2

         real*8         h1m1,  h2m1,  s1,  s2
         integer        io1,   io2

         real*8         fuzz
         parameter    ( fuzz = 1.0d-8 )

         h1m1 = 1.0d0 / h1
         h2m1 = 1.0d0 / h2
         
         io1 = max(min(1 + lnintd(h1m1 * (c1 - orig1)),dm1 - 1),2)
         s1  = h1m1 * (c1 - orig1) - io1 + 1
         io2 = max(min(1 + lnintd(h2m1 * (c2 - orig2)),dm2 - 1),2)
         s2  = h2m1 * (c2 - orig2) - io2 + 1
         if( abs(s1) - fuzz .gt. 1.0d0  .or.  
     *       abs(s2) - fuzz .gt. 1.0d0 ) then
            dm2fqi = fdef
         else
            dm2fqi = f2quai(fm,dm1,dm2,io1,io2,s1,s2)
         end if

         return

      end

c-----------------------------------------------------------------------
c
c     Bi-quadratic interpolation in uniform mesh function f(d1,d2) to
c     f(io1+s1,io2+s2).
c
c     No error checking: assumes 1 .le. io<1|2> .le. d<1|2>, 
c     if abs(s1) or abs(s2) > 1, then value will be extrapolated.
c
c-----------------------------------------------------------------------

      double precision function f2quai(f,d1,d2,io1,io2,s1,s2)
         
         implicit      none 
         
         integer       d1,        d2
         real*8        f(d1,d2)

         integer       io1,       io2
         real*8        s1,        s2

         real*8        ft(-1:1),  cs(-1:1),  s1sq,  s2sq
         integer       i

         s1sq = s1 * s1
         cs(-1) = 0.5d0 * (-s1 + s1sq)
         cs( 0) = 1.0d0 - s1sq
         cs( 1) = 0.5d0 * ( s1 + s1sq)
         do i = -1 , 1
            ft(i) = 
     *              cs(-1) * f(io1-1,io2+i)  +
     *              cs( 0) * f(io1  ,io2+i)  +
     *              cs( 1) * f(io1+1,io2+i)  
         end do
         s2sq = s2 * s2
         cs(-1) = 0.5d0 * (-s2 + s2sq)
         cs( 0) = 1.0d0 - s2sq
         cs( 1) = 0.5d0 * ( s2 + s2sq)
         f2quai = cs(-1) * ft(-1) + cs(0) * ft(0) + cs(1) * ft(1)

         return

      end

C-----------------------------------------------------------------------
C
C     History: DMISCL ... AMIN, AMAX supplied as input parameters 
C     rather than being computed automatically ...
C
C     Scales double precision matrix (if possible) to integer rep.
C     where range of data corresponds (is binned) to IMAX - IMIN + 1
C     equally spaced sub--ranges.
C
C-----------------------------------------------------------------------

      subroutine dm2i(a,ia,m,n,amin,amax,imin,imax)

         implicit     none

         integer      m,            n
         real*8       a(m,n)
         integer      ia(m,n)
         real*8       amin,         amax
         integer      imin,         imax

         real*8       dam1
         integer      i,            j

         if( imax .eq. imin ) then
            do j = 1 , n
               do i = 1 , m
                  ia(i,j) = imin
               end do
            end do
            return
         end if

         if( amin .eq. amax ) then
            do j = 1 , n
               do i = 1 , m
                  ia(i,j) = (imax - imin) / 2
               end do
            end do
         else
            dam1 = 1.0d0 * (imax - imin + 1) / (amax - amin)
            do j = 1 , n
               do i = 1 , m
                 ia(i,j) = max(imin,
     *                         min(imax,imin +
     *                                  int( dam1 * (a(i,j) - amin) )))
               end do
            end do
         end if

         return

       end

C-----------------------------------------------------------------------
C
C     Matrix extrema ...
C
C-----------------------------------------------------------------------

      subroutine dmextrema(a,d1,d2,amin,imin1,imin2,amax,imax1,imax2)

         implicit     none

         integer      d1,           d2
         real*8       a(d1,d2)
         real*8       amin,         amax
         integer      imin1,        imin2,     imax1,     imax2

         integer      i1,           i2

         if( d1 .le. 0  .or.  d2 .le.  0 ) then
            return
         end if
         imin1 = 1
         imin2 = 1
         amin = a(imin1,imin2)
         imax1 = 1
         imax2 = 1
         amax = a(imax1,imax2)

         do i2 = 1 , d2
            do i1 = 1 , d1
               if( a(i1,i2) .lt. amin ) then
                  imin1 = i1
                  imin2 = i2
                  amin = a(imin1,imin2)
               end if
               if( a(i1,i2) .gt. amax ) then
                  imax1 = i1
                  imax2 = i2
                  amax = a(imax1,imax2)
               end if
            end do
         end do

         return

       end

c-----------------------------------------------------------------------
c
c     Write 2-matrix formatted by row ...
c
c-----------------------------------------------------------------------

      subroutine dmwritefr(a,d1,d2,u)

         implicit         none

         integer          d1,      d2,      u
         real*8           a(d1,d2)

         integer          i1,      i2

         do i2 = 1 , d2
            write(u,*) ( a(i1,i2), i1 = 1 , d1 )
         end do
   
         return

      end

c-----------------------------------------------------------------------
c
c     Read 2-matrix formatted by row ...
c
c-----------------------------------------------------------------------

      subroutine dmreadfr(a,d1,d2,u)

         implicit         none

         integer          d1,      d2,      u
         real*8           a(d1,d2)

         integer          i1,      i2

         do i2 = 1 , d2
            read(u,*) ( a(i1,i2), i1 = 1 , d1 )
         end do
   
         return

      end

c-----------------------------------------------------------------------
c
c     a3 = log10(abs(a1 - a2))
c
c-----------------------------------------------------------------------

      subroutine dmlog10absd(a1,a2,a3,d1,d2)

         implicit         none

         integer          d1,      d2
         real*8           a1(d1,d2), 
     *                    a2(d1,d2),
     *                    a3(d1,d2)

         real*8           log10_zero
         parameter      ( log10_zero = -100.0d0 )

         real*8           absd
         integer          i1,      i2

         do i2 = 1 , d2
            do i1 = 1 , d1
               absd = abs(a1(i1,i2) - a2(i1,i2))
               if( absd .eq. 0.0d0 ) then
                  a3(i1,i2) = log10_zero
               else 
                  a3(i1,i2) = log10(absd)
               end if
            end do
         end do
   
         return

      end

C---------------------------------------------------------------------------
C
C     2--array half-weighted 2:1 coarsening with border injection.
C
C---------------------------------------------------------------------------

      subroutine dmrshw(uf,uc,nxc,nyc)
 
         implicit       none

         integer        nxc,                nyc
         real*8         uf(2 * nxc - 1, 2 * nyc - 1),
     *                  uc(nxc,nyc)
 
         integer        ic,       jc,       if,       jf
 
         do jc = 2 , nyc - 1
            jf = 2 * jc - 1
            do ic = 2 , nxc - 1
               if = 2 * ic - 1
               uc(ic,jc) = 0.5d0 * (uf(if,jf) + 0.25d0 *
     &                     (uf(if+1,jf) + uf(if-1,jf) +
     &                      uf(if,jf+1) + uf(if,jf-1)))
            end do
         end do
         do ic = 1 , nxc
            uc(ic,1  ) = uf(2 * ic - 1,1  )
            uc(ic,nyc) = uf(2 * ic - 1,2 * nyc - 1)
         end do
         do jc = 1 , nyc
            uc(1  ,jc) = uf(1,  2 * jc - 1)
            uc(nxc,jc) = uf(2 * nxc - 1,2 * jc - 1)
         end do
 
         return
 
      end

c---------------------------------------------------------------------------
c
c     2 : 1 linear interpolation of coarse 3--function UC to fine
c     function UF.
c
c---------------------------------------------------------------------------

      subroutine dmlint(uc,uf,nxc,nyc)
       
         implicit       none
 
         integer        nxc,      nyc
         real*8         uc(nxc,nyc),        
     *                  uf(2 * nxc - 1,2 * nyc - 1)
 
         integer        nxf,      nyf
         integer        ic,       jc,       if,       jf
 
         nxf = 2 * nxc - 1
         nyf = 2 * nyc - 1

         do jc = 1 , nyc
            jf = 2 * jc - 1
            do ic = 1 , nxc
               uf(2 * ic - 1,jf) = uc(ic,jc)
            end do
         end do
         do jf = 1 , nyf , 2
            do if = 2 , nxf - 1 , 2
               uf(if,jf) = 0.5d0 * (uf(if+1,jf) + uf(if-1,jf))
            end do
         end do
         do jf = 2 , nyf - 1 , 2
            do if = 1 , nxf
               uf(if,jf) = 0.5d0 * (uf(if,jf+1) + uf(if,jf-1))
            end do
         end do
 
         return
 
      end

c--------------------------------------------------------------------------
c
c     Loads border of 2--array with scalar.
c
c--------------------------------------------------------------------------

      subroutine dmlbs(a,s,d1,d2)

         implicit      none

         integer       d1,         d2
         real*8        a(d1,d2)

         real*8        s

         integer       i1,         i2

         do i2 = 1 , d2
            a(1, i2) = s
            a(d1,i2) = s
         end do

         do i1 = 1 , d1
            a(i1,1)  = s
            a(i1,d2) = s
         end do

         return

      end


c---------------------------------------------------------------------------
c     Computes current correction (vc) on coarse grid given current coarse
c     (uc) and fine grid (uf) unknowns.  Uses half-weighted transfer 
c     restriction routine 'dmrshw'.
c---------------------------------------------------------------------------
      subroutine dmrshwcor(uc,uf,vc,nxc,nyc)
       
         implicit       none
 
         integer        nxc,      nyc
         real*8         uc(nxc,nyc),        
     &                  vc(nxc,nyc),        
     &                  uf(2 * nxc - 1,2 * nyc - 1)

         call dmrshw(uf,vc,nxc,nyc)
         call dmms  (uc,vc,vc,nxc,nyc)

         return

      end


c-----------------------------------------------------------------------
c     Local versions of routines in utilmath ...
c-----------------------------------------------------------------------

      double precision function dmdlog2(x)

         implicit        none

         real*8          x

         real*8          ln2m1
         parameter     ( ln2m1 = 1.442695040888963d0 )

         dmdlog2 = ln2m1 * log(x)

         return

      end

      integer function dmilog2(x)

         implicit        none

         real*8          dmdlog2
         integer         dmnintd

         integer         x

         dmilog2 = dmnintd(dmdlog2(1.0d0 * x))

         return

      end
c
c     Predicate: n = 2^p + 1
c
      logical function dmp2np1(n)

         implicit        none

         real*8          dmdlog2
         integer         dmnintd

         integer         n

         dmp2np1 = n .eq. (2 ** dmnintd(dmdlog2(1.0d0 * (n - 1))) + 1)

         return

      end
c
c     Predicate: n = 2^p 
c
      logical function dmp2n(n)

         implicit        none

         real*8          dmdlog2
         integer         dmnintd

         integer         n

         dmp2n = n .eq. (2 ** dmnintd(dmdlog2(1.0d0 * n)))

         return

      end
c-----------------------------------------------------------------------
c     Nearest integer (positive, rounds up).
c-----------------------------------------------------------------------
c
      integer function dmnintd(d)
c
         real*8     d
c
         dmnintd = d
         if( (d - dmnintd) .ge. 0.5d0 ) then
            dmnintd = dmnintd + 1
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

      double precision function dmdnintd(d)
c
         integer   dmnintd

         real*8    d
c
         dmdnintd = dmnintd(d)

         return
c
      end

