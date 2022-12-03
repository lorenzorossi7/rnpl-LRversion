C-----------------------------------------------------------------------
C
C     Returns greatest integer less than or equal to A.
C
C-----------------------------------------------------------------------
C
      INTEGER FUNCTION GINLES(A)
         REAL        A
         REAL        FUZZ
         PARAMETER ( FUZZ = 1.0E-5 )
         IF( A .GE. 0.0E0 ) THEN
            GINLES = IFIX(A + FUZZ)
         ELSE
            GINLES = -LINGES(-A)
         END IF
         RETURN
      END
C
C-----------------------------------------------------------------------
C
C     Returns least integer greater than or equal to A.
C
C-----------------------------------------------------------------------
C
      INTEGER FUNCTION LINGES(A)
         INTEGER     GINLES
         REAL        A
         REAL        FUZZ
         PARAMETER ( FUZZ = 1.0E-5 )
         IF( A .GE. 0.0E0 ) THEN
            LINGES = IFIX(A)
            IF( ABS(A - FLOAT(LINGES)) .GT. FUZZ ) LINGES = LINGES + 1
         ELSE
            LINGES = -GINLES(-A)
         END IF
         RETURN
      END
C
C-----------------------------------------------------------------------
C
C     Given that X has the (base-10) representation (+/-)d.ddd...x 10^p,
C     returns p.
C
C-----------------------------------------------------------------------
C
      INTEGER FUNCTION PREPS(X)
         INTEGER      GINLES
         REAL         X
         PREPS = GINLES(ALOG10(ABS(X)))
         RETURN
      END
C
C-----------------------------------------------------------------------
C
C     Returns integer greatest lower bound of whole number W which is
C     0 mod R.
C
C-----------------------------------------------------------------------
C
      INTEGER FUNCTION RGLBW(W,R)
         INTEGER      R, W
         RGLBW = W - MOD(W,R)
         RETURN
      END
C
C-----------------------------------------------------------------------
C
C     Returns integer least upper bound of whole number W which is
C     0 mod R.
C
C-----------------------------------------------------------------------
C
      INTEGER FUNCTION RLUBW(W,R)
         INTEGER      RGLBW
         INTEGER      R, W
         IF( MOD(W,R) .EQ. 0 ) THEN
            RLUBW = W
         ELSE
            RLUBW = R + RGLBW(W,R)
         END IF
         RETURN
      END
C
C-----------------------------------------------------------------------
C
C     Extension of RGLBW to integers.
C
C-----------------------------------------------------------------------
C
      INTEGER FUNCTION RGLBI(I,R)
         INTEGER      RGLBW, RLUBW
         INTEGER      I, R
         IF( I .GE . 0 ) THEN
            RGLBI = RGLBW(I,R)
         ELSE
            RGLBI = -RLUBW(-I,R)
         END IF
         RETURN
      END
C
C-----------------------------------------------------------------------
C
C     Extension of RLUBW to signed integers.
C
C-----------------------------------------------------------------------
C
      INTEGER FUNCTION RLUBI(I,R)
         INTEGER      RGLBW, RLUBW
         INTEGER      I, R
         IF( I .GE. 0 ) THEN
            RLUBI = RLUBW(I,R)
         ELSE
            RLUBI = -RGLBW(-I,R)
         END IF
         RETURN
      END
C
C-----------------------------------------------------------------------
C
C     Returns the number of digits in the base-10 representation of the
C     whole number W.
C
C-----------------------------------------------------------------------
C
      INTEGER FUNCTION NDREPW(W)
         INTEGER      HI, I, LO, W
         LO = 0
         HI = 10
         DO 10 I = 1 , 9
            IF( W .GE. LO  .AND.  W .LT. HI ) GO TO 20
            IF( I .LT. 9 ) THEN
               LO = HI
               HI = 10 * LO - 1
            END IF
 10      CONTINUE
         I = 10
 20      NDREPW = I
         RETURN
      END
C
C-----------------------------------------------------------------------
C
C     "Converts" FORTRAN Boolean to {0,1} Boolean.
C
C-----------------------------------------------------------------------
C
      INTEGER FUNCTION LOGIC(TVALUE)
         LOGICAL     TVALUE
         IF( TVALUE ) THEN
            LOGIC = 1
         ELSE
            LOGIC = 0
         END IF
         RETURN
      END

C-----------------------------------------------------------------------
C
C     "Converts" FORTRAN Boolean to {0,1} Boolean.
C
C-----------------------------------------------------------------------
C
      INTEGER FUNCTION MLOGIC(TVALUE,VALUET,VALUEF)
         LOGICAL     TVALUE
         INTEGER     VALUET, VALUEF
         IF( TVALUE ) THEN
            MLOGIC = VALUET
         ELSE
            MLOGIC = VALUEF
         END IF
         RETURN
      END
C
C-----------------------------------------------------------------------
C
C     Transfers N bits of FROM to TO. No error checking.
C
C-----------------------------------------------------------------------
C
      SUBROUTINE TRNBTS(TO,IT,FROM,IF,N)
         INTEGER     IBCLR, IBSET
         LOGICAL     BTEST
         INTEGER     FROM, I, IF, IT, N, T, TO
         DO 10 I = 0 , N
            IF( BTEST(FROM,IF+I) ) THEN
               TO = IBSET(TO,IT+I)
            ELSE
               TO = IBCLR(TO,IT+I)
            END IF
 10      CONTINUE
         RETURN
      END
C
C-----------------------------------------------------------------------
C
C     Sets I1 through I2th bits of SETEE to low order bits of SETTER.
C     No error checking.
C
C-----------------------------------------------------------------------
C
      SUBROUTINE SETBTS(SETEE,SETTER,I1,I2)
         INTEGER      I1, I2, SETEE, SETTER
         CALL TRNBTS(SETEE,I1,SETTER,0,I2-I1)
         RETURN
      END
C
C-----------------------------------------------------------------------
C
C     Returns I1 through I2th bits of RETEE in low order bits of RETBTS.
C
C-----------------------------------------------------------------------
C
      INTEGER FUNCTION RETBTS(RETEE,I1,I2)
         INTEGER      I1, I2, RETEE
         RETBTS = 0
         CALL TRNBTS(RETBTS,0,RETEE,I1,I2-I1)
         RETURN
      END
C
C-------------------------------------------------------------------------------
C
C     Solves tridiagonal system
C
C     L(J) * X(J-1) + M(J) * X(J) + U(J) * X(J+1) = RHS(J)
C
C     of order NEQ using standard algorithm. (see for example Richtmeyer and
C     Morton.) Arrays L and U are overwritten as is RHS which contains the
C     solution (X) upon exit. No error checking.
C
C-------------------------------------------------------------------------------
C
      SUBROUTINE TRISLV(NEQ,L,M,U,RHS)
C
         REAL*8         L(1), M(1), RHS(1), U(1)
         INTEGER        NEQ
C
         REAL*8         CJ
         INTEGER        J, NEQM1
C
         NEQM1 = NEQ - 1
         CJ = 1.0D0 / M(1)
         U(1) = -U(1) * CJ
         L(1) = RHS(1) * CJ
         DO 100 J = 2 , NEQM1
            CJ = 1.0D0 / (L(J) * U(J-1) + M(J))
            U(J) = -U(J) * CJ
            L(J) = (RHS(J) - L(J) * L(J-1)) * CJ
 100     CONTINUE
         RHS(NEQ) = (RHS(NEQ) - L(NEQ) * L(NEQM1)) /
     *              (L(NEQ) * U(NEQM1) + M(NEQ))
         DO 200 J = NEQM1 , 1 , -1
            RHS(J) = U(J) * RHS(J+1) + L(J)
 200     CONTINUE
         RETURN
C
      END
C
C-----------------------------------------------------------------------
C
C     For use in conjunction with TRISLV in setting up tridiagonal
C     systems.
C
C-----------------------------------------------------------------------
C
      SUBROUTINE TRLOAD(A,N,J,LO,ALOJ,M,AMJ,UP,AUPJ,RHS,ARHSJ)
C
         INTEGER      N
         REAL*8       A(N,4)
C
         REAL*8       ALOJ, AMJ, ARHSJ, AUPJ
         INTEGER      J, LO, M, RHS, UP
C
         A(J,LO)  = ALOJ
         A(J,M)   = AMJ
         A(J,UP)  = AUPJ
         A(J,RHS) = ARHSJ
C
         RETURN
C
      END
C
C-------------------------------------------------------------------------------
C
C     Version of intrinsic function MOD which prevents division by 0 and
C     is useful for tracing operations etc.
C
C-------------------------------------------------------------------------------
C
      INTEGER FUNCTION MYMOD(I1,I2)
C
         INTEGER       MOD
         INTEGER       I1, I2
C
         IF( I2 .EQ. 0 ) THEN
            MYMOD = 1
         ELSE
            MYMOD = MOD(I1,I2)
         END IF
C
         RETURN
C
      END
C
C-------------------------------------------------------------------------------
C
C     Obvious predicates.
C
C-------------------------------------------------------------------------------
C
      LOGICAL FUNCTION ODD(N)
C
         INTEGER      N
C
         ODD = MOD(N,2) .NE. 0
C
         RETURN
C
      END
C
      LOGICAL FUNCTION EVEN(N)
C
         INTEGER      N
C
         EVEN = MOD(N,2) .EQ. 0
C
         RETURN
C
      END
C
C-----------------------------------------------------------------------
C
C     Disables printing of
C                          underflow
C                                         messages.
C     Causes abort for single
C                          exponent error
C                          sqrt of negative #
C                          exp of negative #
C
C-----------------------------------------------------------------------
C
      SUBROUTINE NOUNFL()
C
         RETURN
C
      END
C
C-----------------------------------------------------------------------
C
C     Timer routine. Maintains array of timers.
C     OPCODE: (0,1,2,3) ---> (stop,start,query,set to SVALUE ms)
C
C-----------------------------------------------------------------------
C
      DOUBLE PRECISION FUNCTION TIMER(I,OPCODE,SVALUE)
C
         INTEGER       I, OPCODE 
         REAL*8        SVALUE
C
         TIMER = 0.0d0
C
         RETURN
C
      END
C
C-----------------------------------------------------------------------
C
C     Timer routine. Maintains array of timers.
C     OPCODE: (0,1,2,3) ---> (stop,start,query,set to SVALUE ms)
C
C-----------------------------------------------------------------------
C
      DOUBLE PRECISION FUNCTION TIMEX(I,OPCODE,SVALUE)
C
         INTEGER       I, OPCODE 
         REAL*8        SVALUE

         TIMEX = 0.0D0

         RETURN
C
      END
C
C-----------------------------------------------------------------------
C
C     Another obvious predicate.
C
C-----------------------------------------------------------------------
C
      LOGICAL FUNCTION PRANGE(LO,X,HI)
C
         REAL*8      LO, HI, X
C
         PRANGE = X .GE. LO  .AND.  X .LE. HI
C
         RETURN
C
      END
C
C-----------------------------------------------------------------------
C
C     Another obvious predicate.
C
C-----------------------------------------------------------------------
C
      LOGICAL FUNCTION PFUZIN  (LO,X,HI)
C
         REAL*8      LO, HI, X
         REAL*8      FUZZ 
         PARAMETER ( FUZZ = 1.0D-12 )
C
         PFUZIN = X .GE. LO  .AND.  X .LE. HI
         PFUZIN = PFUZIN   .OR.
     *            ABS(X - LO) .LE. FUZZ   .OR.
     *            ABS(X - HI) .LE. FUZZ
C
         RETURN
C
      END
C
C-----------------------------------------------------------------------
C
C     Nearest integer (positive, rounds up).
C
C-----------------------------------------------------------------------
C
      INTEGER FUNCTION NINTD(D)
C
         REAL*8     D
C
         NINTD = D
         IF( (D - NINTD) .GE. 0.5D0 ) THEN
            NINTD = NINTD + 1
         END IF
C
         RETURN
C
      END

C-----------------------------------------------------------------------
C
C     Nearest integer (positive, rounds up).
C
C-----------------------------------------------------------------------
C
      INTEGER FUNCTION NINTS(S)
C
         REAL       S
C
         NINTS = S
         IF( (S - NINTS) .GE. 0.5E0 ) THEN
            NINTS = NINTS + 1
         END IF
C
         RETURN
C
      END
C
C-----------------------------------------------------------------------
C
C     Returns ARG unless ARG is not in inclusive range (LO,UP), in
C     which case DFAULT is returned.
C
C-----------------------------------------------------------------------
C
      INTEGER FUNCTION DEFRNG(ARG,DFAULT,LO,UP)
C
         IMPLICIT       NONE
C
         INTEGER        ARG,      DFAULT,   LO,       UP
C
         IF( LO .GT. ARG  .OR.  ARG .GT. UP ) THEN
            DEFRNG = DFAULT
         ELSE
            DEFRNG = ARG
         END IF
C
         RETURN
C
      END
C
C-----------------------------------------------------------------------
C
C     Base--10 logarithm of X with FLOOR (for graphics purposes).
C
C-----------------------------------------------------------------------

      DOUBLE PRECISION FUNCTION LOG10F(X,FLOOR)

         IMPLICIT       NONE

         REAL*8         X,   FLOOR

         IF( X .GT. 0.0D0 ) THEN
            LOG10F = MAX(LOG10(X),FLOOR)
         ELSE
            LOG10F = FLOOR
         END IF

         RETURN

      END
C
C-----------------------------------------------------------------------
C
C     Base10 logarithm of abs(X) with check for zero.
C
C-----------------------------------------------------------------------

      DOUBLE PRECISION FUNCTION ABSLOG(X)

         IMPLICIT       NONE

         REAL*8         X,   FLOOR
         DATA           FLOOR / -99.9D0 /

         IF( X .NE. 0.0D0 ) THEN
            ABSLOG = LOG10(ABS(X))
         ELSE
            ABSLOG = FLOOR
         END IF

         RETURN

      END

c
c     Arc--hyperbolic functions.
c
      double precision function dacosh(x)

         implicit      logical (a-z)

         real*8        x

         if( x .ge. 1.0d0 ) then
            dacosh = log(x + sqrt(x**2 - 1.0d0))
         else
            write(*,*) '<<< dacosh:: Domain error: ',sngl(x),
     *                 '. >>>'
            dacosh = 0.0d0
         end if

         return

      end

      double precision function dasinh(x)

         implicit      logical (a-z)

         real*8        x

         dasinh = log(x + sqrt(x**2 + 1.0d0))

         return

      end

      double precision function datanh(x)

         implicit      logical (a-z)

         real*8        x

         if( abs(x) .lt. 1.0d0 ) then
            datanh = 0.5d0 * log((1.0d0 + x) / (1.0d0 - x))
         else
            write(*,*) '<<< datanh:: Domain error: ',sngl(x),
     *                 '. >>>'
            datanh = 0.0d0
         end if

         return

      end

C-----------------------------------------------------------------------
C
C     Base10 logarithm of abs(X) with check for zero.
C     Calling routine must define initialize COMMON vbl FLOOR.
C
C-----------------------------------------------------------------------

      DOUBLE PRECISION FUNCTION DLOG0(X)

         IMPLICIT       NONE

         REAL*8         X

         REAL*8                     FLOOR 
         COMMON       / COM_DLOG0 / FLOOR

         IF( X .NE. 0.0D0 ) THEN
            DLOG0 = LOG10(ABS(X))
         ELSE
            DLOG0 = FLOOR
         END IF

         RETURN

      END

      double precision function dlog2(x)

         implicit        none

         real*8          x

         real*8          ln2m1
         parameter     ( ln2m1 = 1.442695040888963d0 )

         dlog2 = ln2m1 * log(x)

         return

      end

      integer function ilog2(x)

         implicit        none

         real*8          dlog2
         integer         nintd

         integer         x

         ilog2 = nintd(dlog2(1.0d0 * x))

         return

      end
c
c     Predicate: n = 2^p + 1
c
      logical function p2np1(n)

         implicit        none

         real*8          dlog2
         integer         nintd

         integer         n

         p2np1 = n .eq. (2 ** nintd(dlog2(1.0d0 * (n - 1))) + 1)

         return

      end
c
c     Ring increment/decrement ...
c
      integer function rnginc(i,imin,imax) 
         
         implicit      none 

         integer       i,     imin,     imax

         rnginc = i + 1 
         if( rnginc .gt. imax ) then
            rnginc = imin
         end if

         return

      end
 
      integer function rngdec(i,imin,imax) 
         
         implicit      none 

         integer       i,     imin,     imax

         rngdec = i - 1 
         if( rngdec .lt. imin ) then
            rngdec = imax
         end if

         return

      end

      real function mysl(x)

         real       x

         mysl = x

         return

      end


      logical function deqfuzz(a,b,fuzz)

         implicit       none

         real*8         a,     b,     fuzz

         if( a .eq. 0.0d0 ) then
            deqfuzz = abs(b) .le. fuzz
         else 
            deqfuzz = abs(a - b) / abs(a) .le. fuzz
         end if

         return

      end
C
C-----------------------------------------------------------------------
C
C     Returns estimate of machine epsilon ...
C
C-----------------------------------------------------------------------

      double precision function dmeps()

         implicit               none

         double precision       x,    fac
         parameter            (       fac = 0.5d0 )

         dmeps = 1.0d0
         x = 1.0d0
 100     continue
            if( x .eq. x * (1.0d0 + dmeps) ) go to 200
            dmeps = dmeps * fac
         go to 100

 200     return

      end

C-----------------------------------------------------------------------
C
C     Predicate for integer valued reals ...
C
C-----------------------------------------------------------------------

      logical function paint(x)

         real          x

         paint = aint(x) .eq. x 

         return
      
      end
C-----------------------------------------------------------------------
C
C     Returns maximum permissible argument for exp() ...
C
C-----------------------------------------------------------------------

      double precision function max_exp_arg()

         implicit      none

c
c        From /usr/include/float.h ...
c
         real*8        max_double
         parameter   ( max_double  = 1.797693134862314d+308 )

         max_exp_arg = log(max_double)

         return

      end

c-----------------------------------------------------------------------
c
c     Increments element of LINPACK style sparse array ...
c
c-----------------------------------------------------------------------

      subroutine addabd(abd,labd,n,ml,mu,i,j,val)

         implicit       none

         integer        labd,     n
         real*8         abd(labd,n)
         integer        ml,       mu,      i,      j
         real*8         val

         integer        k

         if( i .ge. max(1,j-mu)  .and.  i .le. min(n,j+ml) ) then
            k = i - j + ml + mu + 1
            abd(k,j) = abd(k,j) + val
         end if
   
         return

      end

c-----------------------------------------------------------------------
c
c      Given two lines defined by point-pairs 
c
c      [ (x0,y0) , (x1,y1) ]   and [ (x2,y2) , (x3,y3) ] 
c
c      returns intersection point (xstar,ystar), if such a (unique) 
c      point exists (rc = 0), else (rc = -1).
c
c-----------------------------------------------------------------------

      subroutine intersect(x0,y0,x1,y1,x2,y2,x3,y3,xstar,ystar,rc)

         implicit          none

         real*8            x0,    y0,    x1,    y1, 
     *                     x2,    y2,    x3,    y3,
     *                     xstar, ystar

         real*8            ala,   bea,   gaa, 
     *                     alb,   beb,   gab, 
     *                     disc,  discm1
         
         integer           rc

         ala = y1 - y0
         bea = x0 - x1
         gaa = x0 * y1 - x1 * y0

         alb = y3 - y2
         beb = x2 - x3
         gab = x2 * y3 - x3 * y2

         disc = ala * beb - alb * bea
         if( disc .ne. 0.0d0 ) then
            discm1 = 1.0d0 / disc
            xstar = discm1 * (beb * gaa - bea * gab)
            ystar = discm1 * (ala * gab - alb * gaa)
            rc = 0
         else
            rc = -1
         end if

         return

      end

c-----------------------------------------------------------------------
c
c     Various routines for computing first and second derivative 
c     approximations using finite-difference formulae ...
c
c-----------------------------------------------------------------------

      double precision function fd1h2(f,x,h)

         implicit      none

         external      f
         real*8        f,       x,       h

         integer       nc
         parameter   ( nc = 2 )
         real*8        offset(nc)
         real*8        c(nc)

         integer       i

         data          offset /
     *                 -1.0d0,
     *                  1.0d0
     *                        /
         data          c      /
     *                 -0.5d0,
     *                  0.5d0
     *                        /

         fd1h2 = 0.0d0
         if( h .ne. 0.0d0 ) then
            do i = 1 , nc
               fd1h2 = fd1h2 + c(i) * f(x + offset(i) * h)
            end do
            fd1h2 = fd1h2 / h
         end if

         return

      end 

c-----------------------------------------------------------------------

      double precision function fd1h4(f,x,h)

         implicit      none

         external      f
         real*8        f,       x,       h

         integer       nc
         parameter   ( nc = 4 )
         real*8        offset(nc)
         real*8        c(nc)

         integer       i

         data          offset /
     *                 -2.0d0,
     *                 -1.0d0,
     *                  1.0d0,
     *                  2.0d0
     *                        /
         data          c      /
     *                  0.08333333333333333d0,
     *                 -0.6666666666666667d0,
     *                  0.6666666666666667d0,
     *                 -0.08333333333333333d0
     *                        /

         fd1h4 = 0.0d0
         if( h .ne. 0.0d0 ) then
            do i = 1 , nc
               fd1h4 = fd1h4 + c(i) * f(x + offset(i) * h)
            end do
            fd1h4 = fd1h4 / h
         end if

         return

      end 

c-----------------------------------------------------------------------

      double precision function fd1h6(f,x,h)

         implicit      none

         external      f
         real*8        f,       x,       h

         integer       nc
         parameter   ( nc = 6 )
         real*8        offset(nc)
         real*8        c(nc)

         integer       i

         data          offset /
     *                 -3.0d0,
     *                 -2.0d0,
     *                 -1.0d0,
     *                  1.0d0,
     *                  2.0d0,
     *                  3.0d0
     *                        /
         data          c      /
     *                 -0.01666666666666667d0,
     *                  0.1500000000000000d0,
     *                 -0.7500000000000000d0,
     *                  0.7500000000000000d0,
     *                 -0.1500000000000000d0,
     *                  0.01666666666666667d0
     *                        /

         fd1h6 = 0.0d0
         if( h .ne. 0.0d0 ) then
            do i = 1 , nc
               fd1h6 = fd1h6 + c(i) * f(x + offset(i) * h)
            end do
            fd1h6 = fd1h6 / h
         end if

         return

      end 

c-----------------------------------------------------------------------
c
c     Various routines for computing first and second derivative 
c     approximations using finite-difference formulae ...
c
c-----------------------------------------------------------------------

      double precision function fd2h2(f,x,h)

         implicit      none

         external      f
         real*8        f,       x,       h

         integer       nc
         parameter   ( nc = 3 )
         real*8        offset(nc)
         real*8        c(nc)

         integer       i

         data          offset /
     *                 -1.0d0,
     *                  0.0d0,
     *                  1.0d0
     *                        /
         data          c      /
     *                  1.0d0,
     *                 -2.0d0,
     *                  1.0d0
     *                        /

         fd2h2 = 0.0d0
         if( h .ne. 0.0d0 ) then
            do i = 1 , nc
               fd2h2 = fd2h2 + c(i) * f(x + offset(i) * h)
            end do
            fd2h2 = fd2h2 / (h * h)
         end if

         return

      end 

c-----------------------------------------------------------------------

      double precision function fd2h4(f,x,h)

         implicit      none

         external      f
         real*8        f,       x,       h

         integer       nc
         parameter   ( nc = 5 )
         real*8        offset(nc)
         real*8        c(nc)

         integer       i

         data          offset /
     *                 -2.0d0,
     *                 -1.0d0,
     *                  0.0d0,
     *                  1.0d0,
     *                  2.0d0
     *                        /
         data          c      /
     *                -0.08333333333333333d0,
     *                 1.333333333333333d0,  
     *                -2.500000000000000d0,
     *                 1.333333333333333d0,  
     *                -0.08333333333333333d0
     *                        /

         fd2h4 = 0.0d0
         if( h .ne. 0.0d0 ) then
            do i = 1 , nc
               fd2h4 = fd2h4 + c(i) * f(x + offset(i) * h)
            end do
            fd2h4 = fd2h4 / (h * h)
         end if

         return

      end 

c-----------------------------------------------------------------------

      double precision function fd2h6(f,x,h)

         implicit      none

         external      f
         real*8        f,       x,       h

         integer       nc
         parameter   ( nc = 7 )
         real*8        offset(nc)
         real*8        c(nc)

         integer       i

         data          offset /
     *                 -3.0d0,
     *                 -2.0d0,
     *                 -1.0d0,
     *                  0.0d0,
     *                  1.0d0,
     *                  2.0d0,
     *                  3.0d0
     *                        /
         data          c      /
     *                  0.01111111111111111d0,
     *                 -0.1500000000000000d0,
     *                  1.5000000000000000d0,
     *                 -2.722222222222222d0,
     *                  1.5000000000000000d0,
     *                 -0.1500000000000000d0,
     *                  0.01111111111111111d0
     *                        /

         fd2h6 = 0.0d0
         if( h .ne. 0.0d0 ) then
            do i = 1 , nc
               fd2h6 = fd2h6 + c(i) * f(x + offset(i) * h)
            end do
            fd2h6 = fd2h6 / (h * h)
         end if

         return

      end 

c-----------------------------------------------------------------------
c     Park and Miller's "Minimal Standard" random number generator ...
c
c     Modified version of "Numerical Recipes" (Press et al) 'ran0()'
c     routine.
c
c     This version always starts with same seed.
c
c     Not completed ...
c-----------------------------------------------------------------------

      double precision function rand01()
         integer      a,     m,      q,       r
         parameter (  a = 165807,
     *                m = 2147483647,
     *                q = 127773 )

         rand01 = 0.0d0
         return
      end 

c-----------------------------------------------------------------------
c      Classic fourth order Runge-Kutta integrator.
c-----------------------------------------------------------------------
      subroutine drk4(fcn,neq,y,x,h,istate)
         implicit    none

         external    fcn

			integer     maxneq
         parameter ( maxneq = 1 000 000 )

         real*8      k(maxneq, 5)

         integer     neq, istate
         real*8      y(neq), x, h

         real*8      hby6
         integer     i

         if( neq .gt. maxneq ) then
            istate = 1
            write(0,*) 'dkr4: Error ... Insufficient internal storage'
            write(0,*) 'dkr4: neq =', neq, ' > maxneq =', maxneq
            return
         end if

         istate = 0
         hby6 = h / 6.0d0

         call fcn(neq, x, y, k(1,1))

         do i = 1 , neq
            k(i,5) = y(i) + 0.5d0 * h * k(i,1)
         end do
         call fcn(neq, x + 0.5d0 * h, k(1,5), k(1,2))

         do i = 1 , neq
            k(i,5) = y(i) + 0.5d0 * h * k(i,2)
         end do
         call fcn(neq, x + 0.5d0 * h, k(1,5), k(1,3))

         do i = 1 , neq
            k(i,5) = y(i) + h * k(i,3)
         end do
         call fcn(neq, x + h, k(1,5), k(1,4))

         do i = 1 , neq
            y(i) = y(i) + hby6 * (k(i,1) + 
     &             2.0d0 * (k(i,2) + k(i,3)) + k(i,4))
         end do

         return
      end

c-----------------------------------------------------------------------
c      Classic second order Runge-Kutta integrator.
c-----------------------------------------------------------------------
      subroutine drk2(fcn,neq,y,x,h,istate)
         implicit    none

         external    fcn

			integer     maxneq
         parameter ( maxneq = 1 000 000 )

         real*8      k(maxneq, 3)

         integer     neq, istate
         real*8      y(neq), x, h

         integer     i

         if( neq .gt. maxneq ) then
            istate = 1
            write(0,*) 'dkr2: Error ... Insufficient internal storage'
            write(0,*) 'dkr2: neq =', neq, ' > maxneq =', maxneq
            return
         end if
         istate = 0 

         call fcn(neq, x, y, k(1,1))

         do i = 1 , neq
            k(i,3) = y(i) + 0.5d0 * h * k(i,1)
         end do
         call fcn(neq, x + 0.5d0 * h, k(1,3), k(1,2))

         do i = 1 , neq
            y(i) = y(i) + h * k(i,2)
         end do

         return
      end
