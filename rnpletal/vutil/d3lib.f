C-----------------------------------------------------------------------
C
C     D O U B L E     P R E C I S I O N     3 -- A R R A Y
C
C                    O P E R A T I O N S
C
C-----------------------------------------------------------------------

C-----------------------------------------------------------------------
C
C     3--array-3--array add.
C
C-----------------------------------------------------------------------
C
      SUBROUTINE D33A(A1,A2,A3,D1,D2,D3)
C
         IMPLICIT       NONE
C
         INTEGER        D1,           D2,           D3
         REAL*8         A1(D1,D2,D3), A2(D1,D2,D3), A3(D1,D2,D3)
C
         INTEGER        I,            J,            K
C
         DO 30 K = 1 , D3
            DO 20 J = 1 , D2
               DO 10 I = 1 , D1
                  A3(I,J,K) = A1(I,J,K) + A2(I,J,K)
 10            CONTINUE
 20         CONTINUE
 30      CONTINUE
C
         RETURN
C
      END
C
C-----------------------------------------------------------------------
C
C     3--array-3--array subtract.
C
C-----------------------------------------------------------------------
C
      SUBROUTINE D33S(A1,A2,A3,D1,D2,D3)
C
         IMPLICIT       NONE
C
         INTEGER        D1,           D2,           D3
         REAL*8         A1(D1,D2,D3), A2(D1,D2,D3), A3(D1,D2,D3)
C
         INTEGER        I,            J,            K
C
         DO 30 K = 1 , D3
            DO 20 J = 1 , D2
               DO 10 I = 1 , D1
                  A3(I,J,K) = A1(I,J,K) - A2(I,J,K)
 10            CONTINUE
 20         CONTINUE
 30      CONTINUE
C
         RETURN
C
      END
C
C-----------------------------------------------------------------------
C
C     3--array-3--array multiply.
C
C-----------------------------------------------------------------------
C
      SUBROUTINE D33M(A1,A2,A3,D1,D2,D3)
C
         IMPLICIT       NONE
C
         INTEGER        D1,           D2,           D3
         REAL*8         A1(D1,D2,D3), A2(D1,D2,D3), A3(D1,D2,D3)
C
         INTEGER        I,            J,            K
C
         DO 30 K = 1 , D3
            DO 20 J = 1 , D2
               DO 10 I = 1 , D1
                  A3(I,J,K) = A1(I,J,K) * A2(I,J,K)
 10            CONTINUE
 20         CONTINUE
 30      CONTINUE
C
         RETURN
C
      END
C
C-----------------------------------------------------------------------
C
C     3--array-3--array divide.
C
C-----------------------------------------------------------------------
C
      SUBROUTINE D33D(A1,A2,A3,D1,D2,D3)
C
         IMPLICIT       NONE
C
         INTEGER        D1,           D2,           D3
         REAL*8         A1(D1,D2,D3), A2(D1,D2,D3), A3(D1,D2,D3)
C
         INTEGER        I,            J,            K
C
         DO 30 K = 1 , D3
            DO 20 J = 1 , D2
               DO 10 I = 1 , D1
                  A3(I,J,K) = A1(I,J,K) / A2(I,J,K)
 10            CONTINUE
 20         CONTINUE
 30      CONTINUE
C
         RETURN
C
      END
C
C-----------------------------------------------------------------------
C
C     3--array l2 norm.
C
C-----------------------------------------------------------------------
C
      DOUBLE PRECISION FUNCTION D3NRM2(A,D1,D2,D3)
C
         IMPLICIT       NONE
C
         INTEGER        D1,       D2,      D3
         REAL*8         A(D1,D2,D3)
C
         INTEGER        I,        J,       K
C
         D3NRM2 = 0.0D0
         DO 30 K = 1 , D3
            DO 20 J = 1 , D2
               DO 10 I = 1 , D1
                  D3NRM2 = D3NRM2 + A(I,J,K) * A(I,J,K)
 10            CONTINUE
 20         CONTINUE
 30      CONTINUE
         D3NRM2 = SQRT(D3NRM2 / (D1 * D2 * D3))
C
         RETURN
C
      END
C
C-----------------------------------------------------------------------
C
C     Load 3--array with scalar.
C
C-----------------------------------------------------------------------
C
      SUBROUTINE D3LS(A,S,D1,D2,D3)
C
         IMPLICIT       NONE
C
         INTEGER        D1,       D2,       D3
         REAL*8         A(D1,D2,D3)
         REAL*8         S
C
         INTEGER        I,        J,        K
C
         DO 30 K = 1 , D3
            DO 20 J = 1 , D2
               DO 10 I = 1 , D1
                  A(I,J,K) = S
 10            CONTINUE
 20         CONTINUE
 30      CONTINUE
C
         RETURN
C
      END
C
C-----------------------------------------------------------------------
C
C     Dumps 3--array labelled with LABEL on UNIT.
C
C-----------------------------------------------------------------------
C
      SUBROUTINE D3DUMP(A,D1,D2,D3,LABEL,UNIT)
C
         IMPLICIT       NONE
C
         INTEGER        D1,       D2,       D3,
     *                  UNIT
         REAL*8         A(D1,D2,D3)
         CHARACTER*(*)  LABEL
         INTEGER        I,        J,        K,
     *                  ST
C
         IF( D1 .GT. 0  .AND.  D2 .GT. 0  .AND.  D3 .GT. 0 ) THEN
            WRITE(UNIT,100) LABEL
 100        FORMAT(/' <<< ',A,' >>>')
            DO 300 K = 1 , D3
               WRITE(UNIT,105) K
 105           FORMAT(' <<< Plane: ',I4,'. >>>')
               DO 200 J = 1 , D2
                  ST = 1
 110              CONTINUE
                     WRITE(UNIT,120) ( A(I,J,K) , I = ST , MIN(ST+3,D1))
 120                 FORMAT(' ',4(1PE19.10))
                  ST = ST + 4
                  IF( ST .LE. D1 ) GO TO 110
                  WRITE(UNIT,140)
 140              FORMAT(' ')
 200           CONTINUE
 300        CONTINUE
         END IF
C
         RETURN
C
      END
C
C-----------------------------------------------------------------------
C
C     (3--array scalar multiply) add (3--array scalar multiply).
C
C-----------------------------------------------------------------------
C
      SUBROUTINE D3S3SA(A1,A2,A3,S1,S2,D1,D2,D3)
C
         IMPLICIT       NONE
C
         INTEGER        D1,             D2,             D3
         REAL*8         A1(D1,D2,D3),   A2(D1,D2,D3),   A3(D1,D2,D3)
         REAL*8         S1,             S2
C
         INTEGER        I,              J,              K
C
         DO 30 K = 1 , D3
            DO 20 J = 1 , D2
               DO 10 I = 1 , D1
                  A3(I,J,K) = S1 * A1(I,J,K) + S2 * A2(I,J,K)
 10            CONTINUE
 20         CONTINUE
 30      CONTINUE
C
         RETURN
C
      END
C
C-----------------------------------------------------------------------
C
C     3--array --  3--array copy
C
C-----------------------------------------------------------------------
C
      SUBROUTINE D3COPY(A1,A2,D1,D2,D3)
C
         IMPLICIT       NONE
C
         INTEGER        D1,            D2,           D3
         REAL*8         A1(D1,D2,D3),  A2(D1,D2,D3)
C
         INTEGER        I,             J,            K
C
         DO 30 K = 1 , D3
            DO 20 J = 1 , D2
               DO 10 I = 1 , D1
                  A2(I,J,K) = A1(I,J,K)
 10            CONTINUE
 20         CONTINUE
 30      CONTINUE
C
         RETURN
C
      END

C-----------------------------------------------------------------------
C
C     3--array --  3--array border copy
C
C-----------------------------------------------------------------------
C
      SUBROUTINE D3BCOPY(A1,A2,D1,D2,D3)
C
         IMPLICIT       NONE
C
         INTEGER        D1,            D2,           D3
         REAL*8         A1(D1,D2,D3),  A2(D1,D2,D3)
C
         INTEGER        I,             J,            K
C
         DO 130 K = 1 , D3
            DO 120 J = 1 , D2
               A2(1,J,K) = A1(1,J,K)
               A2(D1,J,K) = A1(D1,J,K)
120         CONTINUE
130      CONTINUE
         DO 230 K = 1 , D3
            DO 210 I = 1 , D1
               A2(I,1,K) = A1(I,1,K)
               A2(I,D2,K) = A1(I,D2,K)
210         CONTINUE
230      CONTINUE
         DO 320 J = 1 , D2
            DO 310 I = 1 , D1
               A2(I,J,1) = A1(I,J,1)
               A2(I,J,D3) = A1(I,J,D3)
310         CONTINUE
320      CONTINUE
C
         RETURN
C
      END
C
C--------------------------------------------------------------------------
C
C     Computes linear function CX * X + CY * Y + CZ * Z + C0 and
C     stores in A.
C
C--------------------------------------------------------------------------

      SUBROUTINE D3FLIN(A,X,Y,Z,NX,NY,NZ,CX,CY,CZ,C0)

         IMPLICIT       NONE

         INTEGER        NX,         NY,         NZ
         REAL*8         A(NX,NY,NZ),
     *                  X(NX),      Y(NY),      Z(NZ)
         REAL*8         CX,         CY,         CZ,
     *                  C0

         INTEGER        IX,         IY,         IZ

         DO 30 IZ = 1 , NZ
            DO 20 IY = 1 , NY
               DO 10 IX = 1 , NX
                  A(IX,IY,IZ) = CX * X(IX) + CY * Y(IY) +
     *                          CZ * Z(IZ) + C0
 10            CONTINUE
 20         CONTINUE
 30      CONTINUE

         RETURN

      END
C
C---------------------------------------------------------------------------
C
C     2 : 1 linear interpolation of coarse 3--function UC to fine
C     function UF.
C
C---------------------------------------------------------------------------

      SUBROUTINE D3LINT(UC,UF,NXC,NYC,NZC)

         IMPLICIT     NONE

         INTEGER      NXC,      NYC,      NZC
         REAL*8       UC(NXC,NYC,NZC),
     *                UF(2*NXC-1,2*NYC-1,2*NZC-1)

         INTEGER      IY,       IZ,       J,         K,
     *                NXF,      NYF

         NXF = 2*NXC - 1
         NYF = 2*NYC - 1
         DO 50 IZ = 1 , NZC
            K = 2 * IZ - 1
            DO 10 IY = 1 , NYC
               J = 2 * IY - 1
               CALL DVPRLN(UC(1,IY,IZ),UF(1,J,K),2,NXC)
               CALL DVAV2( UF(1,J,K),NXF)
               IF( IY .GT. 1 ) THEN
                  CALL DVSVSA(UF(1,J-2,K),
     *                        UF(1,J,  K),
     *                        UF(1,J-1,K),0.5D0,0.5D0,NXF)
               END IF
 10         CONTINUE
            IF( IZ .GT. 1 ) THEN
               DO 20 IY = 1 , NYF
                  CALL DVSVSA(UF(1,IY,K-2),UF(1,IY,K),
     *                        UF(1,IY,K-1),0.5D0,0.5D0,NXF)
 20            CONTINUE
            END IF
 50       CONTINUE

          RETURN

      END
C
C-----------------------------------------------------------------------
C
C     3--array prolongation.
C
C-----------------------------------------------------------------------

      SUBROUTINE D3PRLN(A1,A2,D1INC,D2INC,D3INC,D1,D2,D3)

         IMPLICIT       NONE

         INTEGER        D1,        D2,       D3,
     *                  D1INC,     D2INC,    D3INC
         REAL*8         A1(D1,D2,D3),
     *                  A2(D1INC * (D1 - 1) + 1,
     *                     D2INC * (D2 - 1) + 1,
     *                     D3INC * (D3 - 1) + 1)

         INTEGER        J,         K

         DO 20 K = 0 , D3 - 1
            DO 10 J = 0 , D2 - 1
                CALL DVPRLN(A1(1,J+1,K+1),
     *                      A2(1,D2INC*J+1,D3INC*K+1),D1INC,D1)
 10         CONTINUE
 20      CONTINUE

         RETURN

      END
C
C-----------------------------------------------------------------------
C
C     3--array injection.
C
C-----------------------------------------------------------------------

      SUBROUTINE D3INJ(A1,A2,D1INC,D2INC,D3INC,D1,D2,D3)

         IMPLICIT      NONE

         INTEGER       D1,        D2,        D3,
     *                 D1INC,     D2INC,     D3INC
         REAL*8        A1(D1INC * (D1 - 1) + 1,
     *                    D2INC * (D2 - 1) + 1,
     *                    D3INC * (D3 - 1) + 1),
     *                 A2(D1,D2,D3)

         INTEGER       J,         K,         D1A2

         D1A2 = D1INC * (D1 - 1) + 1
         DO 20 K = 0 , D3 - 1
            DO 10 J = 0 , D2 - 1
               CALL DVINJ(A1(1,D2INC*J+1,D3INC*K+1),
     *                    A2(1,J+1,K+1),D1INC,D1A2)
 10         CONTINUE
 20      CONTINUE

         RETURN

      END
C
C--------------------------------------------------------------------------
C
C     Loads border of 3--array with scalar.
C
C--------------------------------------------------------------------------

      SUBROUTINE D3LBS(A,S,D1,D2,D3)

         IMPLICIT      NONE

         INTEGER       D1,         D2,         D3
         REAL*8        A(D1,D2,D3)

         REAL*8        S

         INTEGER       I1,         I2,         I3

         DO 20 I3 = 1 , D3
            DO 10 I2 = 1 , D2
               A(1, I2,I3) = S
               A(D1,I2,I3) = S
 10         CONTINUE
 20      CONTINUE

         DO 40 I3 = 1 , D3
            DO 30 I1 = 1 , D1
               A(I1,1, I3) = S
               A(I1,D2,I3) = S
 30         CONTINUE
 40      CONTINUE

         DO 60 I2 = 1 , D2
            DO 50 I1 = 1 , D1
               A(I1,I2,1 ) = S
               A(I1,I2,D3) = S
 50         CONTINUE
 60      CONTINUE

         RETURN

      END
C
C---------------------------------------------------------------------------
C
C     3--array half-weighted 2:1 coarsening with border injection.
C
C---------------------------------------------------------------------------

      SUBROUTINE D3RSHW(UF,UC,D1,D2,D3)

         IMPLICIT       NONE

         INTEGER        D1,         D2,         D3
         REAL*8         UF(2 * D1 - 1, 2 * D2 - 1, 2 * D3 - 1),
     *                  UC(D1,D2,D3)

         REAL*8         HALF,       TWELTH
         PARAMETER    ( HALF = 0.5D0,
     *                  TWELTH = 8.333 3333 3333 3333D-02 )

         INTEGER        I1,         I2,         I3,
     *                  I,          J,          K

         DO 30 I3 = 2 , D3 - 1
            K = 2 * I3 - 1
            DO 20 I2 = 2 , D2 - 1
               J = 2 * I2 - 1
               DO 10 I1 = 2 , D1 - 1
                  I = 2 * I1 - 1
                  UC(I1,I2,I3) = HALF * UF(I,J,K) + TWELTH * (
     *                            UF(I+1,J,K) + UF(I-1,J,K) +
     *                            UF(I,J+1,K) + UF(I,J-1,K) +
     *                            UF(I,J,K+1) + UF(I,J,K-1)  )
 10            CONTINUE
 20         CONTINUE
 30      CONTINUE

         DO 150 I2 = 1 , D2
            J = 2 * I2 - 1
            DO 100 I1 = 1 , D1
               I = 2 * I1 - 1
               UC(I1,I2,1 ) = UF(I,J,1     )
               UC(I1,I2,D3) = UF(I,J,2*D3-1)
 100        CONTINUE
 150     CONTINUE

         DO 250 I3 = 1 , D3
            K = 2 * I3 - 1
            DO 200 I1 = 1 , D1
               I = 2 * I1 - 1
               UC(I1,1, I3) = UF(I,1,     K)
               UC(I1,D2,I3) = UF(I,2*D2-1,K)
 200        CONTINUE
 250     CONTINUE

         DO 350 I3 = 1 , D3
            K = 2 * I3 - 1
            DO 300 I2 = 1 , D2
               J = 2 * I2 - 1
               UC(1, I2,I3) = UF(1,     J,K)
               UC(D1,I2,I3) = UF(2*D1-1,J,K)
 300        CONTINUE
 350     CONTINUE

         RETURN

      END
C
C--------------------------------------------------------------------------
C
C     3--array 2:1 injection (front end to D3INJ).
C
C--------------------------------------------------------------------------

      SUBROUTINE D3INJ2(A1,A2,D1,D2,D3)

         IMPLICIT       NONE

         INTEGER        D1,         D2,         D3
         REAL*8         A1(2 * D1 - 1,2 * D2 - 1,2 * D3 - 1),
     *                  A2(D1,D2,D3)

         CALL D3INJ(A1,A2,2,2,2,D1,D2,D3)

         RETURN

      END
C
C-----------------------------------------------------------------------
C
C     Load 3--array with checkerboard scalar.
C     Red/black consistent (mod 2) with DMZLS.
C
C-----------------------------------------------------------------------
C
      SUBROUTINE D3ZLS(A,S1,S2,D1,D2,D3)
C
         IMPLICIT        NONE

         INTEGER         D1,        D2,       D3
         REAL*8          A(D1,D2,D3)
         REAL*8          S1,        S2

         REAL*8          LS(2)
         INTEGER         I,         ISW,
     *                   J,
     *                   K,         KSW,      PASS

         LS(1) = S1
         LS(2) = S2

         DO 40 PASS = 1 , 2
            KSW = 3 - PASS
            DO 30 K =  1 , D3
               ISW = KSW
               DO 20 J = 1 , D2
                  DO 10 I = ISW , D1 , 2
                     A(I,J,K) = LS(PASS)
 10               CONTINUE
                  ISW = 3 - ISW
 20            CONTINUE
               KSW = 3 - KSW
 30         CONTINUE
 40      CONTINUE

         RETURN

      END
C
C-----------------------------------------------------------------------
C
C     Computes global and red/black l2 norms. (normalized).
C
C-----------------------------------------------------------------------
C
      DOUBLE PRECISION FUNCTION D3ZNR2(RBNR2,A,D1,D2,D3)
C
         IMPLICIT        NONE

         INTEGER         D1,        D2,       D3
         REAL*8          A(D1,D2,D3)
         REAL*8          RBNR2(2)

         INTEGER         NRB(2)

         INTEGER         I,         ISW,
     *                   J,
     *                   K,         KSW,      PASS
         INTEGER         D1O,       D1E,
     *                   D2O,       D2E,
     *                   D3O,       D3E

         RBNR2(1) = 0.0D0
         RBNR2(2) = 0.0D0

         D1O = (D1 + 1) / 2
         D1E = D1 / 2
         D2O = (D2 + 1) / 2
         D2E = D2 / 2
         D3O = (D3 + 1) / 2
         D3E = D3 / 2

         NRB(1) = D1O * (D2O * D3E + D2E * D3O) +
     *            D1E * (D2O * D3O + D2E * D3E)
         NRB(2) = D1O * (D2O * D3O + D2E * D3E) +
     *            D1E * (D2O * D3E + D2E * D3O)

         DO 40 PASS = 1 , 2
            KSW = 3 - PASS
            DO 30 K =  1 , D3
               ISW = KSW
               DO 20 J = 1 , D2
                  DO 10 I = ISW , D1 , 2
                     RBNR2(PASS) = RBNR2(PASS) +
     *                             A(I,J,K) * A(I,J,K)
 10               CONTINUE
                  ISW = 3 - ISW
 20            CONTINUE
               KSW = 3 - KSW
 30         CONTINUE
 40      CONTINUE

         IF( D1 * D2 * D3 .GT. 0 ) THEN
            D3ZNR2 = SQRT((RBNR2(1) + RBNR2(2)) / (D1 * D2 * D3))
         END IF
         DO 50 PASS = 1 , 2
            IF( NRB(PASS) .GT. 0 ) THEN
               RBNR2(PASS) = SQRT(RBNR2(PASS) / NRB(PASS))
            ELSE
               RBNR2(PASS) = 0.0D0
            END IF
 50      CONTINUE

         RETURN

      END

C-----------------------------------------------------------------------
C
C     3--array "ramp" function, same functionality as D3FLINT.
C
C-----------------------------------------------------------------------

      SUBROUTINE D3RAMP(A,D1,D2,D3,X10,DX1,X20,DX2,X30,DX3)

         IMPLICIT       NONE

         INTEGER        D1,         D2,         D3
         REAL*8         A(D1,D2,D3)

         REAL*8         X10,        DX1,        X20,        DX2,
     *                  X30,        DX3

         REAL*8         BASE3,      BASE2,      BASE1
         INTEGER        I,          J,          K

         BASE3 = X10 + X20 + X30
         DO 30 K = 1 , D3
            BASE2 = BASE3
            DO 20 J = 1 , D2
               BASE1 = BASE2
               DO 10 I = 1 , D1
                  A(I,J,K) = BASE1
                  BASE1 = BASE1 + DX1
 10            CONTINUE
               BASE2 = BASE2 + DX2
 20         CONTINUE
            BASE3 = BASE3 + DX3
 30      CONTINUE

         RETURN

      END
C
C-----------------------------------------------------------------------
C
C     3--array sum.
C
C-----------------------------------------------------------------------

      DOUBLE PRECISION FUNCTION D3SUM(A,D1,D2,D3)

         IMPLICIT       NONE

         INTEGER        D1,          D2,         D3
         REAL*8         A(D1,D2,D3)

         INTEGER        I,           J,          K

         D3SUM = 0.0D0
         DO 30 K = 1 , D3
            DO 20 J = 1 , D2
               DO 10 I = 1 , D1
                  D3SUM = D3SUM + A(I,J,K)
 10            CONTINUE
 20         CONTINUE
 30      CONTINUE

         RETURN

      END

C-----------------------------------------------------------------------
C
C     3--array function application.
C
C-----------------------------------------------------------------------

      SUBROUTINE D3FAPL(A,FOFA,D1,D2,D3,F)

         IMPLICIT       NONE

         EXTERNAL       F
         REAL*8         F

         INTEGER        D1,          D2,         D3
         REAL*8         A(D1,D2,D3), FOFA(D1,D2,D3)

         INTEGER        I,           J,          K

         DO 30 K = 1 , D3
            DO 20 J = 1 , D2
               DO 10 I = 1 , D1
                  FOFA(I,J,K) = F(A(I,J,K))
 10            CONTINUE
 20         CONTINUE
 30      CONTINUE

         RETURN

      END

C-----------------------------------------------------------------------
C
C     3--array minimum/maximum and l--infinity norm.
C
C-----------------------------------------------------------------------

      DOUBLE PRECISION FUNCTION D3MIN(A,D1,D2,D3)

         IMPLICIT       NONE

         INTEGER        D1,          D2,         D3
         REAL*8         A(D1,D2,D3)

         INTEGER        I,           J,          K

         D3MIN = A(1,1,1)
         DO 30 K = 1 , D3
            DO 20 J = 1 , D2
               DO 10 I = 1 , D1
                  D3MIN = MIN(D3MIN,A(I,J,K))
 10            CONTINUE
 20         CONTINUE
 30      CONTINUE

         RETURN

      END

      DOUBLE PRECISION FUNCTION D3MAX(A,D1,D2,D3)

         IMPLICIT       NONE

         INTEGER        D1,          D2,         D3
         REAL*8         A(D1,D2,D3)

         INTEGER        I,           J,          K

         D3MAX = A(1,1,1)
         DO 30 K = 1 , D3
            DO 20 J = 1 , D2
               DO 10 I = 1 , D1
                  D3MAX = MAX(D3MAX,A(I,J,K))
 10            CONTINUE
 20         CONTINUE
 30      CONTINUE

         RETURN

      END

      DOUBLE PRECISION FUNCTION D3LINF(A,D1,D2,D3)

         IMPLICIT       NONE

         INTEGER        D1,          D2,         D3
         REAL*8         A(D1,D2,D3)

         INTEGER        I,           J,          K

         D3LINF = ABS(A(1,1,1))
         DO 30 K = 1 , D3
            DO 20 J = 1 , D2
               DO 10 I = 1 , D1
                  D3LINF = MAX(D3LINF,ABS(A(I,J,K)))
 10            CONTINUE
 20         CONTINUE
 30      CONTINUE

         RETURN

      END

c----------------------------------------------------------------------------
c
c     Scales compressed    x, y, z   in--place.
c
c----------------------------------------------------------------------------

      subroutine d3csc(x,y,z,start,ny,
     *                 xmin,xmax,ymin,ymax,zmin,zmax)

         implicit      none 

         logical       ltrace 
         parameter   ( ltrace = .true. )

         real*8        dvmin,      dvmax

         integer       ny
         integer       start(*)
         real*8        x(*),       y(*),       z(*)
         real*8        xmin,       xmax,       ymin,      ymax,
     *                 zmin,       zmax 

         real*8        lxmin,      lxmax,      lymin,     lymax,
     *                 lzmin,      lzmax
         integer       j,          k,          ln

         real*8        scale


         if( ny .lt. 1 ) return
         
         lymin = dvmin(y,ny)
         lymax = dvmax(y,ny)
         
         k  = start(1)
         ln = start(2) - k
         lxmin = dvmin(x(k),ln)
         lxmax = dvmax(x(k),ln)
         lzmin = dvmin(z(k),ln)
         lzmax = dvmax(z(k),ln)
         do 100 j = 2 , ny
            k = start(j)
            ln = start(j+1) - k
            lxmin = min(lxmin,dvmin(x(k),ln))
            lxmax = max(lxmax,dvmax(x(k),ln))
            lzmin = min(lzmin,dvmin(z(k),ln))
            lzmax = max(lzmax,dvmax(z(k),ln))
 100     continue

         write(*,*) '>>> d3csc... Bounding box ...'
         write(*,*) lxmin, lxmax
         write(*,*) lymin, lymax
         write(*,*) lzmin, lzmax

         if( lxmin .ne. lxmax ) then
            scale = (xmax - xmin) / (lxmax - lxmin)
            do 200 j = 1 , start(ny+1) - 1
               x(j) = xmin + scale * (x(j) - lxmin)
 200        continue
         else
            if( ltrace ) then
               write(*,*) '>>> d3csc:: No x range ... '//
     *                    'all x values set to xmin.'
            end if
            call dvls(x,xmin,start(ny+1)-1)
         end if

         if( lymin .ne. lymax ) then
            scale = (ymax - ymin) / (lymax - lymin)
            do 300 j = 1 , ny
               y(j) = ymin + scale * (y(j) - lymin)
 300        continue
         else
            if( ltrace ) then
               write(*,*) '>>> d3csc:: No y range ... '//
     *                    'all y values set to ymin.'
            end if
            call dvls(y,ymin,ny)
         end if

         if( lzmin .ne. lzmax ) then
            scale = (zmax - zmin) / (lzmax - lzmin)
            do 400 j = 1 , start(ny+1) - 1
               z(j) = zmin + scale * (z(j) - lzmin)
 400        continue
         else
            if( ltrace ) then
               write(*,*) '>>> d3csc:: No z range ... '//
     *                    'all z values set to zmin.'
            end if
            call dvls(z,zmin,start(ny+1)-1)
         end if
               
         return

      end

c--------------------------------------------------------------------------
c
c     Dumps compressed 3d data.
c
c--------------------------------------------------------------------------

      subroutine d3cdmp(x,y,z,start,ny)

         implicit      none 

         integer       ny
         integer       start(*)
         real*8        x(*),       y(*),       z(*)

         integer       j,          k

         if( ny .lt. 1 ) return
         
         do 20 j = 1 , ny
            do 10 k = start(j) , start(j+1) - 1
               write(*,*) x(k), y(j), z(k)
 10         continue
 20      continue

         return

      end

c--------------------------------------------------------------------------
c
c     Dumps compressed 3d data in short format.
c
c--------------------------------------------------------------------------

      subroutine d3cdmps(x,y,z,start,ny)

         implicit      none 

         integer       ny
         integer       start(*)
         real*8        x(*),       y(*),       z(*)

         integer       j,          k

         if( ny .lt. 1 ) return
         
         do 20 j = 1 , ny
            do 10 k = start(j) , start(j+1) - 1
               write(*,*) sngl(x(k)), sngl(y(j)), sngl(z(k))
 10         continue
 20      continue

         return

      end

C-----------------------------------------------------------------------
C
C     Utility formatted/unformatted read/write routines ...
C
C-----------------------------------------------------------------------

      subroutine d3fread(a,d1,d2,d3,u)
         
         implicit        none

         integer         d1, d2, d3
         real*8          a(d1,d2,d3)
         integer         u

         read(u,*) a

         return

      end

      subroutine d3fwrite(a,d1,d2,d3,u)
         
         implicit        none

         integer         d1, d2, d3
         real*8          a(d1,d2,d3)
         integer         u

         write(u,*) a

         return

      end

      subroutine d3uread(a,d1,d2,d3,u)
         
         implicit        none

         integer         d1, d2, d3
         real*8          a(d1,d2,d3)
         integer         u

         integer         i1, i2, i3

         read(u) ( ( ( a(i1,i2,i3) , i1 = 1 , d1 ),
     *                               i2 = 1 , d2 ),
     *                               i3 = 1 , d3 )

         return

      end

      subroutine d3uwrite(a,d1,d2,d3,u)
         
         implicit        none

         integer         d1, d2, d3
         real*8          a(d1,d2,d3)
         integer         u

         integer         i1, i2, i3
   
         write(u) ( ( ( a(i1,i2,i3) , i1 = 1 , d1 ),
     *                                i2 = 1 , d2 ),
     *                                i3 = 1 , d3 )

         return

      end

c-----------------------------------------------------------------------
c
c     Dimension checker ...
c
c-----------------------------------------------------------------------

      logical function ccdim(n)

         implicit      none

         integer       n

         ccdim =  mod(n,2) .eq. 1  .and.  n .ge. 5

         return

      end

c-----------------------------------------------------------------------
c
c     2:1 cubic interpolation ...
c
c-----------------------------------------------------------------------

      subroutine d3cint(uc,uf,d1c,d2c,d3c)

         implicit       none

         logical        ccdim

         integer        d1c,     d2c,     d3c
         real*8         uc(d1c,d2c,d3c),
     *                  uf(2*d1c-1,2*d2c-1,2*d3c-1)

         integer        i1,      i2,      i3,
     *                  d1f,     d2f,     d3f

         real*8         lc(4),      cc(4),      rc(4)
         data
     *     lc / 0.31250e0,  0.93750e0, -0.31250e0,  0.06250e0 /,
     *     cc /-0.06250e0,  0.56250e0,  0.56250e0, -0.06250e0 /,
     *     rc / 0.06250e0, -0.31250e0,  0.93750e0,  0.31250e0 /

         if( ccdim(d1c) .and. ccdim(d2c) .and. ccdim(d3c) ) then
            d1f = 2 * d1c - 1
            d2f = 2 * d2c - 1
            d3f = 2 * d3c - 1
            do 20 i3 = 1 , d3c
               do 10 i2 = 1  , d2c
                  call dv2i4(uc(1,i2,i3),uf(1,2*i2-1,2*i3-1),d1c)
 10            continue
 20         continue
            do 40 i3 = 1 , d3f , 2
               call dvsma4(uf(1,1,i3),uf(1,3,i3),
     *                     uf(1,5,i3),uf(1,7,i3),
     *                     uf(1,2,i3),lc,d1f)
                 do 30 i2 = 4 , d2f - 3 , 2
                  call dvsma4(uf(1,i2-3,i3),uf(1,i2-1,i3),
     *                        uf(1,i2+1,i3),uf(1,i2+3,i3),
     *                        uf(1,i2,i3),cc,d1f)
 30            continue
               call dvsma4(uf(1,d2f-6,i3),uf(1,d2f-4,i3),
     *                     uf(1,d2f-2,i3),uf(1,d2f,i3),
     *                     uf(1,d2f-1,i3),rc,d1f)
 40         continue
            do 50 i2 = 1 , d2f
               call dvsma4(uf(1,i2,1),uf(1,i2,3),
     *                     uf(1,i2,5),uf(1,i2,7),
     *                     uf(1,i2,2),lc,d1f)
 50         continue
            do 70 i3 = 4 , d3f - 3 , 2
               do 60 i2 = 1 , d2f
                  call dvsma4(uf(1,i2,i3-3),uf(1,i2,i3-1),
     *                        uf(1,i2,i3+1),uf(1,i2,i3+3),
     *                        uf(1,i2,i3),cc,d1f)
 60            continue
 70         continue   
            do 80 i2 = 1 , d2f
               call dvsma4(uf(1,i2,d3f-6),uf(1,i2,d3f-4),
     *                     uf(1,i2,d3f-2),uf(1,i2,d3f),
     *                     uf(1,i2,d3f-1),rc,d1f)
 80         continue
            
         else
            write(*,*) '>>> d3cint: Invalid dimension(s): ',
     *                 d1c, d2c, d3c
         end if

         return

      end


c-----------------------------------------------------------------------
c
c     Element-wise storage and retrieval ...
c
c-----------------------------------------------------------------------

      double precision function d3get(a,d1,d2,d3,i1,i2,i3)

            implicit       none

            integer        d1,      d2,      d3
            real*8         a(d1,d2,d3) 
            integer        i1,      i2,      i3

            d3get = a(i1,i2,i3)

            return 
   
      end

      subroutine d3put(a,d1,d2,d3,i1,i2,i3,val)

            implicit       none

            integer        d1,      d2,      d3
            real*8         a(d1,d2,d3) 
            integer        i1,      i2,      i3
            real*8         val 

            a(i1,i2,i3) = val

            return 
   
      end

c-----------------------------------------------------------------------
c
c     Linear interpolation in "generalized uniform" mesh function
c     f(1:nx,1:ny,1:nz) to (xp,yp,zp) ...
c
c-----------------------------------------------------------------------

      double precision function d3lin0(f,x,y,z,nx,ny,nz,xp,yp,zp,def)

         implicit      none

         real*8        mm01
         integer       indinf

         integer       nx,        ny,       nz
         real*8        f(nx,ny,nz),
     *                 x(nx),     y(ny),    z(nz)
         real*8        xp,        yp,       zp
         real*8        def


         integer       ip,        jp,       kp

         real*8        alpha,     beta,     gamma     

         real*8        ftemp2(0:1,0:1),     ftemp1(0:1)
         integer       j,                   k

         logical       ltrace
         parameter   ( ltrace = .false. )

         ip = indinf(x,nx,xp)
         jp = indinf(y,ny,yp)
         kp = indinf(z,nz,zp)

         if( ip .eq. 0  .or.  jp .eq. 0  .or.  kp .eq. 0 ) then
            if( ltrace ) then
               write(0,*) '<<< d3lin0: domain error ... >>>' 
               write(0,*) xp, yp, zp
            end if
            d3lin0 = def
            return
         end if

         alpha = mm01((xp - x(ip)) / (x(ip+1) - x(ip)))
         beta  = mm01((yp - y(jp)) / (y(jp+1) - y(jp)))
         gamma = mm01((zp - z(kp)) / (z(kp+1) - z(kp)))

         do j = 0 , 1
            do k = 0 , 1
               ftemp2(j,k) = alpha *            f(ip+1,jp+j,kp+k) + 
     *                       (1.0d0 - alpha ) * f(ip,jp+j,kp+k)
            end do
         end do

         do k = 0 , 1
            ftemp1(k) = beta *            ftemp2(1,k) + 
     *                  (1.0d0 - beta ) * ftemp2(0,k)
         end do

         d3lin0 = gamma * ftemp1(1) + 
     *            (1.0d0 - gamma) * ftemp1(0)

         return

      end

      integer function indinf(v,n,vkey)

         implicit      none

         integer       n
         real*8        v(n) 
         real*8        vkey
         
         integer       i

         real*8        fuzz
         parameter   ( fuzz = 1.0d-10 )

         if( vkey .lt. v(1) - fuzz ) then
            indinf = 0
         else
            do i = 1 , n - 1
               if( v(i+1) .gt. vkey ) then
                  indinf = i
                  return
               end if
            end do
            if( vkey .le. v(n) + fuzz ) then
               indinf = n - 1
            else 
               indinf = 0
            end if
         end if 

         return

      end

      double precision function mm01(x)

         real*8        x

         mm01 = max(min(x,1.0d0),0.0d0)

         return

      end

c-----------------------------------------------------------------------
c
c     Defines a test function ...
c
c-----------------------------------------------------------------------

      subroutine d3tfcn(a,d1,d2,d3)

         implicit       none

         integer        d1,    d2,     d3
         real*8         a(d1,d2,d3)

         integer        i1,    i2,     i3

         do i3 = 1 , d3
            do i2 = 1 , d2
               do i1 = 1 , d1
                  a(i1,i2,i3) = i3 * 1 000 000 + i2 * 1 000 + i1
               end do
            end do
         end do

         return

      end

c-----------------------------------------------------------------------
c
c     "Packs" a(d1,d2,d3) in place so that on return
c     a(1 : d1hi - d1lo + 1, ...) corresponds to a(d1lo : d1hi, ...).
c     "Border values" saved in work array t for recovery with
c     d3unpackbdr. Amount of temporary storage used returned in nt.
c
c-----------------------------------------------------------------------

      subroutine d3packbdr(a,d1,d2,d3,d1lo,d1hi,d2lo,d2hi,
     *                     d3lo,d3hi,t,nt)

         implicit          none

         integer           d1,      d2,      d3,
     *                     d1lo,    d1hi,    d2lo,    d2hi,
     *                     d3lo,    d3hi,
     *                     nt

         real*8            a(d1,d2,d3),
     *                     t(*)

         integer           i1,      i2,      i3, 
     *                     ip,      it,      nd1

         nt = 0

         if( d1lo .lt. 1  .or.  d1hi .gt. d1  .or. 
     *       d2lo .lt. 1  .or.  d2hi .gt. d2  .or.
     *       d3lo .lt. 1  .or.  d3hi .gt. d3  .or.
     *       d1lo .gt. d1hi  .or.  d2lo .gt. d2hi  .or.
     *       d3lo .gt. d3hi
     *     ) then
            write(*,*) '>>> d3packbdr: Invalid subrange(s).' 
            return
         end if

c
c        Save planes ... some values stored more than once to expedite 
c        flow ...
c
         it = 1

         do i1 = 1 , d1lo - 1
            do i2 = 1 , d2
               do i3 = 1 , d3
                  t(it) = a(i1,i2,i3)
                  it = it + 1
               end do
            end do
         end do
         do i1 = d1hi + 1 , d1
            do i2 = 1 , d2
               do i3 = 1 , d3
                  t(it) = a(i1,i2,i3)
                  it = it + 1
               end do
            end do
         end do

         do i2 = 1 , d2lo - 1
            do i1 = 1 , d1
               do i3 = 1 , d3
                  t(it) = a(i1,i2,i3)
                  it = it + 1
               end do
            end do
         end do
         do i2 = d2hi + 1 , d2
            do i1 = 1 , d1
               do i3 = 1 , d3
                  t(it) = a(i1,i2,i3)
                  it = it + 1
               end do
            end do
         end do

         do i3 = 1 , d3lo - 1
            do i1 = 1 , d1
               do i2 = 1 , d2
                  t(it) = a(i1,i2,i3)
                  it = it + 1
               end do
            end do
         end do
         do i3 = d3hi + 1 , d3
            do i1 = 1 , d1
               do i2 = 1 , d2
                  t(it) = a(i1,i2,i3)
                  it = it + 1
               end do
            end do
         end do

         nt = it - 1
c
c        Pack ...
c
         ip = 1
         nd1 = d1hi - d1lo + 1
         do i3 = d3lo , d3hi
            do i2 = d2lo , d2hi
               call dvcopy(a(d1lo,i2,i3),a(ip,1,1),nd1)
               ip = ip + nd1 
            end do
         end do 

         return

      end

c-----------------------------------------------------------------------
c
c     Inverse of d3packbdr: recovers original array from packed rep 
c     and auxiliary storage t.
c
c-----------------------------------------------------------------------

      subroutine d3unpackbdr(a,d1,d2,d3,d1lo,d1hi,d2lo,d2hi,
     *                       d3lo,d3hi,t,nt)

         implicit          none

         integer           d1,      d2,      d3,
     *                     d1lo,    d1hi,    d2lo,    d2hi,
     *                     d3lo,    d3hi,
     *                     nt

         real*8            a(d1,d2,d3),
     *                     t(*)

         integer           i1,      i2,      i3, 
     *                     ip,      it,      nd1

         nt = 0

         if( d1lo .lt. 1  .or.  d1hi .gt. d1  .or.
     *       d2lo .lt. 1  .or.  d2hi .gt. d2  .or.
     *       d3lo .lt. 1  .or.  d3hi .gt. d3  .or.
     *       d1lo .gt. d1hi  .or.  d2lo .gt. d2hi  .or.
     *       d3lo .gt. d3hi
     *     ) then
            write(*,*) '>>> d3packbdr: Invalid subrange(s).' 
            return
         end if
c
c        Unpack ...
c
         nd1 = d1hi - d1lo + 1
         ip  = ((d2hi - d2lo + 1) * (d3hi - d3lo + 1) - 1) *
     *         nd1 + 1
         do i3 = d3hi , d3lo , -1
            do i2 = d2hi , d2lo , -1
               call dvcopr(a(ip,1,1),a(d1lo,i2,i3),nd1)
               ip = ip - nd1 
            end do
         end do 

c
c        Recover planes ...
c
         it = 1

         do i1 = 1 , d1lo - 1
            do i2 = 1 , d2
               do i3 = 1 , d3
                  a(i1,i2,i3) = t(it)
                  it = it + 1
               end do
            end do
         end do
         do i1 = d1hi + 1 , d1
            do i2 = 1 , d2
               do i3 = 1 , d3
                  a(i1,i2,i3) = t(it)
                  it = it + 1
               end do
            end do
         end do

         do i2 = 1 , d2lo - 1
            do i1 = 1 , d1
               do i3 = 1 , d3
                  a(i1,i2,i3) = t(it)
                  it = it + 1
               end do
            end do
         end do
         do i2 = d2hi + 1 , d2
            do i1 = 1 , d1
               do i3 = 1 , d3
                  a(i1,i2,i3) = t(it)
                  it = it + 1
               end do
            end do
         end do

         do i3 = 1 , d3lo - 1
            do i1 = 1 , d1
               do i2 = 1 , d2
                  a(i1,i2,i3) = t(it)
                  it = it + 1
               end do
            end do
         end do
         do i3 = d3hi + 1 , d3
            do i1 = 1 , d1
               do i2 = 1 , d2
                  a(i1,i2,i3) = t(it)
                  it = it + 1
               end do
            end do
         end do

         return

      end

c---------------------------------------------------------------------
c
c     pth-order polynomial interpolation in uniform mesh ...
c
c     uses uflipn from dveclib.f
c
c---------------------------------------------------------------------

      double precision function fd3umint(f,d1,d2,d3,d10,d20,d30,    
     *                                   dd1,dd2,dd3,cp1,cp2,cp3,
     *                                   p,fdef)

         implicit       none

         real*8         uflipn

         integer        d1,      d2,      d3
         real*8         f(d1,d2,d3)
         real*8         d10,     d20,     d30,
     *                  dd1,     dd2,     dd3,
     *                  cp1,     cp2,     cp3,      fdef
         integer        p

         integer        pmax 
         parameter    ( pmax = 20 )
         real*8         ft(0:pmax-1,0:pmax-1),      ftt(0:pmax-1),
     *                  s1(0:pmax-1),               s2(0:pmax-1),
     *                  s3(0:pmax-1)
         integer        kl1,     kl2,     kl3,
     *                           i2,      i3

c
c        Compute indices of corner of interpolation box and associated 
c        offsets from interpolation point (cp1,cp2,cp3).
c
         call cmposo(d10,dd1,d1,cp1,p,kl1,s1)
         call cmposo(d20,dd2,d2,cp2,p,kl2,s2)
         call cmposo(d30,dd3,d3,cp3,p,kl3,s3)
         if( kl1 .le. 0  .or.  kl2 .le. 0  .or.  kl3 .le. 0 ) then
            fd3umint = fdef
            return
         end if
c
c        Interpolate in '1' direction ...
c
         do i2 = 0 , p - 1
            do i3 = 0 , p - 1
               ft(i2,i3) = uflipn(f(kl1,kl2 + i2,kl3 + i3),s1,p)
            end do
         end do
c
c        Interpolate in '2' direction ...
c
         do i3 = 0 , p - 1
            ftt(i3) = uflipn(ft(0,i3),s2,p)
         end do
c
c        Interpolate in '3' direction ...
c
         fd3umint = uflipn(ftt(0),s3,p)

         return

      end

c---------------------------------------------------------------------
c
c     Variation of fd3umint for use in interpolating scott's psi
c     which checks for negative values of f ...
c
c---------------------------------------------------------------------

      double precision function fd3umint_ck(f,d1,d2,d3,d10,d20,d30,    
     *                                      dd1,dd2,dd3,cp1,cp2,cp3,
     *                                      p,fdef)

         implicit       none

         real*8         uflipn_ck

         integer        d1,      d2,      d3
         real*8         f(d1,d2,d3)
         real*8         d10,     d20,     d30,
     *                  dd1,     dd2,     dd3,
     *                  cp1,     cp2,     cp3,      fdef
         integer        p

         integer        pmax 
         parameter    ( pmax = 20 )
         real*8         ft(0:pmax-1,0:pmax-1),      ftt(0:pmax-1),
     *                  s1(0:pmax-1),               s2(0:pmax-1),
     *                  s3(0:pmax-1)
         integer        kl1,     kl2,     kl3,
     *                           i2,      i3

c
c        Compute indices of corner of interpolation box and associated 
c        offsets from interpolation point (cp1,cp2,cp3).
c
         call cmposo(d10,dd1,d1,cp1,p,kl1,s1)
         call cmposo(d20,dd2,d2,cp2,p,kl2,s2)
         call cmposo(d30,dd3,d3,cp3,p,kl3,s3)
         if( kl1 .le. 0  .or.  kl2 .le. 0  .or.  kl3 .le. 0 ) then
            fd3umint_ck = fdef
            return
         end if
c
c        Interpolate in '1' direction ...
c
         do i2 = 0 , p - 1
            do i3 = 0 , p - 1
               ft(i2,i3) = uflipn_ck(f(kl1,kl2 + i2,kl3 + i3),s1,p,fdef)
               if( ft(i2,i3) .eq. fdef ) then
                  fd3umint_ck = fdef
                  return
               end if
            end do
         end do
c
c        Interpolate in '2' direction ...
c
         do i3 = 0 , p - 1
            ftt(i3) = uflipn_ck(ft(0,i3),s2,p,fdef)
         end do
c
c        Interpolate in '3' direction ...
c
         fd3umint_ck = uflipn_ck(ftt(0),s3,p,fdef)

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

      double precision function uflipn_ck(y,s,p,ydef)

         implicit      none

         integer       p
         real*8        y(0:p-1),       s(0:p-1),
     *                 ydef

         integer       pmax
         parameter   ( pmax = 100 )
         real*8        pp(0:pmax-1)

         real*8        mkm1 
         integer       j,               k

         do k = 0 , p - 1
            if( y(k) .lt. 0.0d0 ) then 
               write(0,*) 'uflipn_ck: Warning, negative value '//
     *                    'encountered'
               uflipn_ck = ydef
               return
            end if
            pp(k) = y(k)
         end do
         do k = 1 , p - 1
            mkm1 = -1.0d0 / k
            do j = 0 , p - (k + 1)
               pp(j) = mkm1 * (s(j+k) * pp(j) - s(j) * pp(j+1))
            end do
         end do

         uflipn_ck = pp(0)

         return

      end

C-----------------------------------------------------------------------
C
C     Multiply 3-array with scalar ...
C
C-----------------------------------------------------------------------

      subroutine d3sm(a1,s1,a2,d1,d2,d3)

         implicit         none

         integer          d1,      d2,      d3

         real*8           a1(d1,d2,d3),     a2(d1,d2,d3)
         real*8           s1

         call dvsm(a1,s1,a2,d1*d2*d3)

         return

      end

C-----------------------------------------------------------------------
C
C     Add scalar to 3-array  ...
C
C-----------------------------------------------------------------------

      subroutine d3sa(a1,s1,a2,d1,d2,d3)

         implicit         none

         integer          d1,      d2,      d3

         real*8           a1(d1,d2,d3),     a2(d1,d2,d3)
         real*8           s1

         call dvsa(a1,s1,a2,d1*d2*d3)

         return

      end

C-----------------------------------------------------------------------
C
C     Assumes a1 is uniform mesh (single discretization scale) function,
C     applies 7-pt O(h^2) finite diff. approx to Laplacian and returns
C     in a2 ... boundary points of a2 unchanged.
C
C-----------------------------------------------------------------------

      subroutine d3lap7pt(a1,a2,h,d1,d2,d3)

         implicit         none

         integer          d1,      d2,      d3

         real*8           a1(d1,d2,d3),     a2(d1,d2,d3)
         real*8           h 

         real*8           hm2
         integer          i1,      i2,      i3

         if ( h .gt. 0.0d0 ) then
            hm2 = 1.0d0 / (h * h)
            do i3 = 2 , d3 - 1
               do i2 = 2 , d2 - 1
                  do i1 = 2 , d1 - 1
                     a2(i1,i2,i3) = hm2 * ( -6.0d0 * a1(i1,i2,i3) +
     *                               a1(i1+1,i2,i3) + a1(i1-1,i2,i3) +
     *                               a1(i1,i2+1,i3) + a1(i1,i2-1,i3) +
     *                               a1(i1,i2,i3+1) + a1(i1,i2,i3-1) 
     *                                    )
                  end do
               end do
            end do
         end if

         return

      end

c-----------------------------------------------------------------------
c
c     Multiply 3-array by r^p
c
c-----------------------------------------------------------------------

      subroutine d3rmult(a1,x,y,z,a2,d1,d2,d3,p)

         implicit         none

         integer          d1,      d2,      d3

         real*8           a1(d1,d2,d3),     a2(d1,d2,d3)
         real*8           x(d1),            y(d2),         z(d3)
         integer          p

         real*8           r
         integer          i,     j,    k

         logical          ltrace
         parameter      ( ltrace = .false. )

         if( ltrace ) then
            call dvdump(x,d1,'x',6)
            call dvdump(y,d2,'y',6)
            call dvdump(z,d3,'z',6)
         end if

         do k = 1 , d3
            do j = 1 , d2
               do i = 1 , d1
                  r = sqrt(x(i)**2 + y(j)**2 + z(k)**2)
                  a2(i,j,k) = a1(i,j,k) * r**p
               end do
            end do
         end do

         return

      end

c-----------------------------------------------------------------------
c     Selects "slices" of input array, ain, returning values in aout.
c-----------------------------------------------------------------------
      subroutine d3slice(ain,aout,d1in,d2in,d3in,d1out,d2out,d3out,
     &                   d1sl,d2sl,d3sl)

         implicit     none
         integer      d1in, d2in, d3in, d1out, d2out, d3out
         real*8       ain(d1in,d2in,d3in), aout(d1out,d2out,d3out)
         integer      d1sl(d1out), d2sl(d2out), d3sl(d3out)
         
         integer      i, j, k 

         do k = 1 , d3out
            do j = 1 , d2out
               do i = 1 , d1out
                  aout(i,j,k) = ain(d1sl(i),d2sl(j),d3sl(k))
               end do
            end do
         end do

         return  

      end
