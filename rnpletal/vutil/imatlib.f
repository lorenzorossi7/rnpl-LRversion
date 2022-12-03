C-----------------------------------------------------------------------
C
C     I N T E G E R    M A T R I X    R O U T I N E S
C
C-----------------------------------------------------------------------

C-----------------------------------------------------------------------
C
C     Matrix-matrix add.
C
C-----------------------------------------------------------------------
C
      SUBROUTINE IMMA(A1,A2,A3,M,N)
C
         IMPLICIT       LOGICAL   (A-Z)
C
         INTEGER        M,        N
         INTEGER        A1(M,N),  A2(M,N),  A3(M,N)
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
      SUBROUTINE IMMS(A1,A2,A3,M,N)
C
         IMPLICIT       LOGICAL   (A-Z)
C
         INTEGER        M,        N
         INTEGER        A1(M,N),  A2(M,N),  A3(M,N)
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
      SUBROUTINE IMMM(A1,A2,A3,M,N)
C
         IMPLICIT       LOGICAL   (A-Z)
C
         INTEGER        M,        N
         INTEGER        A1(M,N),  A2(M,N),  A3(M,N)
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
      SUBROUTINE IMMD(A1,A2,A3,M,N)
C
         IMPLICIT       LOGICAL   (A-Z)
C
         INTEGER        M,        N
         INTEGER        A1(M,N),  A2(M,N),  A3(M,N)
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
C     Load matrix with scalar.
C
C-----------------------------------------------------------------------
C
      SUBROUTINE IMLS(A,S,M,N)
C
         IMPLICIT       LOGICAL   (A-Z)
C
         INTEGER        M,        N
         INTEGER        A(M,N)
         INTEGER        S
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
      SUBROUTINE IMDUMP(A,M,N,LABEL,UNIT)
C
         IMPLICIT       LOGICAL   (A-Z)
C
         INTEGER        M,        N,        UNIT
         INTEGER        A(M,N)
         CHARACTER*(*)  LABEL
         INTEGER        I,        J,        ST
C
         IF( N .GT. 0  .AND.  M .GT. 0 ) THEN
            WRITE(UNIT,100) LABEL
 100        FORMAT( /' <<< ',A,' >>>'/)
            DO 200 J = 1 , N
               ST = 1
 110           CONTINUE
                  WRITE(UNIT,120) ( A(I,J) , I = ST , MIN(ST+5,M))
 120              FORMAT(' ',6I12)
                  ST = ST + 6
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
      SUBROUTINE IMRLOD(A,V,M,N,I)
C
         IMPLICIT       LOGICAL   (A-Z)
C
         INTEGER        M,        N,        I
         INTEGER        A(M,N),   V(N)
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
      SUBROUTINE IMRSTO(A,V,M,N,I)
C
         IMPLICIT       LOGICAL   (A-Z)
C
         INTEGER        M,        N,        I
         INTEGER        A(M,N),   V(N)
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
      SUBROUTINE IMIMSA(A1,A2,A3,S1,S2,M,N)
C
         IMPLICIT       LOGICAL   (A-Z)
C
         INTEGER        M,        N
         INTEGER        A1(M,N),  A2(M,N),  A3(M,N)
         INTEGER        S1,       S2
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
      SUBROUTINE IMCOPY(A1,A2,M,N)
C
         IMPLICIT       LOGICAL   (A-Z)
C
         INTEGER        M,        N
         INTEGER        A1(M,N),  A2(M,N)
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
C
C----------------------------------------------------------------------------
C
C     Intializes matrix to "ramp" function (bi--linear function of
C     indices.
C
C----------------------------------------------------------------------------

      SUBROUTINE IMRAMP(A,D1,D2,X10,DX1,X20,DX2)

         IMPLICIT     LOGICAL  (A-Z)

         INTEGER      D1, D2

         INTEGER      A(D1,D2)

         INTEGER      X10, DX1, X20, DX2

         INTEGER      BASE
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

      SUBROUTINE IMFAPL(A,FOFA,M,N,F)

         INTEGER      M, N
         INTEGER      A(M,N), FOFA(M,N)

         EXTERNAL     F
         INTEGER      F

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
      INTEGER FUNCTION IMSUM(A,M,N)
C
         IMPLICIT       NONE
C
         INTEGER        M,        N
         INTEGER        A(M,N)
C
         INTEGER        I,        J
C
         IMSUM = 0.0E0
         DO 20 J = 1 , N
            DO 10 I = 1 , M
               IMSUM = IMSUM + A(I,J)
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

      INTEGER FUNCTION IMMIN(A,M,N)

         IMPLICIT     NONE

         INTEGER      M, N

         INTEGER      A(M,N)

         INTEGER      I, J

         IMMIN = A(1,1)
         DO 20 J = 1 , N
            DO 10 I = 1 , M
              IMMIN = MIN(IMMIN,A(I,J))
 10         CONTINUE
 20      CONTINUE

         RETURN

      END

      INTEGER FUNCTION IMMAX(A,M,N)

         IMPLICIT     NONE

         INTEGER      M, N

         INTEGER      A(M,N)

         INTEGER      I, J

         IMMAX = A(1,1)
         DO 20 J = 1 , N
            DO 10 I = 1 , M
              IMMAX = MAX(IMMAX,A(I,J))
 10         CONTINUE
 20      CONTINUE

         RETURN

      END

      INTEGER FUNCTION IMLINF(A,M,N)

         IMPLICIT     NONE

         INTEGER      M, N

         INTEGER      A(M,N)

         INTEGER      I, J

         IMLINF = ABS(A(1,1))
         DO 20 J = 1 , N
            DO 10 I = 1 , M
              IMLINF = MAX(IMLINF,ABS(A(I,J)))
 10         CONTINUE
 20      CONTINUE

         RETURN

      END

c-----------------------------------------------------------------------
c
c     Write 2-matrix formatted by row ...
c
c-----------------------------------------------------------------------

      subroutine imwritefr(a,d1,d2,u)

         implicit         none

         integer          d1,      d2,      u
         integer          a(d1,d2)

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

      subroutine imreadfr(a,d1,d2,u)

         implicit         none

         integer          d1,      d2,      u
         integer          a(d1,d2)

         integer          i1,      i2

         do i2 = 1 , d2
            read(u,*) ( a(i1,i2), i1 = 1 , d1 )
         end do
   
         return

      end
