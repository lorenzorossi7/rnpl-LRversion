      SUBROUTINE CGET01( M, N, A, LDA, AFAC, LDAFAC, IPIV, RWORK,
     $                   RESID )
*
*  -- LAPACK test routine (version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     September 30, 1994
*
*     .. Scalar Arguments ..
      INTEGER            LDA, LDAFAC, M, N
      REAL               RESID
*     ..
*     .. Array Arguments ..
      INTEGER            IPIV( * )
      REAL               RWORK( * )
      COMPLEX            A( LDA, * ), AFAC( LDAFAC, * )
*     ..
*
*  Purpose
*  =======
*
*  CGET01 reconstructs a matrix A from its L*U factorization and
*  computes the residual
*     norm(L*U - A) / ( N * norm(A) * EPS ),
*  where EPS is the machine epsilon.
*
*  Arguments
*  ==========
*
*  M       (input) INTEGER
*          The number of rows of the matrix A.  M >= 0.
*
*  N       (input) INTEGER
*          The number of columns of the matrix A.  N >= 0.
*
*  A       (input) COMPLEX array, dimension (LDA,N)
*          The original M x N matrix A.
*
*  LDA     (input) INTEGER
*          The leading dimension of the array A.  LDA >= max(1,M).
*
*  AFAC    (input/output) COMPLEX array, dimension (LDAFAC,N)
*          The factored form of the matrix A.  AFAC contains the factors
*          L and U from the L*U factorization as computed by CGETRF.
*          Overwritten with the reconstructed matrix, and then with the
*          difference L*U - A.
*
*  LDAFAC  (input) INTEGER
*          The leading dimension of the array AFAC.  LDAFAC >= max(1,M).
*
*  IPIV    (input) INTEGER array, dimension (N)
*          The pivot indices from CGETRF.
*
*  RWORK   (workspace) REAL array, dimension (M)
*
*  RESID   (output) REAL
*          norm(L*U - A) / ( N * norm(A) * EPS )
*
*  =====================================================================
*
*     .. Parameters ..
      REAL               ONE, ZERO
      PARAMETER          ( ZERO = 0.0E+0, ONE = 1.0E+0 )
      COMPLEX            CONE
      PARAMETER          ( CONE = ( 1.0E+0, 0.0E+0 ) )
*     ..
*     .. Local Scalars ..
      INTEGER            I, J, K
      REAL               ANORM, EPS
      COMPLEX            T
*     ..
*     .. External Functions ..
      REAL               CLANGE, SLAMCH
      COMPLEX            CDOTU
      EXTERNAL           CLANGE, SLAMCH, CDOTU
*     ..
*     .. External Subroutines ..
      EXTERNAL           CGEMV, CLASWP, CSCAL, CTRMV
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MIN, REAL
*     ..
*     .. Executable Statements ..
*
*     Quick exit if M = 0 or N = 0.
*
      IF( M.LE.0 .OR. N.LE.0 ) THEN
         RESID = ZERO
         RETURN
      END IF
*
*     Determine EPS and the norm of A.
*
      EPS = SLAMCH( 'Epsilon' )
      ANORM = CLANGE( '1', M, N, A, LDA, RWORK )
*
*     Compute the product L*U and overwrite AFAC with the result.
*     A column at a time of the product is obtained, starting with
*     column N.
*
      DO 10 K = N, 1, -1
         IF( K.GT.M ) THEN
            CALL CTRMV( 'Lower', 'No transpose', 'Unit', M, AFAC,
     $                  LDAFAC, AFAC( 1, K ), 1 )
         ELSE
*
*           Compute elements (K+1:M,K)
*
            T = AFAC( K, K )
            IF( K+1.LE.M ) THEN
               CALL CSCAL( M-K, T, AFAC( K+1, K ), 1 )
               CALL CGEMV( 'No transpose', M-K, K-1, CONE,
     $                     AFAC( K+1, 1 ), LDAFAC, AFAC( 1, K ), 1, 
     $                     CONE, AFAC( K+1, K ), 1 )
            END IF
*
*           Compute the (K,K) element
*
            AFAC( K, K ) = T + CDOTU( K-1, AFAC( K, 1 ), LDAFAC,
     $                     AFAC( 1, K ), 1 )
*
*           Compute elements (1:K-1,K)
*
            CALL CTRMV( 'Lower', 'No transpose', 'Unit', K-1, AFAC,
     $                  LDAFAC, AFAC( 1, K ), 1 )
         END IF
   10 CONTINUE
      CALL CLASWP( N, AFAC, LDAFAC, 1, MIN( M, N ), IPIV, -1 )
*
*     Compute the difference  L*U - A  and store in AFAC.
*
      DO 30 J = 1, N
         DO 20 I = 1, M
            AFAC( I, J ) = AFAC( I, J ) - A( I, J )
   20    CONTINUE
   30 CONTINUE
*
*     Compute norm( L*U - A ) / ( N * norm(A) * EPS )
*
      RESID = CLANGE( '1', M, N, AFAC, LDAFAC, RWORK )
*
      IF( ANORM.LE.ZERO ) THEN
         IF( RESID.NE.ZERO )
     $      RESID = ONE / EPS
      ELSE
         RESID = ( ( RESID/REAL( N ) )/ANORM ) / EPS
      END IF
*
      RETURN
*
*     End of CGET01
*
      END
