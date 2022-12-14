      SUBROUTINE CHPT01( UPLO, N, A, AFAC, IPIV, C, LDC, RWORK, RESID )
*
*  -- LAPACK test routine (version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     September 30, 1994
*
*     .. Scalar Arguments ..
      CHARACTER          UPLO
      INTEGER            LDC, N
      REAL               RESID
*     ..
*     .. Array Arguments ..
      INTEGER            IPIV( * )
      REAL               RWORK( * )
      COMPLEX            A( * ), AFAC( * ), C( LDC, * )
*     ..
*
*  Purpose
*  =======
*
*  CHPT01 reconstructs a Hermitian indefinite packed matrix A from its
*  block L*D*L' or U*D*U' factorization and computes the residual
*     norm( C - A ) / ( N * norm(A) * EPS ),
*  where C is the reconstructed matrix, EPS is the machine epsilon,
*  L' is the conjugate transpose of L, and U' is the conjugate transpose
*  of U.
*
*  Arguments
*  ==========
*
*  UPLO    (input) CHARACTER*1
*          Specifies whether the upper or lower triangular part of the
*          Hermitian matrix A is stored:
*          = 'U':  Upper triangular
*          = 'L':  Lower triangular
*
*  N       (input) INTEGER
*          The number of rows and columns of the matrix A.  N >= 0.
*
*  A       (input) COMPLEX array, dimension (N*(N+1)/2)
*          The original Hermitian matrix A, stored as a packed
*          triangular matrix.
*
*  AFAC    (input) COMPLEX array, dimension (N*(N+1)/2)
*          The factored form of the matrix A, stored as a packed
*          triangular matrix.  AFAC contains the block diagonal matrix D
*          and the multipliers used to obtain the factor L or U from the
*          block L*D*L' or U*D*U' factorization as computed by CHPTRF.
*
*  IPIV    (input) INTEGER array, dimension (N)
*          The pivot indices from CHPTRF.
*
*  C       (workspace) COMPLEX array, dimension (LDC,N)
*
*  LDC     (integer) INTEGER
*          The leading dimension of the array C.  LDC >= max(1,N).
*
*  RWORK   (workspace) REAL array, dimension (N)
*
*  RESID   (output) REAL
*          If UPLO = 'L', norm(L*D*L' - A) / ( N * norm(A) * EPS )
*          If UPLO = 'U', norm(U*D*U' - A) / ( N * norm(A) * EPS )
*
*  =====================================================================
*
*     .. Parameters ..
      REAL               ZERO, ONE
      PARAMETER          ( ZERO = 0.0E+0, ONE = 1.0E+0 )
      COMPLEX            CZERO, CONE
      PARAMETER          ( CZERO = ( 0.0E+0, 0.0E+0 ),
     $                   CONE = ( 1.0E+0, 0.0E+0 ) )
*     ..
*     .. Local Scalars ..
      INTEGER            I, INFO, J, JC
      REAL               ANORM, EPS
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      REAL               CLANHE, CLANHP, SLAMCH
      EXTERNAL           LSAME, CLANHE, CLANHP, SLAMCH
*     ..
*     .. External Subroutines ..
      EXTERNAL           CLAVHP, CLASET
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          AIMAG, REAL
*     ..
*     .. Executable Statements ..
*
*     Quick exit if N = 0.
*
      IF( N.LE.0 ) THEN
         RESID = ZERO
         RETURN
      END IF
*
*     Determine EPS and the norm of A.
*
      EPS = SLAMCH( 'Epsilon' )
      ANORM = CLANHP( '1', UPLO, N, A, RWORK )
*
*     Check the imaginary parts of the diagonal elements and return with
*     an error code if any are nonzero.
*
      JC = 1
      IF( LSAME( UPLO, 'U' ) ) THEN
         DO 10 J = 1, N
            IF( AIMAG( AFAC( JC ) ).NE.ZERO ) THEN
               RESID = ONE / EPS
               RETURN
            END IF
            JC = JC + J + 1
   10    CONTINUE
      ELSE
         DO 20 J = 1, N
            IF( AIMAG( AFAC( JC ) ).NE.ZERO ) THEN
               RESID = ONE / EPS
               RETURN
            END IF
            JC = JC + N - J + 1
   20    CONTINUE
      END IF
*
*     Initialize C to the identity matrix.
*
      CALL CLASET( 'Full', N, N, CZERO, CONE, C, LDC )
*
*     Call CLAVHP to form the product D * U' (or D * L' ).
*
      CALL CLAVHP( UPLO, 'Conjugate', 'Non-unit', N, N, AFAC, IPIV, C,
     $             LDC, INFO )
*
*     Call CLAVHP again to multiply by U ( or L ).
*
      CALL CLAVHP( UPLO, 'No transpose', 'Unit', N, N, AFAC, IPIV, C,
     $             LDC, INFO )
*
*     Compute the difference  C - A .
*
      IF( LSAME( UPLO, 'U' ) ) THEN
         JC = 0
         DO 40 J = 1, N
            DO 30 I = 1, J - 1
               C( I, J ) = C( I, J ) - A( JC+I )
   30       CONTINUE
            C( J, J ) = C( J, J ) - REAL( A( JC+J ) )
            JC = JC + J
   40    CONTINUE
      ELSE
         JC = 1
         DO 60 J = 1, N
            C( J, J ) = C( J, J ) - REAL( A( JC ) )
            DO 50 I = J + 1, N
               C( I, J ) = C( I, J ) - A( JC+I-J )
   50       CONTINUE
            JC = JC + N - J + 1
   60    CONTINUE
      END IF
*
*     Compute norm( C - A ) / ( N * norm(A) * EPS )
*
      RESID = CLANHE( '1', UPLO, N, C, LDC, RWORK )
*
      IF( ANORM.LE.ZERO ) THEN
         IF( RESID.NE.ZERO )
     $      RESID = ONE / EPS
      ELSE
         RESID = ( ( RESID / REAL( N ) ) / ANORM ) / EPS
      END IF
*
      RETURN
*
*     End of CHPT01
*
      END
