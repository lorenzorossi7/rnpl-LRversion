      SUBROUTINE ZSYT01( UPLO, N, A, LDA, AFAC, LDAFAC, IPIV, C, LDC,
     $                   RWORK, RESID )
*
*  -- LAPACK test routine (version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     September 30, 1994
*
*     .. Scalar Arguments ..
      CHARACTER          UPLO
      INTEGER            LDA, LDAFAC, LDC, N
      DOUBLE PRECISION   RESID
*     ..
*     .. Array Arguments ..
      INTEGER            IPIV( * )
      DOUBLE PRECISION   RWORK( * )
      COMPLEX*16         A( LDA, * ), AFAC( LDAFAC, * ), C( LDC, * )
*     ..
*
*  Purpose
*  =======
*
*  ZSYT01 reconstructs a complex symmetric indefinite matrix A from its
*  block L*D*L' or U*D*U' factorization and computes the residual
*     norm( C - A ) / ( N * norm(A) * EPS ),
*  where C is the reconstructed matrix, EPS is the machine epsilon,
*  L' is the transpose of L, and U' is the transpose of U.
*
*  Arguments
*  ==========
*
*  UPLO    (input) CHARACTER*1
*          Specifies whether the upper or lower triangular part of the
*          complex symmetric matrix A is stored:
*          = 'U':  Upper triangular
*          = 'L':  Lower triangular
*
*  N       (input) INTEGER
*          The number of rows and columns of the matrix A.  N >= 0.
*
*  A       (input) COMPLEX*16 array, dimension (LDA,N)
*          The original complex symmetric matrix A.
*
*  LDA     (input) INTEGER
*          The leading dimension of the array A.  LDA >= max(1,N)
*
*  AFAC    (input) COMPLEX*16 array, dimension (LDAFAC,N)
*          The factored form of the matrix A.  AFAC contains the block
*          diagonal matrix D and the multipliers used to obtain the
*          factor L or U from the block L*D*L' or U*D*U' factorization
*          as computed by ZSYTRF.
*
*  LDAFAC  (input) INTEGER
*          The leading dimension of the array AFAC.  LDAFAC >= max(1,N).
*
*  IPIV    (input) INTEGER array, dimension (N)
*          The pivot indices from ZSYTRF.
*
*  C       (workspace) COMPLEX*16 array, dimension (LDC,N)
*
*  LDC     (integer) INTEGER
*          The leading dimension of the array C.  LDC >= max(1,N).
*
*  RWORK   (workspace) DOUBLE PRECISION array, dimension (N)
*
*  RESID   (output) DOUBLE PRECISION
*          If UPLO = 'L', norm(L*D*L' - A) / ( N * norm(A) * EPS )
*          If UPLO = 'U', norm(U*D*U' - A) / ( N * norm(A) * EPS )
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO, ONE
      PARAMETER          ( ZERO = 0.0D+0, ONE = 1.0D+0 )
      COMPLEX*16         CZERO, CONE
      PARAMETER          ( CZERO = ( 0.0D+0, 0.0D+0 ),
     $                   CONE = ( 1.0D+0, 0.0D+0 ) )
*     ..
*     .. Local Scalars ..
      INTEGER            I, INFO, J
      DOUBLE PRECISION   ANORM, EPS
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      DOUBLE PRECISION   DLAMCH, ZLANSY
      EXTERNAL           LSAME, DLAMCH, ZLANSY
*     ..
*     .. External Subroutines ..
      EXTERNAL           ZLASET, ZLAVSY
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          DBLE
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
      EPS = DLAMCH( 'Epsilon' )
      ANORM = ZLANSY( '1', UPLO, N, A, LDA, RWORK )
*
*     Initialize C to the identity matrix.
*
      CALL ZLASET( 'Full', N, N, CZERO, CONE, C, LDC )
*
*     Call ZLAVSY to form the product D * U' (or D * L' ).
*
      CALL ZLAVSY( UPLO, 'Transpose', 'Non-unit', N, N, AFAC, LDAFAC,
     $             IPIV, C, LDC, INFO )
*
*     Call ZLAVSY again to multiply by U (or L ).
*
      CALL ZLAVSY( UPLO, 'No transpose', 'Unit', N, N, AFAC, LDAFAC,
     $             IPIV, C, LDC, INFO )
*
*     Compute the difference  C - A .
*
      IF( LSAME( UPLO, 'U' ) ) THEN
         DO 20 J = 1, N
            DO 10 I = 1, J
               C( I, J ) = C( I, J ) - A( I, J )
   10       CONTINUE
   20    CONTINUE
      ELSE
         DO 40 J = 1, N
            DO 30 I = J, N
               C( I, J ) = C( I, J ) - A( I, J )
   30       CONTINUE
   40    CONTINUE
      END IF
*
*     Compute norm( C - A ) / ( N * norm(A) * EPS )
*
      RESID = ZLANSY( '1', UPLO, N, C, LDC, RWORK )
*
      IF( ANORM.LE.ZERO ) THEN
         IF( RESID.NE.ZERO )
     $      RESID = ONE / EPS
      ELSE
         RESID = ( ( RESID / DBLE( N ) ) / ANORM ) / EPS
      END IF
*
      RETURN
*
*     End of ZSYT01
*
      END
