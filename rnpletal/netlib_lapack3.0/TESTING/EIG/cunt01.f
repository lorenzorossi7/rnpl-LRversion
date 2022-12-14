      SUBROUTINE CUNT01( ROWCOL, M, N, U, LDU, WORK, LWORK, RWORK,
     $                   RESID )
*
*  -- LAPACK test routine (version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     June 30, 1999
*
*     .. Scalar Arguments ..
      CHARACTER          ROWCOL
      INTEGER            LDU, LWORK, M, N
      REAL               RESID
*     ..
*     .. Array Arguments ..
      REAL               RWORK( * )
      COMPLEX            U( LDU, * ), WORK( * )
*     ..
*
*  Purpose
*  =======
*
*  CUNT01 checks that the matrix U is unitary by computing the ratio
*
*     RESID = norm( I - U*U' ) / ( n * EPS ), if ROWCOL = 'R',
*  or
*     RESID = norm( I - U'*U ) / ( m * EPS ), if ROWCOL = 'C'.
*
*  Alternatively, if there isn't sufficient workspace to form
*  I - U*U' or I - U'*U, the ratio is computed as
*
*     RESID = abs( I - U*U' ) / ( n * EPS ), if ROWCOL = 'R',
*  or
*     RESID = abs( I - U'*U ) / ( m * EPS ), if ROWCOL = 'C'.
*
*  where EPS is the machine precision.  ROWCOL is used only if m = n;
*  if m > n, ROWCOL is assumed to be 'C', and if m < n, ROWCOL is
*  assumed to be 'R'.
*
*  Arguments
*  =========
*
*  ROWCOL  (input) CHARACTER
*          Specifies whether the rows or columns of U should be checked
*          for orthogonality.  Used only if M = N.
*          = 'R':  Check for orthogonal rows of U
*          = 'C':  Check for orthogonal columns of U
*
*  M       (input) INTEGER
*          The number of rows of the matrix U.
*
*  N       (input) INTEGER
*          The number of columns of the matrix U.
*
*  U       (input) COMPLEX array, dimension (LDU,N)
*          The unitary matrix U.  U is checked for orthogonal columns
*          if m > n or if m = n and ROWCOL = 'C'.  U is checked for
*          orthogonal rows if m < n or if m = n and ROWCOL = 'R'.
*
*  LDU     (input) INTEGER
*          The leading dimension of the array U.  LDU >= max(1,M).
*
*  WORK    (workspace) COMPLEX array, dimension (LWORK)
*
*  LWORK   (input) INTEGER
*          The length of the array WORK.  For best performance, LWORK
*          should be at least N*N if ROWCOL = 'C' or M*M if
*          ROWCOL = 'R', but the test will be done even if LWORK is 0.
*
*  RWORK   (workspace) REAL array, dimension (min(M,N))
*          Used only if LWORK is large enough to use the Level 3 BLAS
*          code.
*
*  RESID   (output) REAL
*          RESID = norm( I - U * U' ) / ( n * EPS ), if ROWCOL = 'R', or
*          RESID = norm( I - U' * U ) / ( m * EPS ), if ROWCOL = 'C'.
*
*  =====================================================================
*
*     .. Parameters ..
      REAL               ZERO, ONE
      PARAMETER          ( ZERO = 0.0E+0, ONE = 1.0E+0 )
*     ..
*     .. Local Scalars ..
      CHARACTER          TRANSU
      INTEGER            I, J, K, LDWORK, MNMIN
      REAL               EPS
      COMPLEX            TMP, ZDUM
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      REAL               CLANSY, SLAMCH
      COMPLEX            CDOTC
      EXTERNAL           LSAME, CLANSY, SLAMCH, CDOTC
*     ..
*     .. External Subroutines ..
      EXTERNAL           CHERK, CLASET
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, AIMAG, CMPLX, MAX, MIN, REAL
*     ..
*     .. Statement Functions ..
      REAL               CABS1
*     ..
*     .. Statement Function definitions ..
      CABS1( ZDUM ) = ABS( REAL( ZDUM ) ) + ABS( AIMAG( ZDUM ) )
*     ..
*     .. Executable Statements ..
*
      RESID = ZERO
*
*     Quick return if possible
*
      IF( M.LE.0 .OR. N.LE.0 )
     $   RETURN
*
      EPS = SLAMCH( 'Precision' )
      IF( M.LT.N .OR. ( M.EQ.N .AND. LSAME( ROWCOL, 'R' ) ) ) THEN
         TRANSU = 'N'
         K = N
      ELSE
         TRANSU = 'C'
         K = M
      END IF
      MNMIN = MIN( M, N )
*
      IF( ( MNMIN+1 )*MNMIN.LE.LWORK ) THEN
         LDWORK = MNMIN
      ELSE
         LDWORK = 0
      END IF
      IF( LDWORK.GT.0 ) THEN
*
*        Compute I - U*U' or I - U'*U.
*
         CALL CLASET( 'Upper', MNMIN, MNMIN, CMPLX( ZERO ),
     $                CMPLX( ONE ), WORK, LDWORK )
         CALL CHERK( 'Upper', TRANSU, MNMIN, K, -ONE, U, LDU, ONE, WORK,
     $               LDWORK )
*
*        Compute norm( I - U*U' ) / ( K * EPS ) .
*
         RESID = CLANSY( '1', 'Upper', MNMIN, WORK, LDWORK, RWORK )
         RESID = ( RESID / REAL( K ) ) / EPS
      ELSE IF( TRANSU.EQ.'C' ) THEN
*
*        Find the maximum element in abs( I - U'*U ) / ( m * EPS )
*
         DO 20 J = 1, N
            DO 10 I = 1, J
               IF( I.NE.J ) THEN
                  TMP = ZERO
               ELSE
                  TMP = ONE
               END IF
               TMP = TMP - CDOTC( M, U( 1, I ), 1, U( 1, J ), 1 )
               RESID = MAX( RESID, CABS1( TMP ) )
   10       CONTINUE
   20    CONTINUE
         RESID = ( RESID / REAL( M ) ) / EPS
      ELSE
*
*        Find the maximum element in abs( I - U*U' ) / ( n * EPS )
*
         DO 40 J = 1, M
            DO 30 I = 1, J
               IF( I.NE.J ) THEN
                  TMP = ZERO
               ELSE
                  TMP = ONE
               END IF
               TMP = TMP - CDOTC( N, U( J, 1 ), LDU, U( I, 1 ), LDU )
               RESID = MAX( RESID, CABS1( TMP ) )
   30       CONTINUE
   40    CONTINUE
         RESID = ( RESID / REAL( N ) ) / EPS
      END IF
      RETURN
*
*     End of CUNT01
*
      END
