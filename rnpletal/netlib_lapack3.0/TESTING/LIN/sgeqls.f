      SUBROUTINE SGEQLS( M, N, NRHS, A, LDA, TAU, B, LDB, WORK, LWORK,
     $                   INFO )
*
*  -- LAPACK routine (version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     February 29, 1992
*
*     .. Scalar Arguments ..
      INTEGER            INFO, LDA, LDB, LWORK, M, N, NRHS
*     ..
*     .. Array Arguments ..
      REAL               A( LDA, * ), B( LDB, * ), TAU( * ),
     $                   WORK( LWORK )
*     ..
*
*  Purpose
*  =======
*
*  Solve the least squares problem
*      min || A*X - B ||
*  using the QL factorization
*      A = Q*L
*  computed by SGEQLF.
*
*  Arguments
*  =========
*
*  M       (input) INTEGER
*          The number of rows of the matrix A.  M >= 0.
*
*  N       (input) INTEGER
*          The number of columns of the matrix A.  M >= N >= 0.
*
*  NRHS    (input) INTEGER
*          The number of columns of B.  NRHS >= 0.
*
*  A       (input) REAL array, dimension (LDA,N)
*          Details of the QL factorization of the original matrix A as
*          returned by SGEQLF.
*
*  LDA     (input) INTEGER
*          The leading dimension of the array A.  LDA >= M.
*
*  TAU     (input) REAL array, dimension (N)
*          Details of the orthogonal matrix Q.
*
*  B       (input/output) REAL array, dimension (LDB,NRHS)
*          On entry, the m-by-nrhs right hand side matrix B.
*          On exit, the n-by-nrhs solution matrix X, stored in rows
*          m-n+1:m.
*
*  LDB     (input) INTEGER
*          The leading dimension of the array B. LDB >= M.
*
*  WORK    (workspace) REAL array, dimension (LWORK)
*
*  LWORK   (input) INTEGER
*          The length of the array WORK.  LWORK must be at least NRHS,
*          and should be at least NRHS*NB, where NB is the block size
*          for this environment.
*
*  INFO    (output) INTEGER
*          = 0: successful exit
*          < 0: if INFO = -i, the i-th argument had an illegal value
*
*  =====================================================================
*
*     .. Parameters ..
      REAL               ONE
      PARAMETER          ( ONE = 1.0E+0 )
*     ..
*     .. External Subroutines ..
      EXTERNAL           SORMQL, STRSM, XERBLA
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MAX
*     ..
*     .. Executable Statements ..
*
*     Test the input arguments.
*
      INFO = 0
      IF( M.LT.0 ) THEN
         INFO = -1
      ELSE IF( N.LT.0 .OR. N.GT.M ) THEN
         INFO = -2
      ELSE IF( NRHS.LT.0 ) THEN
         INFO = -3
      ELSE IF( LDA.LT.MAX( 1, M ) ) THEN
         INFO = -5
      ELSE IF( LDB.LT.MAX( 1, M ) ) THEN
         INFO = -8
      ELSE IF( LWORK.LT.1 .OR. LWORK.LT.NRHS .AND. M.GT.0 .AND. N.GT.0 )
     $          THEN
         INFO = -10
      END IF
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'SGEQLS', -INFO )
         RETURN
      END IF
*
*     Quick return if possible
*
      IF( N.EQ.0 .OR. NRHS.EQ.0 .OR. M.EQ.0 )
     $   RETURN
*
*     B := Q' * B
*
      CALL SORMQL( 'Left', 'Transpose', M, NRHS, N, A, LDA, TAU, B, LDB,
     $             WORK, LWORK, INFO )
*
*     Solve L*X = B(m-n+1:m,:)
*
      CALL STRSM( 'Left', 'Lower', 'No transpose', 'Non-unit', N, NRHS,
     $            ONE, A( M-N+1, 1 ), LDA, B( M-N+1, 1 ), LDB )
*
      RETURN
*
*     End of SGEQLS
*
      END
