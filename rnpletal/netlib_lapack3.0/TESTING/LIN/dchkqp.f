      SUBROUTINE DCHKQP( DOTYPE, NM, MVAL, NN, NVAL, THRESH, TSTERR, A,
     $                   COPYA, S, COPYS, TAU, WORK, IWORK, NOUT )
*
*  -- LAPACK test routine (version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     September 30, 1994
*
*     .. Scalar Arguments ..
      LOGICAL            TSTERR
      INTEGER            NM, NN, NOUT
      DOUBLE PRECISION   THRESH
*     ..
*     .. Array Arguments ..
      LOGICAL            DOTYPE( * )
      INTEGER            IWORK( * ), MVAL( * ), NVAL( * )
      DOUBLE PRECISION   A( * ), COPYA( * ), COPYS( * ), S( * ),
     $                   TAU( * ), WORK( * )
*     ..
*
*  Purpose
*  =======
*
*  DCHKQP tests DGEQPF.
*
*  Arguments
*  =========
*
*  DOTYPE  (input) LOGICAL array, dimension (NTYPES)
*          The matrix types to be used for testing.  Matrices of type j
*          (for 1 <= j <= NTYPES) are used for testing if DOTYPE(j) =
*          .TRUE.; if DOTYPE(j) = .FALSE., then type j is not used.
*
*  NM      (input) INTEGER
*          The number of values of M contained in the vector MVAL.
*
*  MVAL    (input) INTEGER array, dimension (NM)
*          The values of the matrix row dimension M.
*
*  NN      (input) INTEGER
*          The number of values of N contained in the vector NVAL.
*
*  NVAL    (input) INTEGER array, dimension (NN)
*          The values of the matrix column dimension N.
*
*  THRESH  (input) DOUBLE PRECISION
*          The threshold value for the test ratios.  A result is
*          included in the output file if RESULT >= THRESH.  To have
*          every test ratio printed, use THRESH = 0.
*
*  TSTERR  (input) LOGICAL
*          Flag that indicates whether error exits are to be tested.
*
*  A       (workspace) DOUBLE PRECISION array, dimension (MMAX*NMAX)
*          where MMAX is the maximum value of M in MVAL and NMAX is the
*          maximum value of N in NVAL.
*
*  COPYA   (workspace) DOUBLE PRECISION array, dimension (MMAX*NMAX)
*
*  S       (workspace) DOUBLE PRECISION array, dimension
*                      (min(MMAX,NMAX))
*
*  COPYS   (workspace) DOUBLE PRECISION array, dimension
*                      (min(MMAX,NMAX))
*
*  TAU     (workspace) DOUBLE PRECISION array, dimension (MMAX)
*
*  WORK    (workspace) DOUBLE PRECISION array, dimension
*                      (MMAX*NMAX + 4*NMAX + MMAX)
*
*  IWORK   (workspace) INTEGER array, dimension (NMAX)
*
*  NOUT    (input) INTEGER
*          The unit number for output.
*
*  =====================================================================
*
*     .. Parameters ..
      INTEGER            NTYPES
      PARAMETER          ( NTYPES = 6 )
      INTEGER            NTESTS
      PARAMETER          ( NTESTS = 3 )
      DOUBLE PRECISION   ONE, ZERO
      PARAMETER          ( ONE = 1.0D0, ZERO = 0.0D0 )
*     ..
*     .. Local Scalars ..
      CHARACTER*3        PATH
      INTEGER            I, IHIGH, ILOW, IM, IMODE, IN, INFO, ISTEP, K,
     $                   LDA, LWORK, M, MNMIN, MODE, N, NERRS, NFAIL,
     $                   NRUN
      DOUBLE PRECISION   EPS
*     ..
*     .. Local Arrays ..
      INTEGER            ISEED( 4 ), ISEEDY( 4 )
      DOUBLE PRECISION   RESULT( NTESTS )
*     ..
*     .. External Functions ..
      DOUBLE PRECISION   DLAMCH, DQPT01, DQRT11, DQRT12
      EXTERNAL           DLAMCH, DQPT01, DQRT11, DQRT12
*     ..
*     .. External Subroutines ..
      EXTERNAL           ALAHD, ALASUM, DERRQP, DGEQPF, DLACPY, DLAORD,
     $                   DLASET, DLATMS
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MAX, MIN
*     ..
*     .. Scalars in Common ..
      LOGICAL            LERR, OK
      CHARACTER*6        SRNAMT
      INTEGER            INFOT, IOUNIT
*     ..
*     .. Common blocks ..
      COMMON             / INFOC / INFOT, IOUNIT, OK, LERR
      COMMON             / SRNAMC / SRNAMT
*     ..
*     .. Data statements ..
      DATA               ISEEDY / 1988, 1989, 1990, 1991 /
*     ..
*     .. Executable Statements ..
*
*     Initialize constants and the random number seed.
*
      PATH( 1: 1 ) = 'Double precision'
      PATH( 2: 3 ) = 'QP'
      NRUN = 0
      NFAIL = 0
      NERRS = 0
      DO 10 I = 1, 4
         ISEED( I ) = ISEEDY( I )
   10 CONTINUE
      EPS = DLAMCH( 'Epsilon' )
*
*     Test the error exits
*
      IF( TSTERR )
     $   CALL DERRQP( PATH, NOUT )
      INFOT = 0
*
      DO 80 IM = 1, NM
*
*        Do for each value of M in MVAL.
*
         M = MVAL( IM )
         LDA = MAX( 1, M )
*
         DO 70 IN = 1, NN
*
*           Do for each value of N in NVAL.
*
            N = NVAL( IN )
            MNMIN = MIN( M, N )
            LWORK = MAX( 1, M*MAX( M, N )+4*MNMIN+MAX( M, N ) )
*
            DO 60 IMODE = 1, NTYPES
               IF( .NOT.DOTYPE( IMODE ) )
     $            GO TO 60
*
*              Do for each type of matrix
*                 1:  zero matrix
*                 2:  one small singular value
*                 3:  geometric distribution of singular values
*                 4:  first n/2 columns fixed
*                 5:  last n/2 columns fixed
*                 6:  every second column fixed
*
               MODE = IMODE
               IF( IMODE.GT.3 )
     $            MODE = 1
*
*              Generate test matrix of size m by n using
*              singular value distribution indicated by `mode'.
*
               DO 20 I = 1, N
                  IWORK( I ) = 0
   20          CONTINUE
               IF( IMODE.EQ.1 ) THEN
                  CALL DLASET( 'Full', M, N, ZERO, ZERO, COPYA, LDA )
                  DO 30 I = 1, MNMIN
                     COPYS( I ) = ZERO
   30             CONTINUE
               ELSE
                  CALL DLATMS( M, N, 'Uniform', ISEED, 'Nonsymm', COPYS,
     $                         MODE, ONE / EPS, ONE, M, N, 'No packing',
     $                         COPYA, LDA, WORK, INFO )
                  IF( IMODE.GE.4 ) THEN
                     IF( IMODE.EQ.4 ) THEN
                        ILOW = 1
                        ISTEP = 1
                        IHIGH = MAX( 1, N / 2 )
                     ELSE IF( IMODE.EQ.5 ) THEN
                        ILOW = MAX( 1, N / 2 )
                        ISTEP = 1
                        IHIGH = N
                     ELSE IF( IMODE.EQ.6 ) THEN
                        ILOW = 1
                        ISTEP = 2
                        IHIGH = N
                     END IF
                     DO 40 I = ILOW, IHIGH, ISTEP
                        IWORK( I ) = 1
   40                CONTINUE
                  END IF
                  CALL DLAORD( 'Decreasing', MNMIN, COPYS, 1 )
               END IF
*
*              Save A and its singular values
*
               CALL DLACPY( 'All', M, N, COPYA, LDA, A, LDA )
*
*              Compute the QR factorization with pivoting of A
*
               SRNAMT = 'DGEQPF'
               CALL DGEQPF( M, N, A, LDA, IWORK, TAU, WORK, INFO )
*
*              Compute norm(svd(a) - svd(r))
*
               RESULT( 1 ) = DQRT12( M, N, A, LDA, COPYS, WORK, LWORK )
*
*              Compute norm( A*P - Q*R )
*
               RESULT( 2 ) = DQPT01( M, N, MNMIN, COPYA, A, LDA, TAU,
     $                       IWORK, WORK, LWORK )
*
*              Compute Q'*Q
*
               RESULT( 3 ) = DQRT11( M, MNMIN, A, LDA, TAU, WORK,
     $                       LWORK )
*
*              Print information about the tests that did not pass
*              the threshold.
*
               DO 50 K = 1, 3
                  IF( RESULT( K ).GE.THRESH ) THEN
                     IF( NFAIL.EQ.0 .AND. NERRS.EQ.0 )
     $                  CALL ALAHD( NOUT, PATH )
                     WRITE( NOUT, FMT = 9999 )M, N, IMODE, K,
     $                  RESULT( K )
                     NFAIL = NFAIL + 1
                  END IF
   50          CONTINUE
               NRUN = NRUN + 3
   60       CONTINUE
   70    CONTINUE
   80 CONTINUE
*
*     Print a summary of the results.
*
      CALL ALASUM( PATH, NOUT, NFAIL, NRUN, NERRS )
*
 9999 FORMAT( ' M =', I5, ', N =', I5, ', type ', I2, ', test ', I2,
     $      ', ratio =', G12.5 )
*
*     End of DCHKQP
*
      END
