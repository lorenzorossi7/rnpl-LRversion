      SUBROUTINE DCKGQR( NM, MVAL, NP, PVAL, NN, NVAL, NMATS, ISEED,
     $                   THRESH, NMAX, A, AF, AQ, AR, TAUA, B, BF, BZ,
     $                   BT, BWK, TAUB, WORK, RWORK, NIN, NOUT, INFO )
*
*  -- LAPACK test routine (version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     March 31, 1993
*
*     .. Scalar Arguments ..
      INTEGER            INFO, NIN, NM, NMATS, NMAX, NN, NOUT, NP
      DOUBLE PRECISION   THRESH
*     ..
*     .. Array Arguments ..
      INTEGER            ISEED( 4 ), MVAL( * ), NVAL( * ), PVAL( * )
      DOUBLE PRECISION   A( * ), AF( * ), AQ( * ), AR( * ), B( * ),
     $                   BF( * ), BT( * ), BWK( * ), BZ( * ),
     $                   RWORK( * ), TAUA( * ), TAUB( * ), WORK( * )
*     ..
*
*  Purpose
*  =======
*
*  DCKGQR tests
*  DGGQRF: GQR factorization for N-by-M matrix A and N-by-P matrix B,
*  DGGRQF: GRQ factorization for M-by-N matrix A and P-by-N matrix B.
*
*  Arguments
*  =========
*
*  NM      (input) INTEGER
*          The number of values of M contained in the vector MVAL.
*
*  MVAL    (input) INTEGER array, dimension (NM)
*          The values of the matrix row(column) dimension M.
*
*  NP      (input) INTEGER
*          The number of values of P contained in the vector PVAL.
*
*  PVAL    (input) INTEGER array, dimension (NP)
*          The values of the matrix row(column) dimension P.
*
*  NN      (input) INTEGER
*          The number of values of N contained in the vector NVAL.
*
*  NVAL    (input) INTEGER array, dimension (NN)
*          The values of the matrix column(row) dimension N.
*
*  NMATS   (input) INTEGER
*          The number of matrix types to be tested for each combination
*          of matrix dimensions.  If NMATS >= NTYPES (the maximum
*          number of matrix types), then all the different types are
*          generated for testing.  If NMATS < NTYPES, another input line
*          is read to get the numbers of the matrix types to be used.
*
*  ISEED   (input/output) INTEGER array, dimension (4)
*          On entry, the seed of the random number generator.  The array
*          elements should be between 0 and 4095, otherwise they will be
*          reduced mod 4096, and ISEED(4) must be odd.
*          On exit, the next seed in the random number sequence after
*          all the test matrices have been generated.
*
*  THRESH  (input) DOUBLE PRECISION
*          The threshold value for the test ratios.  A result is
*          included in the output file if RESULT >= THRESH.  To have
*          every test ratio printed, use THRESH = 0.
*
*  NMAX    (input) INTEGER
*          The maximum value permitted for M or N, used in dimensioning
*          the work arrays.
*
*  A       (workspace) DOUBLE PRECISION array, dimension (NMAX*NMAX)
*
*  AF      (workspace) DOUBLE PRECISION array, dimension (NMAX*NMAX)
*
*  AQ      (workspace) DOUBLE PRECISION array, dimension (NMAX*NMAX)
*
*  AR      (workspace) DOUBLE PRECISION array, dimension (NMAX*NMAX)
*
*  TAUA    (workspace) DOUBLE PRECISION array, dimension (NMAX)
*
*  B       (workspace) DOUBLE PRECISION array, dimension (NMAX*NMAX)
*
*  BF      (workspace) DOUBLE PRECISION array, dimension (NMAX*NMAX)
*
*  BZ      (workspace) DOUBLE PRECISION array, dimension (NMAX*NMAX)
*
*  BT      (workspace) DOUBLE PRECISION array, dimension (NMAX*NMAX)
*
*  BWK     (workspace) DOUBLE PRECISION array, dimension (NMAX*NMAX)
*
*  TAUB    (workspace) DOUBLE PRECISION array, dimension (NMAX)
*
*  WORK    (workspace) DOUBLE PRECISION array, dimension (NMAX*NMAX)
*
*  RWORK   (workspace) DOUBLE PRECISION array, dimension (NMAX)
*
*  NIN     (input) INTEGER
*          The unit number for input.
*
*  NOUT    (input) INTEGER
*          The unit number for output.
*
*  INFO    (output) INTEGER
*          = 0 :  successful exit
*          > 0 :  If DLATMS returns an error code, the absolute value
*                 of it is returned.
*
*  =====================================================================
*
*     .. Parameters ..
      INTEGER            NTESTS
      PARAMETER          ( NTESTS = 7 )
      INTEGER            NTYPES
      PARAMETER          ( NTYPES = 8 )
*     ..
*     .. Local Scalars ..
      LOGICAL            FIRSTT
      CHARACTER          DISTA, DISTB, TYPE
      CHARACTER*3        PATH
      INTEGER            I, IINFO, IM, IMAT, IN, IP, KLA, KLB, KUA, KUB,
     $                   LDA, LDB, LWORK, M, MODEA, MODEB, N, NFAIL,
     $                   NRUN, NT, P
      DOUBLE PRECISION   ANORM, BNORM, CNDNMA, CNDNMB
*     ..
*     .. Local Arrays ..
      LOGICAL            DOTYPE( NTYPES )
      DOUBLE PRECISION   RESULT( NTESTS )
*     ..
*     .. External Subroutines ..
      EXTERNAL           ALAHDG, ALAREQ, ALASUM, DGQRTS, DGRQTS, DLATB9,
     $                   DLATMS
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS
*     ..
*     .. Executable Statements ..
*
*     Initialize constants.
*
      PATH( 1: 3 ) = 'GQR'
      INFO = 0
      NRUN = 0
      NFAIL = 0
      FIRSTT = .TRUE.
      CALL ALAREQ( PATH, NMATS, DOTYPE, NTYPES, NIN, NOUT )
      LDA = NMAX
      LDB = NMAX
      LWORK = NMAX*NMAX
*
*     Do for each value of M in MVAL.
*
      DO 60 IM = 1, NM
         M = MVAL( IM )
*
*        Do for each value of P in PVAL.
*
         DO 50 IP = 1, NP
            P = PVAL( IP )
*
*           Do for each value of N in NVAL.
*
            DO 40 IN = 1, NN
               N = NVAL( IN )
*
               DO 30 IMAT = 1, NTYPES
*
*                 Do the tests only if DOTYPE( IMAT ) is true.
*
                  IF( .NOT.DOTYPE( IMAT ) )
     $               GO TO 30
*
*                 Test DGGRQF
*
*                 Set up parameters with DLATB9 and generate test
*                 matrices A and B with DLATMS.
*
                  CALL DLATB9( 'GRQ', IMAT, M, P, N, TYPE, KLA, KUA,
     $                         KLB, KUB, ANORM, BNORM, MODEA, MODEB,
     $                         CNDNMA, CNDNMB, DISTA, DISTB )
*
*                 Generate M by N matrix A
*
                  CALL DLATMS( M, N, DISTA, ISEED, TYPE, RWORK, MODEA,
     $                         CNDNMA, ANORM, KLA, KUA, 'No packing', A,
     $                         LDA, WORK, IINFO )
                  IF( IINFO.NE.0 ) THEN
                     WRITE( NOUT, FMT = 9999 )IINFO
                     INFO = ABS( IINFO )
                     GO TO 30
                  END IF
*
*                 Generate P by N matrix B
*
                  CALL DLATMS( P, N, DISTB, ISEED, TYPE, RWORK, MODEB,
     $                         CNDNMB, BNORM, KLB, KUB, 'No packing', B,
     $                         LDB, WORK, IINFO )
                  IF( IINFO.NE.0 ) THEN
                     WRITE( NOUT, FMT = 9999 )IINFO
                     INFO = ABS( IINFO )
                     GO TO 30
                  END IF
*
                  NT = 4
*
                  CALL DGRQTS( M, P, N, A, AF, AQ, AR, LDA, TAUA, B, BF,
     $                         BZ, BT, BWK, LDB, TAUB, WORK, LWORK,
     $                         RWORK, RESULT )
*
*                 Print information about the tests that did not
*                 pass the threshold.
*
                  DO 10 I = 1, NT
                     IF( RESULT( I ).GE.THRESH ) THEN
                        IF( NFAIL.EQ.0 .AND. FIRSTT ) THEN
                           FIRSTT = .FALSE.
                           CALL ALAHDG( NOUT, 'GRQ' )
                        END IF
                        WRITE( NOUT, FMT = 9998 )M, P, N, IMAT, I,
     $                     RESULT( I )
                        NFAIL = NFAIL + 1
                     END IF
   10             CONTINUE
                  NRUN = NRUN + NT
*
*                 Test DGGQRF
*
*                 Set up parameters with DLATB9 and generate test
*                 matrices A and B with DLATMS.
*
                  CALL DLATB9( 'GQR', IMAT, M, P, N, TYPE, KLA, KUA,
     $                         KLB, KUB, ANORM, BNORM, MODEA, MODEB,
     $                         CNDNMA, CNDNMB, DISTA, DISTB )
*
*                 Generate N-by-M matrix  A
*
                  CALL DLATMS( N, M, DISTA, ISEED, TYPE, RWORK, MODEA,
     $                         CNDNMA, ANORM, KLA, KUA, 'No packing', A,
     $                         LDA, WORK, IINFO )
                  IF( IINFO.NE.0 ) THEN
                     WRITE( NOUT, FMT = 9999 )IINFO
                     INFO = ABS( IINFO )
                     GO TO 30
                  END IF
*
*                 Generate N-by-P matrix  B
*
                  CALL DLATMS( N, P, DISTB, ISEED, TYPE, RWORK, MODEA,
     $                         CNDNMA, BNORM, KLB, KUB, 'No packing', B,
     $                         LDB, WORK, IINFO )
                  IF( IINFO.NE.0 ) THEN
                     WRITE( NOUT, FMT = 9999 )IINFO
                     INFO = ABS( IINFO )
                     GO TO 30
                  END IF
*
                  NT = 4
*
                  CALL DGQRTS( N, M, P, A, AF, AQ, AR, LDA, TAUA, B, BF,
     $                         BZ, BT, BWK, LDB, TAUB, WORK, LWORK,
     $                         RWORK, RESULT )
*
*                 Print information about the tests that did not
*                 pass the threshold.
*
                  DO 20 I = 1, NT
                     IF( RESULT( I ).GE.THRESH ) THEN
                        IF( NFAIL.EQ.0 .AND. FIRSTT ) THEN
                           FIRSTT = .FALSE.
                           CALL ALAHDG( NOUT, PATH )
                        END IF
                        WRITE( NOUT, FMT = 9997 )N, M, P, IMAT, I,
     $                     RESULT( I )
                        NFAIL = NFAIL + 1
                     END IF
   20             CONTINUE
                  NRUN = NRUN + NT
*
   30          CONTINUE
   40       CONTINUE
   50    CONTINUE
   60 CONTINUE
*
*     Print a summary of the results.
*
      CALL ALASUM( PATH, NOUT, NFAIL, NRUN, 0 )
*
 9999 FORMAT( ' DLATMS in DCKGQR:    INFO = ', I5 )
 9998 FORMAT( ' M=', I4, ' P=', I4, ', N=', I4, ', type ', I2,
     $      ', test ', I2, ', ratio=', G13.6 )
 9997 FORMAT( ' N=', I4, ' M=', I4, ', P=', I4, ', type ', I2,
     $      ', test ', I2, ', ratio=', G13.6 )
      RETURN
*
*     End of DCKGQR
*
      END
