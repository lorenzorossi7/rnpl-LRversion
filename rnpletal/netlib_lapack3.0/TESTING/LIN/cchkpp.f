      SUBROUTINE CCHKPP( DOTYPE, NN, NVAL, NNS, NSVAL, THRESH, TSTERR,
     $                   NMAX, A, AFAC, AINV, B, X, XACT, WORK, RWORK,
     $                   NOUT )
*
*  -- LAPACK test routine (version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     December 7, 1999
*
*     .. Scalar Arguments ..
      LOGICAL            TSTERR
      INTEGER            NMAX, NN, NNS, NOUT
      REAL               THRESH
*     ..
*     .. Array Arguments ..
      LOGICAL            DOTYPE( * )
      INTEGER            NSVAL( * ), NVAL( * )
      REAL               RWORK( * )
      COMPLEX            A( * ), AFAC( * ), AINV( * ), B( * ),
     $                   WORK( * ), X( * ), XACT( * )
*     ..
*
*  Purpose
*  =======
*
*  CCHKPP tests CPPTRF, -TRI, -TRS, -RFS, and -CON
*
*  Arguments
*  =========
*
*  DOTYPE  (input) LOGICAL array, dimension (NTYPES)
*          The matrix types to be used for testing.  Matrices of type j
*          (for 1 <= j <= NTYPES) are used for testing if DOTYPE(j) =
*          .TRUE.; if DOTYPE(j) = .FALSE., then type j is not used.
*
*  NN      (input) INTEGER
*          The number of values of N contained in the vector NVAL.
*
*  NVAL    (input) INTEGER array, dimension (NN)
*          The values of the matrix dimension N.
*
*  NNS     (input) INTEGER
*          The number of values of NRHS contained in the vector NSVAL.
*
*  NSVAL   (input) INTEGER array, dimension (NNS)
*          The values of the number of right hand sides NRHS.
*
*  THRESH  (input) REAL
*          The threshold value for the test ratios.  A result is
*          included in the output file if RESULT >= THRESH.  To have
*          every test ratio printed, use THRESH = 0.
*
*  TSTERR  (input) LOGICAL
*          Flag that indicates whether error exits are to be tested.
*
*  NMAX    (input) INTEGER
*          The maximum value permitted for N, used in dimensioning the
*          work arrays.
*
*  A       (workspace) COMPLEX array, dimension
*                      (NMAX*(NMAX+1)/2)
*
*  AFAC    (workspace) COMPLEX array, dimension
*                      (NMAX*(NMAX+1)/2)
*
*  AINV    (workspace) COMPLEX array, dimension
*                      (NMAX*(NMAX+1)/2)
*
*  B       (workspace) COMPLEX array, dimension (NMAX*NSMAX)
*          where NSMAX is the largest entry in NSVAL.
*
*  X       (workspace) COMPLEX array, dimension (NMAX*NSMAX)
*
*  XACT    (workspace) COMPLEX array, dimension (NMAX*NSMAX)
*
*  WORK    (workspace) COMPLEX array, dimension
*                      (NMAX*max(3,NSMAX))
*
*  RWORK   (workspace) REAL array, dimension
*                      (max(NMAX,2*NSMAX))
*
*  NOUT    (input) INTEGER
*          The unit number for output.
*
*  =====================================================================
*
*     .. Parameters ..
      REAL               ZERO
      PARAMETER          ( ZERO = 0.0E+0 )
      INTEGER            NTYPES
      PARAMETER          ( NTYPES = 9 )
      INTEGER            NTESTS
      PARAMETER          ( NTESTS = 8 )
*     ..
*     .. Local Scalars ..
      LOGICAL            ZEROT
      CHARACTER          DIST, PACKIT, TYPE, UPLO, XTYPE
      CHARACTER*3        PATH
      INTEGER            I, IMAT, IN, INFO, IOFF, IRHS, IUPLO, IZERO, K,
     $                   KL, KU, LDA, MODE, N, NERRS, NFAIL, NIMAT, NPP,
     $                   NRHS, NRUN
      REAL               ANORM, CNDNUM, RCOND, RCONDC
*     ..
*     .. Local Arrays ..
      CHARACTER          PACKS( 2 ), UPLOS( 2 )
      INTEGER            ISEED( 4 ), ISEEDY( 4 )
      REAL               RESULT( NTESTS )
*     ..
*     .. External Functions ..
      REAL               CLANHP, SGET06
      EXTERNAL           CLANHP, SGET06
*     ..
*     .. External Subroutines ..
      EXTERNAL           ALAERH, ALAHD, ALASUM, CCOPY, CERRPO, CGET04,
     $                   CLACPY, CLAIPD, CLARHS, CLATB4, CLATMS, CPPCON,
     $                   CPPRFS, CPPT01, CPPT02, CPPT03, CPPT05, CPPTRF,
     $                   CPPTRI, CPPTRS
*     ..
*     .. Scalars in Common ..
      LOGICAL            LERR, OK
      CHARACTER*6        SRNAMT
      INTEGER            INFOT, NUNIT
*     ..
*     .. Common blocks ..
      COMMON             / INFOC / INFOT, NUNIT, OK, LERR
      COMMON             / SRNAMC / SRNAMT
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MAX
*     ..
*     .. Data statements ..
      DATA               ISEEDY / 1988, 1989, 1990, 1991 /
      DATA               UPLOS / 'U', 'L' / , PACKS / 'C', 'R' /
*     ..
*     .. Executable Statements ..
*
*     Initialize constants and the random number seed.
*
      PATH( 1: 1 ) = 'Complex precision'
      PATH( 2: 3 ) = 'PP'
      NRUN = 0
      NFAIL = 0
      NERRS = 0
      DO 10 I = 1, 4
         ISEED( I ) = ISEEDY( I )
   10 CONTINUE
*
*     Test the error exits
*
      IF( TSTERR )
     $   CALL CERRPO( PATH, NOUT )
      INFOT = 0
*
*     Do for each value of N in NVAL
*
      DO 110 IN = 1, NN
         N = NVAL( IN )
         LDA = MAX( N, 1 )
         XTYPE = 'N'
         NIMAT = NTYPES
         IF( N.LE.0 )
     $      NIMAT = 1
*
         DO 100 IMAT = 1, NIMAT
*
*           Do the tests only if DOTYPE( IMAT ) is true.
*
            IF( .NOT.DOTYPE( IMAT ) )
     $         GO TO 100
*
*           Skip types 3, 4, or 5 if the matrix size is too small.
*
            ZEROT = IMAT.GE.3 .AND. IMAT.LE.5
            IF( ZEROT .AND. N.LT.IMAT-2 )
     $         GO TO 100
*
*           Do first for UPLO = 'U', then for UPLO = 'L'
*
            DO 90 IUPLO = 1, 2
               UPLO = UPLOS( IUPLO )
               PACKIT = PACKS( IUPLO )
*
*              Set up parameters with CLATB4 and generate a test matrix
*              with CLATMS.
*
               CALL CLATB4( PATH, IMAT, N, N, TYPE, KL, KU, ANORM, MODE,
     $                      CNDNUM, DIST )
*
               SRNAMT = 'CLATMS'
               CALL CLATMS( N, N, DIST, ISEED, TYPE, RWORK, MODE,
     $                      CNDNUM, ANORM, KL, KU, PACKIT, A, LDA, WORK,
     $                      INFO )
*
*              Check error code from CLATMS.
*
               IF( INFO.NE.0 ) THEN
                  CALL ALAERH( PATH, 'CLATMS', INFO, 0, UPLO, N, N, -1,
     $                         -1, -1, IMAT, NFAIL, NERRS, NOUT )
                  GO TO 90
               END IF
*
*              For types 3-5, zero one row and column of the matrix to
*              test that INFO is returned correctly.
*
               IF( ZEROT ) THEN
                  IF( IMAT.EQ.3 ) THEN
                     IZERO = 1
                  ELSE IF( IMAT.EQ.4 ) THEN
                     IZERO = N
                  ELSE
                     IZERO = N / 2 + 1
                  END IF
*
*                 Set row and column IZERO of A to 0.
*
                  IF( IUPLO.EQ.1 ) THEN
                     IOFF = ( IZERO-1 )*IZERO / 2
                     DO 20 I = 1, IZERO - 1
                        A( IOFF+I ) = ZERO
   20                CONTINUE
                     IOFF = IOFF + IZERO
                     DO 30 I = IZERO, N
                        A( IOFF ) = ZERO
                        IOFF = IOFF + I
   30                CONTINUE
                  ELSE
                     IOFF = IZERO
                     DO 40 I = 1, IZERO - 1
                        A( IOFF ) = ZERO
                        IOFF = IOFF + N - I
   40                CONTINUE
                     IOFF = IOFF - IZERO
                     DO 50 I = IZERO, N
                        A( IOFF+I ) = ZERO
   50                CONTINUE
                  END IF
               ELSE
                  IZERO = 0
               END IF
*
*              Set the imaginary part of the diagonals.
*
               IF( IUPLO.EQ.1 ) THEN
                  CALL CLAIPD( N, A, 2, 1 )
               ELSE
                  CALL CLAIPD( N, A, N, -1 )
               END IF
*
*              Compute the L*L' or U'*U factorization of the matrix.
*
               NPP = N*( N+1 ) / 2
               CALL CCOPY( NPP, A, 1, AFAC, 1 )
               SRNAMT = 'CPPTRF'
               CALL CPPTRF( UPLO, N, AFAC, INFO )
*
*              Check error code from CPPTRF.
*
               IF( INFO.NE.IZERO ) THEN
                  CALL ALAERH( PATH, 'CPPTRF', INFO, IZERO, UPLO, N, N,
     $                         -1, -1, -1, IMAT, NFAIL, NERRS, NOUT )
                  GO TO 90
               END IF
*
*              Skip the tests if INFO is not 0.
*
               IF( INFO.NE.0 )
     $            GO TO 90
*
*+    TEST 1
*              Reconstruct matrix from factors and compute residual.
*
               CALL CCOPY( NPP, AFAC, 1, AINV, 1 )
               CALL CPPT01( UPLO, N, A, AINV, RWORK, RESULT( 1 ) )
*
*+    TEST 2
*              Form the inverse and compute the residual.
*
               CALL CCOPY( NPP, AFAC, 1, AINV, 1 )
               SRNAMT = 'CPPTRI'
               CALL CPPTRI( UPLO, N, AINV, INFO )
*
*              Check error code from CPPTRI.
*
               IF( INFO.NE.0 )
     $            CALL ALAERH( PATH, 'CPPTRI', INFO, 0, UPLO, N, N, -1,
     $                         -1, -1, IMAT, NFAIL, NERRS, NOUT )
*
               CALL CPPT03( UPLO, N, A, AINV, WORK, LDA, RWORK, RCONDC,
     $                      RESULT( 2 ) )
*
*              Print information about the tests that did not pass
*              the threshold.
*
               DO 60 K = 1, 2
                  IF( RESULT( K ).GE.THRESH ) THEN
                     IF( NFAIL.EQ.0 .AND. NERRS.EQ.0 )
     $                  CALL ALAHD( NOUT, PATH )
                     WRITE( NOUT, FMT = 9999 )UPLO, N, IMAT, K,
     $                  RESULT( K )
                     NFAIL = NFAIL + 1
                  END IF
   60          CONTINUE
               NRUN = NRUN + 2
*
               DO 80 IRHS = 1, NNS
                  NRHS = NSVAL( IRHS )
*
*+    TEST 3
*              Solve and compute residual for  A * X = B.
*
                  SRNAMT = 'CLARHS'
                  CALL CLARHS( PATH, XTYPE, UPLO, ' ', N, N, KL, KU,
     $                         NRHS, A, LDA, XACT, LDA, B, LDA, ISEED,
     $                         INFO )
                  CALL CLACPY( 'Full', N, NRHS, B, LDA, X, LDA )
*
                  SRNAMT = 'CPPTRS'
                  CALL CPPTRS( UPLO, N, NRHS, AFAC, X, LDA, INFO )
*
*              Check error code from CPPTRS.
*
                  IF( INFO.NE.0 )
     $               CALL ALAERH( PATH, 'CPPTRS', INFO, 0, UPLO, N, N,
     $                            -1, -1, NRHS, IMAT, NFAIL, NERRS,
     $                            NOUT )
*
                  CALL CLACPY( 'Full', N, NRHS, B, LDA, WORK, LDA )
                  CALL CPPT02( UPLO, N, NRHS, A, X, LDA, WORK, LDA,
     $                         RWORK, RESULT( 3 ) )
*
*+    TEST 4
*              Check solution from generated exact solution.
*
                  CALL CGET04( N, NRHS, X, LDA, XACT, LDA, RCONDC,
     $                         RESULT( 4 ) )
*
*+    TESTS 5, 6, and 7
*              Use iterative refinement to improve the solution.
*
                  SRNAMT = 'CPPRFS'
                  CALL CPPRFS( UPLO, N, NRHS, A, AFAC, B, LDA, X, LDA,
     $                         RWORK, RWORK( NRHS+1 ), WORK,
     $                         RWORK( 2*NRHS+1 ), INFO )
*
*              Check error code from CPPRFS.
*
                  IF( INFO.NE.0 )
     $               CALL ALAERH( PATH, 'CPPRFS', INFO, 0, UPLO, N, N,
     $                            -1, -1, NRHS, IMAT, NFAIL, NERRS,
     $                            NOUT )
*
                  CALL CGET04( N, NRHS, X, LDA, XACT, LDA, RCONDC,
     $                         RESULT( 5 ) )
                  CALL CPPT05( UPLO, N, NRHS, A, B, LDA, X, LDA, XACT,
     $                         LDA, RWORK, RWORK( NRHS+1 ),
     $                         RESULT( 6 ) )
*
*                 Print information about the tests that did not pass
*                 the threshold.
*
                  DO 70 K = 3, 7
                     IF( RESULT( K ).GE.THRESH ) THEN
                        IF( NFAIL.EQ.0 .AND. NERRS.EQ.0 )
     $                     CALL ALAHD( NOUT, PATH )
                        WRITE( NOUT, FMT = 9998 )UPLO, N, NRHS, IMAT,
     $                     K, RESULT( K )
                        NFAIL = NFAIL + 1
                     END IF
   70             CONTINUE
                  NRUN = NRUN + 5
   80          CONTINUE
*
*+    TEST 8
*              Get an estimate of RCOND = 1/CNDNUM.
*
               ANORM = CLANHP( '1', UPLO, N, A, RWORK )
               SRNAMT = 'CPPCON'
               CALL CPPCON( UPLO, N, AFAC, ANORM, RCOND, WORK, RWORK,
     $                      INFO )
*
*              Check error code from CPPCON.
*
               IF( INFO.NE.0 )
     $            CALL ALAERH( PATH, 'CPPCON', INFO, 0, UPLO, N, N, -1,
     $                         -1, -1, IMAT, NFAIL, NERRS, NOUT )
*
               RESULT( 8 ) = SGET06( RCOND, RCONDC )
*
*              Print the test ratio if greater than or equal to THRESH.
*
               IF( RESULT( 8 ).GE.THRESH ) THEN
                  IF( NFAIL.EQ.0 .AND. NERRS.EQ.0 )
     $               CALL ALAHD( NOUT, PATH )
                  WRITE( NOUT, FMT = 9999 )UPLO, N, IMAT, 8,
     $               RESULT( 8 )
                  NFAIL = NFAIL + 1
               END IF
               NRUN = NRUN + 1
*
   90       CONTINUE
  100    CONTINUE
  110 CONTINUE
*
*     Print a summary of the results.
*
      CALL ALASUM( PATH, NOUT, NFAIL, NRUN, NERRS )
*
 9999 FORMAT( ' UPLO = ''', A1, ''', N =', I5, ', type ', I2, ', test ',
     $      I2, ', ratio =', G12.5 )
 9998 FORMAT( ' UPLO = ''', A1, ''', N =', I5, ', NRHS=', I3, ', type ',
     $      I2, ', test(', I2, ') =', G12.5 )
      RETURN
*
*     End of CCHKPP
*
      END
