      SUBROUTINE CCHKHE( DOTYPE, NN, NVAL, NNB, NBVAL, NNS, NSVAL,
     $                   THRESH, TSTERR, NMAX, A, AFAC, AINV, B, X,
     $                   XACT, WORK, RWORK, IWORK, NOUT )
*
*  -- LAPACK test routine (version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     June 30, 1999
*
*     .. Scalar Arguments ..
      LOGICAL            TSTERR
      INTEGER            NMAX, NN, NNB, NNS, NOUT
      REAL               THRESH
*     ..
*     .. Array Arguments ..
      LOGICAL            DOTYPE( * )
      INTEGER            IWORK( * ), NBVAL( * ), NSVAL( * ), NVAL( * )
      REAL               RWORK( * )
      COMPLEX            A( * ), AFAC( * ), AINV( * ), B( * ),
     $                   WORK( * ), X( * ), XACT( * )
*     ..
*
*  Purpose
*  =======
*
*  CCHKHE tests CHETRF, -TRI, -TRS, -RFS, and -CON.
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
*  NNB     (input) INTEGER
*          The number of values of NB contained in the vector NBVAL.
*
*  NBVAL   (input) INTEGER array, dimension (NBVAL)
*          The values of the blocksize NB.
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
*  A       (workspace) COMPLEX array, dimension (NMAX*NMAX)
*
*  AFAC    (workspace) COMPLEX array, dimension (NMAX*NMAX)
*
*  AINV    (workspace) COMPLEX array, dimension (NMAX*NMAX)
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
*  IWORK   (workspace) INTEGER array, dimension (NMAX)
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
      PARAMETER          ( NTYPES = 10 )
      INTEGER            NTESTS
      PARAMETER          ( NTESTS = 8 )
*     ..
*     .. Local Scalars ..
      LOGICAL            TRFCON, ZEROT
      CHARACTER          DIST, TYPE, UPLO, XTYPE
      CHARACTER*3        PATH
      INTEGER            I, I1, I2, IMAT, IN, INB, INFO, IOFF, IRHS,
     $                   IUPLO, IZERO, J, K, KL, KU, LDA, LWORK, MODE,
     $                   N, NB, NERRS, NFAIL, NIMAT, NRHS, NRUN, NT
      REAL               ANORM, CNDNUM, RCOND, RCONDC
*     ..
*     .. Local Arrays ..
      CHARACTER          UPLOS( 2 )
      INTEGER            ISEED( 4 ), ISEEDY( 4 )
      REAL               RESULT( NTESTS )
*     ..
*     .. External Functions ..
      REAL               CLANHE, SGET06
      EXTERNAL           CLANHE, SGET06
*     ..
*     .. External Subroutines ..
      EXTERNAL           ALAERH, ALAHD, ALASUM, CERRHE, CGET04, CHECON,
     $                   CHERFS, CHET01, CHETRF, CHETRI, CHETRS, CLACPY,
     $                   CLAIPD, CLARHS, CLATB4, CLATMS, CPOT02, CPOT03,
     $                   CPOT05, XLAENV
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          MAX, MIN
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
*     .. Data statements ..
      DATA               ISEEDY / 1988, 1989, 1990, 1991 /
      DATA               UPLOS / 'U', 'L' /
*     ..
*     .. Executable Statements ..
*
*     Initialize constants and the random number seed.
*
      PATH( 1: 1 ) = 'Complex precision'
      PATH( 2: 3 ) = 'HE'
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
     $   CALL CERRHE( PATH, NOUT )
      INFOT = 0
*
*     Do for each value of N in NVAL
*
      DO 180 IN = 1, NN
         N = NVAL( IN )
         LDA = MAX( N, 1 )
         XTYPE = 'N'
         NIMAT = NTYPES
         IF( N.LE.0 )
     $      NIMAT = 1
*
         IZERO = 0
         DO 170 IMAT = 1, NIMAT
*
*           Do the tests only if DOTYPE( IMAT ) is true.
*
            IF( .NOT.DOTYPE( IMAT ) )
     $         GO TO 170
*
*           Skip types 3, 4, 5, or 6 if the matrix size is too small.
*
            ZEROT = IMAT.GE.3 .AND. IMAT.LE.6
            IF( ZEROT .AND. N.LT.IMAT-2 )
     $         GO TO 170
*
*           Do first for UPLO = 'U', then for UPLO = 'L'
*
            DO 160 IUPLO = 1, 2
               UPLO = UPLOS( IUPLO )
*
*              Set up parameters with CLATB4 and generate a test matrix
*              with CLATMS.
*
               CALL CLATB4( PATH, IMAT, N, N, TYPE, KL, KU, ANORM, MODE,
     $                      CNDNUM, DIST )
*
               SRNAMT = 'CLATMS'
               CALL CLATMS( N, N, DIST, ISEED, TYPE, RWORK, MODE,
     $                      CNDNUM, ANORM, KL, KU, UPLO, A, LDA, WORK,
     $                      INFO )
*
*              Check error code from CLATMS.
*
               IF( INFO.NE.0 ) THEN
                  CALL ALAERH( PATH, 'CLATMS', INFO, 0, UPLO, N, N, -1,
     $                         -1, -1, IMAT, NFAIL, NERRS, NOUT )
                  GO TO 160
               END IF
*
*              For types 3-6, zero one or more rows and columns of
*              the matrix to test that INFO is returned correctly.
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
                  IF( IMAT.LT.6 ) THEN
*
*                    Set row and column IZERO to zero.
*
                     IF( IUPLO.EQ.1 ) THEN
                        IOFF = ( IZERO-1 )*LDA
                        DO 20 I = 1, IZERO - 1
                           A( IOFF+I ) = ZERO
   20                   CONTINUE
                        IOFF = IOFF + IZERO
                        DO 30 I = IZERO, N
                           A( IOFF ) = ZERO
                           IOFF = IOFF + LDA
   30                   CONTINUE
                     ELSE
                        IOFF = IZERO
                        DO 40 I = 1, IZERO - 1
                           A( IOFF ) = ZERO
                           IOFF = IOFF + LDA
   40                   CONTINUE
                        IOFF = IOFF - IZERO
                        DO 50 I = IZERO, N
                           A( IOFF+I ) = ZERO
   50                   CONTINUE
                     END IF
                  ELSE
                     IOFF = 0
                     IF( IUPLO.EQ.1 ) THEN
*
*                       Set the first IZERO rows and columns to zero.
*
                        DO 70 J = 1, N
                           I2 = MIN( J, IZERO )
                           DO 60 I = 1, I2
                              A( IOFF+I ) = ZERO
   60                      CONTINUE
                           IOFF = IOFF + LDA
   70                   CONTINUE
                     ELSE
*
*                       Set the last IZERO rows and columns to zero.
*
                        DO 90 J = 1, N
                           I1 = MAX( J, IZERO )
                           DO 80 I = I1, N
                              A( IOFF+I ) = ZERO
   80                      CONTINUE
                           IOFF = IOFF + LDA
   90                   CONTINUE
                     END IF
                  END IF
               ELSE
                  IZERO = 0
               END IF
*
*              Set the imaginary part of the diagonals.
*
               CALL CLAIPD( N, A, LDA+1, 0 )
*
*              Do for each value of NB in NBVAL
*
               DO 150 INB = 1, NNB
                  NB = NBVAL( INB )
                  CALL XLAENV( 1, NB )
*
*                 Compute the L*D*L' or U*D*U' factorization of the
*                 matrix.
*
                  CALL CLACPY( UPLO, N, N, A, LDA, AFAC, LDA )
                  LWORK = MAX( 2, NB )*LDA
                  SRNAMT = 'CHETRF'
                  CALL CHETRF( UPLO, N, AFAC, LDA, IWORK, AINV, LWORK,
     $                         INFO )
*
*                 Adjust the expected value of INFO to account for
*                 pivoting.
*
                  K = IZERO
                  IF( K.GT.0 ) THEN
  100                CONTINUE
                     IF( IWORK( K ).LT.0 ) THEN
                        IF( IWORK( K ).NE.-K ) THEN
                           K = -IWORK( K )
                           GO TO 100
                        END IF
                     ELSE IF( IWORK( K ).NE.K ) THEN
                        K = IWORK( K )
                        GO TO 100
                     END IF
                  END IF
*
*                 Check error code from CHETRF.
*
                  IF( INFO.NE.K )
     $               CALL ALAERH( PATH, 'CHETRF', INFO, K, UPLO, N, N,
     $                            -1, -1, NB, IMAT, NFAIL, NERRS, NOUT )
                  IF( INFO.NE.0 ) THEN
                     TRFCON = .TRUE.
                  ELSE
                     TRFCON = .FALSE.
                  END IF
*
*+    TEST 1
*                 Reconstruct matrix from factors and compute residual.
*
                  CALL CHET01( UPLO, N, A, LDA, AFAC, LDA, IWORK, AINV,
     $                         LDA, RWORK, RESULT( 1 ) )
                  NT = 1
*
*+    TEST 2
*                 Form the inverse and compute the residual.
*
                  IF( INB.EQ.1 .AND. .NOT.TRFCON ) THEN
                     CALL CLACPY( UPLO, N, N, AFAC, LDA, AINV, LDA )
                     SRNAMT = 'CHETRI'
                     CALL CHETRI( UPLO, N, AINV, LDA, IWORK, WORK,
     $                            INFO )
*
*                 Check error code from CHETRI.
*
                     IF( INFO.NE.0 )
     $                  CALL ALAERH( PATH, 'CHETRI', INFO, -1, UPLO, N,
     $                               N, -1, -1, -1, IMAT, NFAIL, NERRS,
     $                               NOUT )
*
                     CALL CPOT03( UPLO, N, A, LDA, AINV, LDA, WORK, LDA,
     $                            RWORK, RCONDC, RESULT( 2 ) )
                     NT = 2
                  END IF
*
*                 Print information about the tests that did not pass
*                 the threshold.
*
                  DO 110 K = 1, NT
                     IF( RESULT( K ).GE.THRESH ) THEN
                        IF( NFAIL.EQ.0 .AND. NERRS.EQ.0 )
     $                     CALL ALAHD( NOUT, PATH )
                        WRITE( NOUT, FMT = 9999 )UPLO, N, NB, IMAT, K,
     $                     RESULT( K )
                        NFAIL = NFAIL + 1
                     END IF
  110             CONTINUE
                  NRUN = NRUN + NT
*
*                 Skip the other tests if this is not the first block
*                 size.
*
                  IF( INB.GT.1 )
     $               GO TO 150
*
*                 Do only the condition estimate if INFO is not 0.
*
                  IF( TRFCON ) THEN
                     RCONDC = ZERO
                     GO TO 140
                  END IF
*
                  DO 130 IRHS = 1, NNS
                     NRHS = NSVAL( IRHS )
*
*+    TEST 3
*                 Solve and compute residual for  A * X = B.
*
                     SRNAMT = 'CLARHS'
                     CALL CLARHS( PATH, XTYPE, UPLO, ' ', N, N, KL, KU,
     $                            NRHS, A, LDA, XACT, LDA, B, LDA,
     $                            ISEED, INFO )
                     XTYPE = 'C'
                     CALL CLACPY( 'Full', N, NRHS, B, LDA, X, LDA )
*
                     SRNAMT = 'CHETRS'
                     CALL CHETRS( UPLO, N, NRHS, AFAC, LDA, IWORK, X,
     $                            LDA, INFO )
*
*                 Check error code from CHETRS.
*
                     IF( INFO.NE.0 )
     $                  CALL ALAERH( PATH, 'CHETRS', INFO, 0, UPLO, N,
     $                               N, -1, -1, NRHS, IMAT, NFAIL,
     $                               NERRS, NOUT )
*
                     CALL CLACPY( 'Full', N, NRHS, B, LDA, WORK, LDA )
                     CALL CPOT02( UPLO, N, NRHS, A, LDA, X, LDA, WORK,
     $                            LDA, RWORK, RESULT( 3 ) )
*
*+    TEST 4
*                 Check solution from generated exact solution.
*
                     CALL CGET04( N, NRHS, X, LDA, XACT, LDA, RCONDC,
     $                            RESULT( 4 ) )
*
*+    TESTS 5, 6, and 7
*                 Use iterative refinement to improve the solution.
*
                     SRNAMT = 'CHERFS'
                     CALL CHERFS( UPLO, N, NRHS, A, LDA, AFAC, LDA,
     $                            IWORK, B, LDA, X, LDA, RWORK,
     $                            RWORK( NRHS+1 ), WORK,
     $                            RWORK( 2*NRHS+1 ), INFO )
*
*                 Check error code from CHERFS.
*
                     IF( INFO.NE.0 )
     $                  CALL ALAERH( PATH, 'CHERFS', INFO, 0, UPLO, N,
     $                               N, -1, -1, NRHS, IMAT, NFAIL,
     $                               NERRS, NOUT )
*
                     CALL CGET04( N, NRHS, X, LDA, XACT, LDA, RCONDC,
     $                            RESULT( 5 ) )
                     CALL CPOT05( UPLO, N, NRHS, A, LDA, B, LDA, X, LDA,
     $                            XACT, LDA, RWORK, RWORK( NRHS+1 ),
     $                            RESULT( 6 ) )
*
*                    Print information about the tests that did not pass
*                    the threshold.
*
                     DO 120 K = 3, 7
                        IF( RESULT( K ).GE.THRESH ) THEN
                           IF( NFAIL.EQ.0 .AND. NERRS.EQ.0 )
     $                        CALL ALAHD( NOUT, PATH )
                           WRITE( NOUT, FMT = 9998 )UPLO, N, NRHS,
     $                        IMAT, K, RESULT( K )
                           NFAIL = NFAIL + 1
                        END IF
  120                CONTINUE
                     NRUN = NRUN + 5
  130             CONTINUE
*
*+    TEST 8
*                 Get an estimate of RCOND = 1/CNDNUM.
*
  140             CONTINUE
                  ANORM = CLANHE( '1', UPLO, N, A, LDA, RWORK )
                  SRNAMT = 'CHECON'
                  CALL CHECON( UPLO, N, AFAC, LDA, IWORK, ANORM, RCOND,
     $                         WORK, INFO )
*
*                 Check error code from CHECON.
*
                  IF( INFO.NE.0 )
     $               CALL ALAERH( PATH, 'CHECON', INFO, 0, UPLO, N, N,
     $                            -1, -1, -1, IMAT, NFAIL, NERRS, NOUT )
*
                  RESULT( 8 ) = SGET06( RCOND, RCONDC )
*
*                 Print information about the tests that did not pass
*                 the threshold.
*
                  IF( RESULT( 8 ).GE.THRESH ) THEN
                     IF( NFAIL.EQ.0 .AND. NERRS.EQ.0 )
     $                  CALL ALAHD( NOUT, PATH )
                     WRITE( NOUT, FMT = 9997 )UPLO, N, IMAT, 8,
     $                  RESULT( 8 )
                     NFAIL = NFAIL + 1
                  END IF
                  NRUN = NRUN + 1
  150          CONTINUE
  160       CONTINUE
  170    CONTINUE
  180 CONTINUE
*
*     Print a summary of the results.
*
      CALL ALASUM( PATH, NOUT, NFAIL, NRUN, NERRS )
*
 9999 FORMAT( ' UPLO = ''', A1, ''', N =', I5, ', NB =', I4, ', type ',
     $      I2, ', test ', I2, ', ratio =', G12.5 )
 9998 FORMAT( ' UPLO = ''', A1, ''', N =', I5, ', NRHS=', I3, ', type ',
     $      I2, ', test(', I2, ') =', G12.5 )
 9997 FORMAT( ' UPLO = ''', A1, ''', N =', I5, ',', 10X, ' type ', I2,
     $      ', test(', I2, ') =', G12.5 )
      RETURN
*
*     End of CCHKHE
*
      END
