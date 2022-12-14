      SUBROUTINE CCHKTR( DOTYPE, NN, NVAL, NNB, NBVAL, NNS, NSVAL,
     $                   THRESH, TSTERR, NMAX, A, AINV, B, X, XACT,
     $                   WORK, RWORK, NOUT )
*
*  -- LAPACK test routine (version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     December 7, 1999
*
*     .. Scalar Arguments ..
      LOGICAL            TSTERR
      INTEGER            NMAX, NN, NNB, NNS, NOUT
      REAL               THRESH
*     ..
*     .. Array Arguments ..
      LOGICAL            DOTYPE( * )
      INTEGER            NBVAL( * ), NSVAL( * ), NVAL( * )
      REAL               RWORK( * )
      COMPLEX            A( * ), AINV( * ), B( * ), WORK( * ), X( * ),
     $                   XACT( * )
*     ..
*
*  Purpose
*  =======
*
*  CCHKTR tests CTRTRI, -TRS, -RFS, and -CON, and CLATRS
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
*          The values of the matrix column dimension N.
*
*  NNB     (input) INTEGER
*          The number of values of NB contained in the vector NBVAL.
*
*  NBVAL   (input) INTEGER array, dimension (NNB)
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
*          The leading dimension of the work arrays.
*          NMAX >= the maximum value of N in NVAL.
*
*  A       (workspace) COMPLEX array, dimension (NMAX*NMAX)
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
*  NOUT    (input) INTEGER
*          The unit number for output.
*
*  =====================================================================
*
*     .. Parameters ..
      INTEGER            NTYPE1, NTYPES
      PARAMETER          ( NTYPE1 = 10, NTYPES = 18 )
      INTEGER            NTESTS
      PARAMETER          ( NTESTS = 9 )
      INTEGER            NTRAN
      PARAMETER          ( NTRAN = 3 )
      REAL               ONE, ZERO
      PARAMETER          ( ONE = 1.0E0, ZERO = 0.0E0 )
*     ..
*     .. Local Scalars ..
      CHARACTER          DIAG, NORM, TRANS, UPLO, XTYPE
      CHARACTER*3        PATH
      INTEGER            I, IDIAG, IMAT, IN, INB, INFO, IRHS, ITRAN,
     $                   IUPLO, K, LDA, N, NB, NERRS, NFAIL, NRHS, NRUN
      REAL               AINVNM, ANORM, DUMMY, RCOND, RCONDC, RCONDI,
     $                   RCONDO, SCALE
*     ..
*     .. Local Arrays ..
      CHARACTER          TRANSS( NTRAN ), UPLOS( 2 )
      INTEGER            ISEED( 4 ), ISEEDY( 4 )
      REAL               RESULT( NTESTS )
*     ..
*     .. External Functions ..
      LOGICAL            LSAME
      REAL               CLANTR
      EXTERNAL           LSAME, CLANTR
*     ..
*     .. External Subroutines ..
      EXTERNAL           ALAERH, ALAHD, ALASUM, CCOPY, CERRTR, CGET04,
     $                   CLACPY, CLARHS, CLATRS, CLATTR, CTRCON, CTRRFS,
     $                   CTRT01, CTRT02, CTRT03, CTRT05, CTRT06, CTRTRI,
     $                   CTRTRS, XLAENV
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
*     .. Intrinsic Functions ..
      INTRINSIC          MAX
*     ..
*     .. Data statements ..
      DATA               ISEEDY / 1988, 1989, 1990, 1991 /
      DATA               UPLOS / 'U', 'L' / , TRANSS / 'N', 'T', 'C' /
*     ..
*     .. Executable Statements ..
*
*     Initialize constants and the random number seed.
*
      PATH( 1: 1 ) = 'Complex precision'
      PATH( 2: 3 ) = 'TR'
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
     $   CALL CERRTR( PATH, NOUT )
      INFOT = 0
*
      DO 120 IN = 1, NN
*
*        Do for each value of N in NVAL
*
         N = NVAL( IN )
         LDA = MAX( 1, N )
         XTYPE = 'N'
*
         DO 80 IMAT = 1, NTYPE1
*
*           Do the tests only if DOTYPE( IMAT ) is true.
*
            IF( .NOT.DOTYPE( IMAT ) )
     $         GO TO 80
*
            DO 70 IUPLO = 1, 2
*
*              Do first for UPLO = 'U', then for UPLO = 'L'
*
               UPLO = UPLOS( IUPLO )
*
*              Call CLATTR to generate a triangular test matrix.
*
               SRNAMT = 'CLATTR'
               CALL CLATTR( IMAT, UPLO, 'No transpose', DIAG, ISEED, N,
     $                      A, LDA, X, WORK, RWORK, INFO )
*
*              Set IDIAG = 1 for non-unit matrices, 2 for unit.
*
               IF( LSAME( DIAG, 'N' ) ) THEN
                  IDIAG = 1
               ELSE
                  IDIAG = 2
               END IF
*
               DO 60 INB = 1, NNB
*
*                 Do for each blocksize in NBVAL
*
                  NB = NBVAL( INB )
                  CALL XLAENV( 1, NB )
*
*+    TEST 1
*                 Form the inverse of A.
*
                  CALL CLACPY( UPLO, N, N, A, LDA, AINV, LDA )
                  SRNAMT = 'CTRTRI'
                  CALL CTRTRI( UPLO, DIAG, N, AINV, LDA, INFO )
*
*                 Check error code from CTRTRI.
*
                  IF( INFO.NE.0 )
     $               CALL ALAERH( PATH, 'CTRTRI', INFO, 0, UPLO // DIAG,
     $                            N, N, -1, -1, NB, IMAT, NFAIL, NERRS,
     $                            NOUT )
*
*                 Compute the infinity-norm condition number of A.
*
                  ANORM = CLANTR( 'I', UPLO, DIAG, N, N, A, LDA, RWORK )
                  AINVNM = CLANTR( 'I', UPLO, DIAG, N, N, AINV, LDA,
     $                     RWORK )
                  IF( ANORM.LE.ZERO .OR. AINVNM.LE.ZERO ) THEN
                     RCONDI = ONE
                  ELSE
                     RCONDI = ( ONE / ANORM ) / AINVNM
                  END IF
*
*                 Compute the residual for the triangular matrix times
*                 its inverse.  Also compute the 1-norm condition number
*                 of A.
*
                  CALL CTRT01( UPLO, DIAG, N, A, LDA, AINV, LDA, RCONDO,
     $                         RWORK, RESULT( 1 ) )
*                 Print the test ratio if it is .GE. THRESH.
*
                  IF( RESULT( 1 ).GE.THRESH ) THEN
                     IF( NFAIL.EQ.0 .AND. NERRS.EQ.0 )
     $                  CALL ALAHD( NOUT, PATH )
                     WRITE( NOUT, FMT = 9999 )UPLO, DIAG, N, NB, IMAT,
     $                  1, RESULT( 1 )
                     NFAIL = NFAIL + 1
                  END IF
                  NRUN = NRUN + 1
*
*                 Skip remaining tests if not the first block size.
*
                  IF( INB.NE.1 )
     $               GO TO 60
*
                  DO 40 IRHS = 1, NNS
                     NRHS = NSVAL( IRHS )
                     XTYPE = 'N'
*
                     DO 30 ITRAN = 1, NTRAN
*
*                    Do for op(A) = A, A**T, or A**H.
*
                        TRANS = TRANSS( ITRAN )
                        IF( ITRAN.EQ.1 ) THEN
                           NORM = 'O'
                           RCONDC = RCONDO
                        ELSE
                           NORM = 'I'
                           RCONDC = RCONDI
                        END IF
*
*+    TEST 2
*                       Solve and compute residual for op(A)*x = b.
*
                        SRNAMT = 'CLARHS'
                        CALL CLARHS( PATH, XTYPE, UPLO, TRANS, N, N, 0,
     $                               IDIAG, NRHS, A, LDA, XACT, LDA, B,
     $                               LDA, ISEED, INFO )
                        XTYPE = 'C'
                        CALL CLACPY( 'Full', N, NRHS, B, LDA, X, LDA )
*
                        SRNAMT = 'CTRTRS'
                        CALL CTRTRS( UPLO, TRANS, DIAG, N, NRHS, A, LDA,
     $                               X, LDA, INFO )
*
*                       Check error code from CTRTRS.
*
                        IF( INFO.NE.0 )
     $                     CALL ALAERH( PATH, 'CTRTRS', INFO, 0,
     $                                  UPLO // TRANS // DIAG, N, N, -1,
     $                                  -1, NRHS, IMAT, NFAIL, NERRS,
     $                                  NOUT )
*
*                       This line is needed on a Sun SPARCstation.
*
                        IF( N.GT.0 )
     $                     DUMMY = A( 1 )
*
                        CALL CTRT02( UPLO, TRANS, DIAG, N, NRHS, A, LDA,
     $                               X, LDA, B, LDA, WORK, RWORK,
     $                               RESULT( 2 ) )
*
*+    TEST 3
*                       Check solution from generated exact solution.
*
                        CALL CGET04( N, NRHS, X, LDA, XACT, LDA, RCONDC,
     $                               RESULT( 3 ) )
*
*+    TESTS 4, 5, and 6
*                       Use iterative refinement to improve the solution
*                       and compute error bounds.
*
                        SRNAMT = 'CTRRFS'
                        CALL CTRRFS( UPLO, TRANS, DIAG, N, NRHS, A, LDA,
     $                               B, LDA, X, LDA, RWORK,
     $                               RWORK( NRHS+1 ), WORK,
     $                               RWORK( 2*NRHS+1 ), INFO )
*
*                       Check error code from CTRRFS.
*
                        IF( INFO.NE.0 )
     $                     CALL ALAERH( PATH, 'CTRRFS', INFO, 0,
     $                                  UPLO // TRANS // DIAG, N, N, -1,
     $                                  -1, NRHS, IMAT, NFAIL, NERRS,
     $                                  NOUT )
*
                        CALL CGET04( N, NRHS, X, LDA, XACT, LDA, RCONDC,
     $                               RESULT( 4 ) )
                        CALL CTRT05( UPLO, TRANS, DIAG, N, NRHS, A, LDA,
     $                               B, LDA, X, LDA, XACT, LDA, RWORK,
     $                               RWORK( NRHS+1 ), RESULT( 5 ) )
*
*                       Print information about the tests that did not
*                       pass the threshold.
*
                        DO 20 K = 2, 6
                           IF( RESULT( K ).GE.THRESH ) THEN
                              IF( NFAIL.EQ.0 .AND. NERRS.EQ.0 )
     $                           CALL ALAHD( NOUT, PATH )
                              WRITE( NOUT, FMT = 9998 )UPLO, TRANS,
     $                           DIAG, N, NRHS, IMAT, K, RESULT( K )
                              NFAIL = NFAIL + 1
                           END IF
   20                   CONTINUE
                        NRUN = NRUN + 5
   30                CONTINUE
   40             CONTINUE
*
*+    TEST 7
*                       Get an estimate of RCOND = 1/CNDNUM.
*
                  DO 50 ITRAN = 1, 2
                     IF( ITRAN.EQ.1 ) THEN
                        NORM = 'O'
                        RCONDC = RCONDO
                     ELSE
                        NORM = 'I'
                        RCONDC = RCONDI
                     END IF
                     SRNAMT = 'CTRCON'
                     CALL CTRCON( NORM, UPLO, DIAG, N, A, LDA, RCOND,
     $                            WORK, RWORK, INFO )
*
*                       Check error code from CTRCON.
*
                     IF( INFO.NE.0 )
     $                  CALL ALAERH( PATH, 'CTRCON', INFO, 0,
     $                               NORM // UPLO // DIAG, N, N, -1, -1,
     $                               -1, IMAT, NFAIL, NERRS, NOUT )
*
                     CALL CTRT06( RCOND, RCONDC, UPLO, DIAG, N, A, LDA,
     $                            RWORK, RESULT( 7 ) )
*
*                    Print the test ratio if it is .GE. THRESH.
*
                     IF( RESULT( 7 ).GE.THRESH ) THEN
                        IF( NFAIL.EQ.0 .AND. NERRS.EQ.0 )
     $                     CALL ALAHD( NOUT, PATH )
                        WRITE( NOUT, FMT = 9997 )NORM, UPLO, N, IMAT,
     $                     7, RESULT( 7 )
                        NFAIL = NFAIL + 1
                     END IF
                     NRUN = NRUN + 1
   50             CONTINUE
   60          CONTINUE
   70       CONTINUE
   80    CONTINUE
*
*        Use pathological test matrices to test CLATRS.
*
         DO 110 IMAT = NTYPE1 + 1, NTYPES
*
*           Do the tests only if DOTYPE( IMAT ) is true.
*
            IF( .NOT.DOTYPE( IMAT ) )
     $         GO TO 110
*
            DO 100 IUPLO = 1, 2
*
*              Do first for UPLO = 'U', then for UPLO = 'L'
*
               UPLO = UPLOS( IUPLO )
               DO 90 ITRAN = 1, NTRAN
*
*                 Do for op(A) = A, A**T, and A**H.
*
                  TRANS = TRANSS( ITRAN )
*
*                 Call CLATTR to generate a triangular test matrix.
*
                  SRNAMT = 'CLATTR'
                  CALL CLATTR( IMAT, UPLO, TRANS, DIAG, ISEED, N, A,
     $                         LDA, X, WORK, RWORK, INFO )
*
*+    TEST 8
*                 Solve the system op(A)*x = b.
*
                  SRNAMT = 'CLATRS'
                  CALL CCOPY( N, X, 1, B, 1 )
                  CALL CLATRS( UPLO, TRANS, DIAG, 'N', N, A, LDA, B,
     $                         SCALE, RWORK, INFO )
*
*                 Check error code from CLATRS.
*
                  IF( INFO.NE.0 )
     $               CALL ALAERH( PATH, 'CLATRS', INFO, 0,
     $                            UPLO // TRANS // DIAG // 'N', N, N,
     $                            -1, -1, -1, IMAT, NFAIL, NERRS, NOUT )
*
                  CALL CTRT03( UPLO, TRANS, DIAG, N, 1, A, LDA, SCALE,
     $                         RWORK, ONE, B, LDA, X, LDA, WORK,
     $                         RESULT( 8 ) )
*
*+    TEST 9
*                 Solve op(A)*X = b again with NORMIN = 'Y'.
*
                  CALL CCOPY( N, X, 1, B( N+1 ), 1 )
                  CALL CLATRS( UPLO, TRANS, DIAG, 'Y', N, A, LDA,
     $                         B( N+1 ), SCALE, RWORK, INFO )
*
*                 Check error code from CLATRS.
*
                  IF( INFO.NE.0 )
     $               CALL ALAERH( PATH, 'CLATRS', INFO, 0,
     $                            UPLO // TRANS // DIAG // 'Y', N, N,
     $                            -1, -1, -1, IMAT, NFAIL, NERRS, NOUT )
*
                  CALL CTRT03( UPLO, TRANS, DIAG, N, 1, A, LDA, SCALE,
     $                         RWORK, ONE, B( N+1 ), LDA, X, LDA, WORK,
     $                         RESULT( 9 ) )
*
*                 Print information about the tests that did not pass
*                 the threshold.
*
                  IF( RESULT( 8 ).GE.THRESH ) THEN
                     IF( NFAIL.EQ.0 .AND. NERRS.EQ.0 )
     $                  CALL ALAHD( NOUT, PATH )
                     WRITE( NOUT, FMT = 9996 )'CLATRS', UPLO, TRANS,
     $                  DIAG, 'N', N, IMAT, 8, RESULT( 8 )
                     NFAIL = NFAIL + 1
                  END IF
                  IF( RESULT( 9 ).GE.THRESH ) THEN
                     IF( NFAIL.EQ.0 .AND. NERRS.EQ.0 )
     $                  CALL ALAHD( NOUT, PATH )
                     WRITE( NOUT, FMT = 9996 )'CLATRS', UPLO, TRANS,
     $                  DIAG, 'Y', N, IMAT, 9, RESULT( 9 )
                     NFAIL = NFAIL + 1
                  END IF
                  NRUN = NRUN + 2
   90          CONTINUE
  100       CONTINUE
  110    CONTINUE
  120 CONTINUE
*
*     Print a summary of the results.
*
      CALL ALASUM( PATH, NOUT, NFAIL, NRUN, NERRS )
*
 9999 FORMAT( ' UPLO=''', A1, ''', DIAG=''', A1, ''', N=', I5, ', NB=',
     $      I4, ', type ', I2, ', test(', I2, ')= ', G12.5 )
 9998 FORMAT( ' UPLO=''', A1, ''', TRANS=''', A1, ''', DIAG=''', A1,
     $      ''', N=', I5, ', NB=', I4, ', type ', I2, ',
     $      test(', I2, ')= ', G12.5 )
 9997 FORMAT( ' NORM=''', A1, ''', UPLO =''', A1, ''', N=', I5, ',',
     $      11X, ' type ', I2, ', test(', I2, ')=', G12.5 )
 9996 FORMAT( 1X, A6, '( ''', A1, ''', ''', A1, ''', ''', A1, ''', ''',
     $      A1, ''',', I5, ', ... ), type ', I2, ', test(', I2, ')=',
     $      G12.5 )
      RETURN
*
*     End of CCHKTR
*
      END
