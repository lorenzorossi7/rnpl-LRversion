      SUBROUTINE ZDRVLS( DOTYPE, NM, MVAL, NN, NVAL, NNS, NSVAL, NNB,
     $                   NBVAL, NXVAL, THRESH, TSTERR, A, COPYA, B,
     $                   COPYB, C, S, COPYS, WORK, RWORK, IWORK, NOUT )
*
*  -- LAPACK test routine (version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     January 3, 2000
*
*     .. Scalar Arguments ..
      LOGICAL            TSTERR
      INTEGER            NM, NN, NNB, NNS, NOUT
      DOUBLE PRECISION   THRESH
*     ..
*     .. Array Arguments ..
      LOGICAL            DOTYPE( * )
      INTEGER            IWORK( * ), MVAL( * ), NBVAL( * ), NSVAL( * ),
     $                   NVAL( * ), NXVAL( * )
      DOUBLE PRECISION   COPYS( * ), RWORK( * ), S( * )
      COMPLEX*16         A( * ), B( * ), C( * ), COPYA( * ), COPYB( * ),
     $                   WORK( * )
*     ..
*
*  Purpose
*  =======
*
*  ZDRVLS tests the least squares driver routines ZGELS, CGELSX, CGELSS,
*  ZGELSY and CGELSD.
*
*  Arguments
*  =========
*
*  DOTYPE  (input) LOGICAL array, dimension (NTYPES)
*          The matrix types to be used for testing.  Matrices of type j
*          (for 1 <= j <= NTYPES) are used for testing if DOTYPE(j) =
*          .TRUE.; if DOTYPE(j) = .FALSE., then type j is not used.
*          The matrix of type j is generated as follows:
*          j=1: A = U*D*V where U and V are random unitary matrices
*               and D has random entries (> 0.1) taken from a uniform
*               distribution (0,1). A is full rank.
*          j=2: The same of 1, but A is scaled up.
*          j=3: The same of 1, but A is scaled down.
*          j=4: A = U*D*V where U and V are random unitary matrices
*               and D has 3*min(M,N)/4 random entries (> 0.1) taken
*               from a uniform distribution (0,1) and the remaining
*               entries set to 0. A is rank-deficient.
*          j=5: The same of 4, but A is scaled up.
*          j=6: The same of 5, but A is scaled down.
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
*  NNB     (input) INTEGER
*          The number of values of NB and NX contained in the
*          vectors NBVAL and NXVAL.  The blocking parameters are used
*          in pairs (NB,NX).
*
*  NBVAL   (input) INTEGER array, dimension (NNB)
*          The values of the blocksize NB.
*
*  NXVAL   (input) INTEGER array, dimension (NNB)
*          The values of the crossover point NX.
*
*  NNS     (input) INTEGER
*          The number of values of NRHS contained in the vector NSVAL.
*
*  NSVAL   (input) INTEGER array, dimension (NNS)
*          The values of the number of right hand sides NRHS.
*
*  THRESH  (input) DOUBLE PRECISION
*          The threshold value for the test ratios.  A result is
*          included in the output file if RESULT >= THRESH.  To have
*          every test ratio printed, use THRESH = 0.
*
*  TSTERR  (input) LOGICAL
*          Flag that indicates whether error exits are to be tested.
*
*  A       (workspace) COMPLEX*16 array, dimension (MMAX*NMAX)
*          where MMAX is the maximum value of M in MVAL and NMAX is the
*          maximum value of N in NVAL.
*
*  COPYA   (workspace) COMPLEX*16 array, dimension (MMAX*NMAX)
*
*  B       (workspace) COMPLEX*16 array, dimension (MMAX*NSMAX)
*          where MMAX is the maximum value of M in MVAL and NSMAX is the
*          maximum value of NRHS in NSVAL.
*
*  COPYB   (workspace) COMPLEX*16 array, dimension (MMAX*NSMAX)
*
*  C       (workspace) COMPLEX*16 array, dimension (MMAX*NSMAX)
*
*  S       (workspace) DOUBLE PRECISION array, dimension
*                      (min(MMAX,NMAX))
*
*  COPYS   (workspace) DOUBLE PRECISION array, dimension
*                      (min(MMAX,NMAX))
*
*  WORK    (workspace) COMPLEX*16 array, dimension
*                      (MMAX*NMAX + 4*NMAX + MMAX).
*
*  RWORK   (workspace) DOUBLE PRECISION array, dimension (5*NMAX-1)
*
*  IWORK   (workspace) INTEGER array, dimension (15*NMAX)
*
*  NOUT    (input) INTEGER
*          The unit number for output.
*
*  =====================================================================
*
*     .. Parameters ..
      INTEGER            NTESTS
      PARAMETER          ( NTESTS = 18 )
      DOUBLE PRECISION   ONE, ZERO
      PARAMETER          ( ONE = 1.0D+0, ZERO = 0.0D+0 )
      COMPLEX*16         CONE, CZERO
      PARAMETER          ( CONE = ( 1.0D+0, 0.0D+0 ),
     $                   CZERO = ( 0.0D+0, 0.0D+0 ) )
*     ..
*     .. Local Scalars ..
      CHARACTER          TRANS
      CHARACTER*3        PATH
      INTEGER            CRANK, I, IM, IN, INB, INFO, INS, IRANK,
     $                   ISCALE, ITRAN, ITYPE, J, K, LDA, LDB, LDWORK,
     $                   LWLSY, LWORK, M, MNMIN, N, NB, NCOLS, NERRS,
     $                   NFAIL, NRHS, NROWS, NRUN, RANK
      DOUBLE PRECISION   EPS, NORMA, NORMB, RCOND
*     ..
*     .. Local Arrays ..
      INTEGER            ISEED( 4 ), ISEEDY( 4 )
      DOUBLE PRECISION   RESULT( NTESTS )
*     ..
*     .. External Functions ..
      DOUBLE PRECISION   DASUM, DLAMCH, ZQRT12, ZQRT14, ZQRT17
      EXTERNAL           DASUM, DLAMCH, ZQRT12, ZQRT14, ZQRT17
*     ..
*     .. External Subroutines ..
      EXTERNAL           ALAERH, ALAHD, ALASVM, DAXPY, DLASRT, XLAENV,
     $                   ZDSCAL, ZERRLS, ZGELS, ZGELSD, ZGELSS, ZGELSX,
     $                   ZGELSY, ZGEMM, ZLACPY, ZLARNV, ZQRT13, ZQRT15,
     $                   ZQRT16
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          DBLE, MAX, MIN, SQRT
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
      PATH( 1: 1 ) = 'Zomplex precision'
      PATH( 2: 3 ) = 'LS'
      NRUN = 0
      NFAIL = 0
      NERRS = 0
      DO 10 I = 1, 4
         ISEED( I ) = ISEEDY( I )
   10 CONTINUE
      EPS = DLAMCH( 'Epsilon' )
*
*     Threshold for rank estimation
*
      RCOND = SQRT( EPS ) - ( SQRT( EPS )-EPS ) / 2
*
*     Test the error exits
*
      IF( TSTERR )
     $   CALL ZERRLS( PATH, NOUT )
*
*     Print the header if NM = 0 or NN = 0 and THRESH = 0.
*
      IF( ( NM.EQ.0 .OR. NN.EQ.0 ) .AND. THRESH.EQ.ZERO )
     $   CALL ALAHD( NOUT, PATH )
      INFOT = 0
*
      DO 140 IM = 1, NM
         M = MVAL( IM )
         LDA = MAX( 1, M )
*
         DO 130 IN = 1, NN
            N = NVAL( IN )
            MNMIN = MIN( M, N )
            LDB = MAX( 1, M, N )
*
            DO 120 INS = 1, NNS
               NRHS = NSVAL( INS )
               LWORK = MAX( 1, ( M+NRHS )*( N+2 ), ( N+NRHS )*( M+2 ),
     $                 M*N+4*MNMIN+MAX( M, N ), 2*N+M )
*
               DO 110 IRANK = 1, 2
                  DO 100 ISCALE = 1, 3
                     ITYPE = ( IRANK-1 )*3 + ISCALE
                     IF( .NOT.DOTYPE( ITYPE ) )
     $                  GO TO 100
*
                     IF( IRANK.EQ.1 ) THEN
*
*                       Test ZGELS
*
*                       Generate a matrix of scaling type ISCALE
*
                        CALL ZQRT13( ISCALE, M, N, COPYA, LDA, NORMA,
     $                               ISEED )
                        DO 40 INB = 1, NNB
                           NB = NBVAL( INB )
                           CALL XLAENV( 1, NB )
                           CALL XLAENV( 3, NXVAL( INB ) )
*
                           DO 30 ITRAN = 1, 2
                              IF( ITRAN.EQ.1 ) THEN
                                 TRANS = 'N'
                                 NROWS = M
                                 NCOLS = N
                              ELSE
                                 TRANS = 'C'
                                 NROWS = N
                                 NCOLS = M
                              END IF
                              LDWORK = MAX( 1, NCOLS )
*
*                             Set up a consistent rhs
*
                              IF( NCOLS.GT.0 ) THEN
                                 CALL ZLARNV( 2, ISEED, NCOLS*NRHS,
     $                                        WORK )
                                 CALL ZDSCAL( NCOLS*NRHS,
     $                                        ONE / DBLE( NCOLS ), WORK,
     $                                        1 )
                              END IF
                              CALL ZGEMM( TRANS, 'No transpose', NROWS,
     $                                    NRHS, NCOLS, CONE, COPYA, LDA,
     $                                    WORK, LDWORK, CZERO, B, LDB )
                              CALL ZLACPY( 'Full', NROWS, NRHS, B, LDB,
     $                                     COPYB, LDB )
*
*                             Solve LS or overdetermined system
*
                              IF( M.GT.0 .AND. N.GT.0 ) THEN
                                 CALL ZLACPY( 'Full', M, N, COPYA, LDA,
     $                                        A, LDA )
                                 CALL ZLACPY( 'Full', NROWS, NRHS,
     $                                        COPYB, LDB, B, LDB )
                              END IF
                              SRNAMT = 'ZGELS '
                              CALL ZGELS( TRANS, M, N, NRHS, A, LDA, B,
     $                                    LDB, WORK, LWORK, INFO )
*
                              IF( INFO.NE.0 )
     $                           CALL ALAERH( PATH, 'ZGELS ', INFO, 0,
     $                                        TRANS, M, N, NRHS, -1, NB,
     $                                        ITYPE, NFAIL, NERRS,
     $                                        NOUT )
*
*                             Check correctness of results
*
                              LDWORK = MAX( 1, NROWS )
                              IF( NROWS.GT.0 .AND. NRHS.GT.0 )
     $                           CALL ZLACPY( 'Full', NROWS, NRHS,
     $                                        COPYB, LDB, C, LDB )
                              CALL ZQRT16( TRANS, M, N, NRHS, COPYA,
     $                                     LDA, B, LDB, C, LDB, RWORK,
     $                                     RESULT( 1 ) )
*
                              IF( ( ITRAN.EQ.1 .AND. M.GE.N ) .OR.
     $                            ( ITRAN.EQ.2 .AND. M.LT.N ) ) THEN
*
*                                Solving LS system
*
                                 RESULT( 2 ) = ZQRT17( TRANS, 1, M, N,
     $                                         NRHS, COPYA, LDA, B, LDB,
     $                                         COPYB, LDB, C, WORK,
     $                                         LWORK )
                              ELSE
*
*                                Solving overdetermined system
*
                                 RESULT( 2 ) = ZQRT14( TRANS, M, N,
     $                                         NRHS, COPYA, LDA, B, LDB,
     $                                         WORK, LWORK )
                              END IF
*
*                             Print information about the tests that
*                             did not pass the threshold.
*
                              DO 20 K = 1, 2
                                 IF( RESULT( K ).GE.THRESH ) THEN
                                    IF( NFAIL.EQ.0 .AND. NERRS.EQ.0 )
     $                                 CALL ALAHD( NOUT, PATH )
                                    WRITE( NOUT, FMT = 9999 )TRANS, M,
     $                                 N, NRHS, NB, ITYPE, K,
     $                                 RESULT( K )
                                    NFAIL = NFAIL + 1
                                 END IF
   20                         CONTINUE
                              NRUN = NRUN + 2
   30                      CONTINUE
   40                   CONTINUE
                     END IF
*
*                    Generate a matrix of scaling type ISCALE and rank
*                    type IRANK.
*
                     CALL ZQRT15( ISCALE, IRANK, M, N, NRHS, COPYA, LDA,
     $                            COPYB, LDB, COPYS, RANK, NORMA, NORMB,
     $                            ISEED, WORK, LWORK )
*
*                    workspace used: MAX(M+MIN(M,N),NRHS*MIN(M,N),2*N+M)
*
                     DO 50 J = 1, N
                        IWORK( J ) = 0
   50                CONTINUE
                     LDWORK = MAX( 1, M )
*
*                    Test ZGELSX
*
*                    ZGELSX:  Compute the minimum-norm solution X
*                    to min( norm( A * X - B ) )
*                    using a complete orthogonal factorization.
*
                     CALL ZLACPY( 'Full', M, N, COPYA, LDA, A, LDA )
                     CALL ZLACPY( 'Full', M, NRHS, COPYB, LDB, B, LDB )
*
                     SRNAMT = 'ZGELSX'
                     CALL ZGELSX( M, N, NRHS, A, LDA, B, LDB, IWORK,
     $                            RCOND, CRANK, WORK, RWORK, INFO )
*
                     IF( INFO.NE.0 )
     $                  CALL ALAERH( PATH, 'ZGELSX', INFO, 0, ' ', M, N,
     $                               NRHS, -1, NB, ITYPE, NFAIL, NERRS,
     $                               NOUT )
*
*                    workspace used: MAX( MNMIN+3*N, 2*MNMIN+NRHS )
*
*                    Test 3:  Compute relative error in svd
*                             workspace: M*N + 4*MIN(M,N) + MAX(M,N)
*
                     RESULT( 3 ) = ZQRT12( CRANK, CRANK, A, LDA, COPYS,
     $                             WORK, LWORK, RWORK )
*
*                    Test 4:  Compute error in solution
*                             workspace:  M*NRHS + M
*
                     CALL ZLACPY( 'Full', M, NRHS, COPYB, LDB, WORK,
     $                            LDWORK )
                     CALL ZQRT16( 'No transpose', M, N, NRHS, COPYA,
     $                            LDA, B, LDB, WORK, LDWORK, RWORK,
     $                            RESULT( 4 ) )
*
*                    Test 5:  Check norm of r'*A
*                             workspace: NRHS*(M+N)
*
                     RESULT( 5 ) = ZERO
                     IF( M.GT.CRANK )
     $                  RESULT( 5 ) = ZQRT17( 'No transpose', 1, M, N,
     $                                NRHS, COPYA, LDA, B, LDB, COPYB,
     $                                LDB, C, WORK, LWORK )
*
*                    Test 6:  Check if x is in the rowspace of A
*                             workspace: (M+NRHS)*(N+2)
*
                     RESULT( 6 ) = ZERO
*
                     IF( N.GT.CRANK )
     $                  RESULT( 6 ) = ZQRT14( 'No transpose', M, N,
     $                                NRHS, COPYA, LDA, B, LDB, WORK,
     $                                LWORK )
*
*                    Print information about the tests that did not
*                    pass the threshold.
*
                     DO 60 K = 3, 6
                        IF( RESULT( K ).GE.THRESH ) THEN
                           IF( NFAIL.EQ.0 .AND. NERRS.EQ.0 )
     $                        CALL ALAHD( NOUT, PATH )
                           WRITE( NOUT, FMT = 9998 )M, N, NRHS, 0,
     $                        ITYPE, K, RESULT( K )
                           NFAIL = NFAIL + 1
                        END IF
   60                CONTINUE
                     NRUN = NRUN + 4
*
*                    Loop for testing different block sizes.
*
                     DO 90 INB = 1, NNB
                        NB = NBVAL( INB )
                        CALL XLAENV( 1, NB )
                        CALL XLAENV( 3, NXVAL( INB ) )
*
*                       Test ZGELSY
*
*                       ZGELSY:  Compute the minimum-norm solution
*                       X to min( norm( A * X - B ) )
*                       using the rank-revealing orthogonal
*                       factorization.
*
                        CALL ZLACPY( 'Full', M, N, COPYA, LDA, A, LDA )
                        CALL ZLACPY( 'Full', M, NRHS, COPYB, LDB, B,
     $                               LDB )
*
*                       Initialize vector IWORK.
*
                        DO 70 J = 1, N
                           IWORK( J ) = 0
   70                   CONTINUE
*
*                       Set LWLSY to the adequate value.
*
                        LWLSY = MNMIN + MAX( 2*MNMIN, NB*( N+1 ),
     $                          MNMIN+NB*NRHS )
                        LWLSY = MAX( 1, LWLSY )
*
                        SRNAMT = 'ZGELSY'
                        CALL ZGELSY( M, N, NRHS, A, LDA, B, LDB, IWORK,
     $                               RCOND, CRANK, WORK, LWLSY, RWORK,
     $                               INFO )
                        IF( INFO.NE.0 )
     $                     CALL ALAERH( PATH, 'ZGELSY', INFO, 0, ' ', M,
     $                                  N, NRHS, -1, NB, ITYPE, NFAIL,
     $                                  NERRS, NOUT )
*
*                       workspace used: 2*MNMIN+NB*NB+NB*MAX(N,NRHS)
*
*                       Test 7:  Compute relative error in svd
*                                workspace: M*N + 4*MIN(M,N) + MAX(M,N)
*
                        RESULT( 7 ) = ZQRT12( CRANK, CRANK, A, LDA,
     $                                COPYS, WORK, LWORK, RWORK )
*
*                       Test 8:  Compute error in solution
*                                workspace:  M*NRHS + M
*
                        CALL ZLACPY( 'Full', M, NRHS, COPYB, LDB, WORK,
     $                               LDWORK )
                        CALL ZQRT16( 'No transpose', M, N, NRHS, COPYA,
     $                               LDA, B, LDB, WORK, LDWORK, RWORK,
     $                               RESULT( 8 ) )
*
*                       Test 9:  Check norm of r'*A
*                                workspace: NRHS*(M+N)
*
                        RESULT( 9 ) = ZERO
                        IF( M.GT.CRANK )
     $                     RESULT( 9 ) = ZQRT17( 'No transpose', 1, M,
     $                                   N, NRHS, COPYA, LDA, B, LDB,
     $                                   COPYB, LDB, C, WORK, LWORK )
*
*                       Test 10:  Check if x is in the rowspace of A
*                                workspace: (M+NRHS)*(N+2)
*
                        RESULT( 10 ) = ZERO
*
                        IF( N.GT.CRANK )
     $                     RESULT( 10 ) = ZQRT14( 'No transpose', M, N,
     $                                    NRHS, COPYA, LDA, B, LDB,
     $                                    WORK, LWORK )
*
*                       Test ZGELSS
*
*                       ZGELSS:  Compute the minimum-norm solution
*                       X to min( norm( A * X - B ) )
*                       using the SVD.
*
                        CALL ZLACPY( 'Full', M, N, COPYA, LDA, A, LDA )
                        CALL ZLACPY( 'Full', M, NRHS, COPYB, LDB, B,
     $                               LDB )
                        SRNAMT = 'ZGELSS'
                        CALL ZGELSS( M, N, NRHS, A, LDA, B, LDB, S,
     $                               RCOND, CRANK, WORK, LWORK, RWORK,
     $                               INFO )
*
                        IF( INFO.NE.0 )
     $                     CALL ALAERH( PATH, 'ZGELSS', INFO, 0, ' ', M,
     $                                  N, NRHS, -1, NB, ITYPE, NFAIL,
     $                                  NERRS, NOUT )
*
*                       workspace used: 3*min(m,n) +
*                                       max(2*min(m,n),nrhs,max(m,n))
*
*                       Test 11:  Compute relative error in svd
*
                        IF( RANK.GT.0 ) THEN
                           CALL DAXPY( MNMIN, -ONE, COPYS, 1, S, 1 )
                           RESULT( 11 ) = DASUM( MNMIN, S, 1 ) /
     $                                    DASUM( MNMIN, COPYS, 1 ) /
     $                                    ( EPS*DBLE( MNMIN ) )
                        ELSE
                           RESULT( 11 ) = ZERO
                        END IF
*
*                       Test 12:  Compute error in solution
*
                        CALL ZLACPY( 'Full', M, NRHS, COPYB, LDB, WORK,
     $                               LDWORK )
                        CALL ZQRT16( 'No transpose', M, N, NRHS, COPYA,
     $                               LDA, B, LDB, WORK, LDWORK, RWORK,
     $                               RESULT( 12 ) )
*
*                       Test 13:  Check norm of r'*A
*
                        RESULT( 13 ) = ZERO
                        IF( M.GT.CRANK )
     $                     RESULT( 13 ) = ZQRT17( 'No transpose', 1, M,
     $                                    N, NRHS, COPYA, LDA, B, LDB,
     $                                    COPYB, LDB, C, WORK, LWORK )
*
*                       Test 14:  Check if x is in the rowspace of A
*
                        RESULT( 14 ) = ZERO
                        IF( N.GT.CRANK )
     $                     RESULT( 14 ) = ZQRT14( 'No transpose', M, N,
     $                                    NRHS, COPYA, LDA, B, LDB,
     $                                    WORK, LWORK )
*
*                       Test ZGELSD
*
*                       ZGELSD:  Compute the minimum-norm solution X
*                       to min( norm( A * X - B ) ) using a
*                       divide and conquer SVD.
*
                        CALL XLAENV( 9, 25 )
*
                        CALL ZLACPY( 'Full', M, N, COPYA, LDA, A, LDA )
                        CALL ZLACPY( 'Full', M, NRHS, COPYB, LDB, B,
     $                               LDB )
*
                        SRNAMT = 'ZGELSD'
                        CALL ZGELSD( M, N, NRHS, A, LDA, B, LDB, S,
     $                               RCOND, CRANK, WORK, LWORK, RWORK,
     $                               IWORK, INFO )
                        IF( INFO.NE.0 )
     $                     CALL ALAERH( PATH, 'ZGELSD', INFO, 0, ' ', M,
     $                                  N, NRHS, -1, NB, ITYPE, NFAIL,
     $                                  NERRS, NOUT )
*
*                       Test 15:  Compute relative error in svd
*
                        IF( RANK.GT.0 ) THEN
                           CALL DAXPY( MNMIN, -ONE, COPYS, 1, S, 1 )
                           RESULT( 15 ) = DASUM( MNMIN, S, 1 ) /
     $                                    DASUM( MNMIN, COPYS, 1 ) /
     $                                    ( EPS*DBLE( MNMIN ) )
                        ELSE
                           RESULT( 15 ) = ZERO
                        END IF
*
*                       Test 16:  Compute error in solution
*
                        CALL ZLACPY( 'Full', M, NRHS, COPYB, LDB, WORK,
     $                               LDWORK )
                        CALL ZQRT16( 'No transpose', M, N, NRHS, COPYA,
     $                               LDA, B, LDB, WORK, LDWORK, RWORK,
     $                               RESULT( 16 ) )
*
*                       Test 17:  Check norm of r'*A
*
                        RESULT( 17 ) = ZERO
                        IF( M.GT.CRANK )
     $                     RESULT( 17 ) = ZQRT17( 'No transpose', 1, M,
     $                                    N, NRHS, COPYA, LDA, B, LDB,
     $                                    COPYB, LDB, C, WORK, LWORK )
*
*                       Test 18:  Check if x is in the rowspace of A
*
                        RESULT( 18 ) = ZERO
                        IF( N.GT.CRANK )
     $                     RESULT( 18 ) = ZQRT14( 'No transpose', M, N,
     $                                    NRHS, COPYA, LDA, B, LDB,
     $                                    WORK, LWORK )
*
*                       Print information about the tests that did not
*                       pass the threshold.
*
                        DO 80 K = 7, NTESTS
                           IF( RESULT( K ).GE.THRESH ) THEN
                              IF( NFAIL.EQ.0 .AND. NERRS.EQ.0 )
     $                           CALL ALAHD( NOUT, PATH )
                              WRITE( NOUT, FMT = 9998 )M, N, NRHS, NB,
     $                           ITYPE, K, RESULT( K )
                              NFAIL = NFAIL + 1
                           END IF
   80                   CONTINUE
                        NRUN = NRUN + 12
*
   90                CONTINUE
  100             CONTINUE
  110          CONTINUE
  120       CONTINUE
  130    CONTINUE
  140 CONTINUE
*
*     Print a summary of the results.
*
      CALL ALASVM( PATH, NOUT, NFAIL, NRUN, NERRS )
*
 9999 FORMAT( ' TRANS=''', A1, ''', M=', I5, ', N=', I5, ', NRHS=', I4,
     $      ', NB=', I4, ', type', I2, ', test(', I2, ')=', G12.5 )
 9998 FORMAT( ' M=', I5, ', N=', I5, ', NRHS=', I4, ', NB=', I4,
     $      ', type', I2, ', test(', I2, ')=', G12.5 )
      RETURN
*
*     End of ZDRVLS
*
      END
