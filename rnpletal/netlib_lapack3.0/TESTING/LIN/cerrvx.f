      SUBROUTINE CERRVX( PATH, NUNIT )
*
*  -- LAPACK test routine (version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     October 31, 1992
*
*     .. Scalar Arguments ..
      CHARACTER*3        PATH
      INTEGER            NUNIT
*     ..
*
*  Purpose
*  =======
*
*  CERRVX tests the error exits for the COMPLEX driver routines
*  for solving linear systems of equations.
*
*  Arguments
*  =========
*
*  PATH    (input) CHARACTER*3
*          The LAPACK path name for the routines to be tested.
*
*  NUNIT   (input) INTEGER
*          The unit number for output.
*
*  =====================================================================
*
*     .. Parameters ..
      INTEGER            NMAX
      PARAMETER          ( NMAX = 4 )
*     ..
*     .. Local Scalars ..
      CHARACTER          EQ
      CHARACTER*2        C2
      INTEGER            I, INFO, J
      REAL               RCOND
*     ..
*     .. Local Arrays ..
      INTEGER            IP( NMAX )
      REAL               C( NMAX ), R( NMAX ), R1( NMAX ), R2( NMAX ),
     $                   RF( NMAX ), RW( NMAX )
      COMPLEX            A( NMAX, NMAX ), AF( NMAX, NMAX ), B( NMAX ),
     $                   W( 2*NMAX ), X( NMAX )
*     ..
*     .. External Functions ..
      LOGICAL            LSAMEN
      EXTERNAL           LSAMEN
*     ..
*     .. External Subroutines ..
      EXTERNAL           CGBSV, CGBSVX, CGESV, CGESVX, CGTSV, CGTSVX,
     $                   CHESV, CHESVX, CHKXER, CHPSV, CHPSVX, CPBSV,
     $                   CPBSVX, CPOSV, CPOSVX, CPPSV, CPPSVX, CPTSV,
     $                   CPTSVX, CSPSV, CSPSVX, CSYSV, CSYSVX
*     ..
*     .. Scalars in Common ..
      LOGICAL            LERR, OK
      CHARACTER*6        SRNAMT
      INTEGER            INFOT, NOUT
*     ..
*     .. Common blocks ..
      COMMON             / INFOC / INFOT, NOUT, OK, LERR
      COMMON             / SRNAMC / SRNAMT
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          CMPLX, REAL
*     ..
*     .. Executable Statements ..
*
      NOUT = NUNIT
      WRITE( NOUT, FMT = * )
      C2 = PATH( 2: 3 )
*
*     Set the variables to innocuous values.
*
      DO 20 J = 1, NMAX
         DO 10 I = 1, NMAX
            A( I, J ) = CMPLX( 1. / REAL( I+J ), -1. / REAL( I+J ) )
            AF( I, J ) = CMPLX( 1. / REAL( I+J ), -1. / REAL( I+J ) )
   10    CONTINUE
         B( J ) = 0.
         R1( J ) = 0.
         R2( J ) = 0.
         W( J ) = 0.
         X( J ) = 0.
         C( J ) = 0.
         R( J ) = 0.
         IP( J ) = J
   20 CONTINUE
      OK = .TRUE.
*
      IF( LSAMEN( 2, C2, 'GE' ) ) THEN
*
*        CGESV
*
         SRNAMT = 'CGESV '
         INFOT = 1
         CALL CGESV( -1, 0, A, 1, IP, B, 1, INFO )
         CALL CHKXER( 'CGESV ', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL CGESV( 0, -1, A, 1, IP, B, 1, INFO )
         CALL CHKXER( 'CGESV ', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL CGESV( 2, 1, A, 1, IP, B, 2, INFO )
         CALL CHKXER( 'CGESV ', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL CGESV( 2, 1, A, 2, IP, B, 1, INFO )
         CALL CHKXER( 'CGESV ', INFOT, NOUT, LERR, OK )
*
*        CGESVX
*
         SRNAMT = 'CGESVX'
         INFOT = 1
         CALL CGESVX( '/', 'N', 0, 0, A, 1, AF, 1, IP, EQ, R, C, B, 1,
     $                X, 1, RCOND, R1, R2, W, RW, INFO )
         CALL CHKXER( 'CGESVX', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL CGESVX( 'N', '/', 0, 0, A, 1, AF, 1, IP, EQ, R, C, B, 1,
     $                X, 1, RCOND, R1, R2, W, RW, INFO )
         CALL CHKXER( 'CGESVX', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL CGESVX( 'N', 'N', -1, 0, A, 1, AF, 1, IP, EQ, R, C, B, 1,
     $                X, 1, RCOND, R1, R2, W, RW, INFO )
         CALL CHKXER( 'CGESVX', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL CGESVX( 'N', 'N', 0, -1, A, 1, AF, 1, IP, EQ, R, C, B, 1,
     $                X, 1, RCOND, R1, R2, W, RW, INFO )
         CALL CHKXER( 'CGESVX', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL CGESVX( 'N', 'N', 2, 1, A, 1, AF, 2, IP, EQ, R, C, B, 2,
     $                X, 2, RCOND, R1, R2, W, RW, INFO )
         CALL CHKXER( 'CGESVX', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL CGESVX( 'N', 'N', 2, 1, A, 2, AF, 1, IP, EQ, R, C, B, 2,
     $                X, 2, RCOND, R1, R2, W, RW, INFO )
         CALL CHKXER( 'CGESVX', INFOT, NOUT, LERR, OK )
         INFOT = 10
         EQ = '/'
         CALL CGESVX( 'F', 'N', 0, 0, A, 1, AF, 1, IP, EQ, R, C, B, 1,
     $                X, 1, RCOND, R1, R2, W, RW, INFO )
         CALL CHKXER( 'CGESVX', INFOT, NOUT, LERR, OK )
         INFOT = 11
         EQ = 'R'
         CALL CGESVX( 'F', 'N', 1, 0, A, 1, AF, 1, IP, EQ, R, C, B, 1,
     $                X, 1, RCOND, R1, R2, W, RW, INFO )
         CALL CHKXER( 'CGESVX', INFOT, NOUT, LERR, OK )
         INFOT = 12
         EQ = 'C'
         CALL CGESVX( 'F', 'N', 1, 0, A, 1, AF, 1, IP, EQ, R, C, B, 1,
     $                X, 1, RCOND, R1, R2, W, RW, INFO )
         CALL CHKXER( 'CGESVX', INFOT, NOUT, LERR, OK )
         INFOT = 14
         CALL CGESVX( 'N', 'N', 2, 1, A, 2, AF, 2, IP, EQ, R, C, B, 1,
     $                X, 2, RCOND, R1, R2, W, RW, INFO )
         CALL CHKXER( 'CGESVX', INFOT, NOUT, LERR, OK )
         INFOT = 16
         CALL CGESVX( 'N', 'N', 2, 1, A, 2, AF, 2, IP, EQ, R, C, B, 2,
     $                X, 1, RCOND, R1, R2, W, RW, INFO )
         CALL CHKXER( 'CGESVX', INFOT, NOUT, LERR, OK )
*
      ELSE IF( LSAMEN( 2, C2, 'GB' ) ) THEN
*
*        CGBSV
*
         SRNAMT = 'CGBSV '
         INFOT = 1
         CALL CGBSV( -1, 0, 0, 0, A, 1, IP, B, 1, INFO )
         CALL CHKXER( 'CGBSV ', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL CGBSV( 1, -1, 0, 0, A, 1, IP, B, 1, INFO )
         CALL CHKXER( 'CGBSV ', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL CGBSV( 1, 0, -1, 0, A, 1, IP, B, 1, INFO )
         CALL CHKXER( 'CGBSV ', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL CGBSV( 0, 0, 0, -1, A, 1, IP, B, 1, INFO )
         CALL CHKXER( 'CGBSV ', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL CGBSV( 1, 1, 1, 0, A, 3, IP, B, 1, INFO )
         CALL CHKXER( 'CGBSV ', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL CGBSV( 2, 0, 0, 0, A, 1, IP, B, 1, INFO )
         CALL CHKXER( 'CGBSV ', INFOT, NOUT, LERR, OK )
*
*        CGBSVX
*
         SRNAMT = 'CGBSVX'
         INFOT = 1
         CALL CGBSVX( '/', 'N', 0, 0, 0, 0, A, 1, AF, 1, IP, EQ, R, C,
     $                B, 1, X, 1, RCOND, R1, R2, W, RW, INFO )
         CALL CHKXER( 'CGBSVX', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL CGBSVX( 'N', '/', 0, 0, 0, 0, A, 1, AF, 1, IP, EQ, R, C,
     $                B, 1, X, 1, RCOND, R1, R2, W, RW, INFO )
         CALL CHKXER( 'CGBSVX', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL CGBSVX( 'N', 'N', -1, 0, 0, 0, A, 1, AF, 1, IP, EQ, R, C,
     $                B, 1, X, 1, RCOND, R1, R2, W, RW, INFO )
         CALL CHKXER( 'CGBSVX', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL CGBSVX( 'N', 'N', 1, -1, 0, 0, A, 1, AF, 1, IP, EQ, R, C,
     $                B, 1, X, 1, RCOND, R1, R2, W, RW, INFO )
         CALL CHKXER( 'CGBSVX', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL CGBSVX( 'N', 'N', 1, 0, -1, 0, A, 1, AF, 1, IP, EQ, R, C,
     $                B, 1, X, 1, RCOND, R1, R2, W, RW, INFO )
         CALL CHKXER( 'CGBSVX', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL CGBSVX( 'N', 'N', 0, 0, 0, -1, A, 1, AF, 1, IP, EQ, R, C,
     $                B, 1, X, 1, RCOND, R1, R2, W, RW, INFO )
         CALL CHKXER( 'CGBSVX', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL CGBSVX( 'N', 'N', 1, 1, 1, 0, A, 2, AF, 4, IP, EQ, R, C,
     $                B, 1, X, 1, RCOND, R1, R2, W, RW, INFO )
         CALL CHKXER( 'CGBSVX', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL CGBSVX( 'N', 'N', 1, 1, 1, 0, A, 3, AF, 3, IP, EQ, R, C,
     $                B, 1, X, 1, RCOND, R1, R2, W, RW, INFO )
         CALL CHKXER( 'CGBSVX', INFOT, NOUT, LERR, OK )
         INFOT = 12
         EQ = '/'
         CALL CGBSVX( 'F', 'N', 0, 0, 0, 0, A, 1, AF, 1, IP, EQ, R, C,
     $                B, 1, X, 1, RCOND, R1, R2, W, RW, INFO )
         CALL CHKXER( 'CGBSVX', INFOT, NOUT, LERR, OK )
         INFOT = 13
         EQ = 'R'
         CALL CGBSVX( 'F', 'N', 1, 0, 0, 0, A, 1, AF, 1, IP, EQ, R, C,
     $                B, 1, X, 1, RCOND, R1, R2, W, RW, INFO )
         CALL CHKXER( 'CGBSVX', INFOT, NOUT, LERR, OK )
         INFOT = 14
         EQ = 'C'
         CALL CGBSVX( 'F', 'N', 1, 0, 0, 0, A, 1, AF, 1, IP, EQ, R, C,
     $                B, 1, X, 1, RCOND, R1, R2, W, RW, INFO )
         CALL CHKXER( 'CGBSVX', INFOT, NOUT, LERR, OK )
         INFOT = 16
         CALL CGBSVX( 'N', 'N', 2, 0, 0, 0, A, 1, AF, 1, IP, EQ, R, C,
     $                B, 1, X, 2, RCOND, R1, R2, W, RW, INFO )
         CALL CHKXER( 'CGBSVX', INFOT, NOUT, LERR, OK )
         INFOT = 18
         CALL CGBSVX( 'N', 'N', 2, 0, 0, 0, A, 1, AF, 1, IP, EQ, R, C,
     $                B, 2, X, 1, RCOND, R1, R2, W, RW, INFO )
         CALL CHKXER( 'CGBSVX', INFOT, NOUT, LERR, OK )
*
      ELSE IF( LSAMEN( 2, C2, 'GT' ) ) THEN
*
*        CGTSV
*
         SRNAMT = 'CGTSV '
         INFOT = 1
         CALL CGTSV( -1, 0, A( 1, 1 ), A( 1, 2 ), A( 1, 3 ), B, 1,
     $               INFO )
         CALL CHKXER( 'CGTSV ', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL CGTSV( 0, -1, A( 1, 1 ), A( 1, 2 ), A( 1, 3 ), B, 1,
     $               INFO )
         CALL CHKXER( 'CGTSV ', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL CGTSV( 2, 0, A( 1, 1 ), A( 1, 2 ), A( 1, 3 ), B, 1, INFO )
         CALL CHKXER( 'CGTSV ', INFOT, NOUT, LERR, OK )
*
*        CGTSVX
*
         SRNAMT = 'CGTSVX'
         INFOT = 1
         CALL CGTSVX( '/', 'N', 0, 0, A( 1, 1 ), A( 1, 2 ), A( 1, 3 ),
     $                AF( 1, 1 ), AF( 1, 2 ), AF( 1, 3 ), AF( 1, 4 ),
     $                IP, B, 1, X, 1, RCOND, R1, R2, W, RW, INFO )
         CALL CHKXER( 'CGTSVX', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL CGTSVX( 'N', '/', 0, 0, A( 1, 1 ), A( 1, 2 ), A( 1, 3 ),
     $                AF( 1, 1 ), AF( 1, 2 ), AF( 1, 3 ), AF( 1, 4 ),
     $                IP, B, 1, X, 1, RCOND, R1, R2, W, RW, INFO )
         CALL CHKXER( 'CGTSVX', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL CGTSVX( 'N', 'N', -1, 0, A( 1, 1 ), A( 1, 2 ), A( 1, 3 ),
     $                AF( 1, 1 ), AF( 1, 2 ), AF( 1, 3 ), AF( 1, 4 ),
     $                IP, B, 1, X, 1, RCOND, R1, R2, W, RW, INFO )
         CALL CHKXER( 'CGTSVX', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL CGTSVX( 'N', 'N', 0, -1, A( 1, 1 ), A( 1, 2 ), A( 1, 3 ),
     $                AF( 1, 1 ), AF( 1, 2 ), AF( 1, 3 ), AF( 1, 4 ),
     $                IP, B, 1, X, 1, RCOND, R1, R2, W, RW, INFO )
         CALL CHKXER( 'CGTSVX', INFOT, NOUT, LERR, OK )
         INFOT = 14
         CALL CGTSVX( 'N', 'N', 2, 0, A( 1, 1 ), A( 1, 2 ), A( 1, 3 ),
     $                AF( 1, 1 ), AF( 1, 2 ), AF( 1, 3 ), AF( 1, 4 ),
     $                IP, B, 1, X, 2, RCOND, R1, R2, W, RW, INFO )
         CALL CHKXER( 'CGTSVX', INFOT, NOUT, LERR, OK )
         INFOT = 16
         CALL CGTSVX( 'N', 'N', 2, 0, A( 1, 1 ), A( 1, 2 ), A( 1, 3 ),
     $                AF( 1, 1 ), AF( 1, 2 ), AF( 1, 3 ), AF( 1, 4 ),
     $                IP, B, 2, X, 1, RCOND, R1, R2, W, RW, INFO )
         CALL CHKXER( 'CGTSVX', INFOT, NOUT, LERR, OK )
*
      ELSE IF( LSAMEN( 2, C2, 'PO' ) ) THEN
*
*        CPOSV
*
         SRNAMT = 'CPOSV '
         INFOT = 1
         CALL CPOSV( '/', 0, 0, A, 1, B, 1, INFO )
         CALL CHKXER( 'CPOSV ', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL CPOSV( 'U', -1, 0, A, 1, B, 1, INFO )
         CALL CHKXER( 'CPOSV ', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL CPOSV( 'U', 0, -1, A, 1, B, 1, INFO )
         CALL CHKXER( 'CPOSV ', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL CPOSV( 'U', 2, 0, A, 1, B, 2, INFO )
         CALL CHKXER( 'CPOSV ', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL CPOSV( 'U', 2, 0, A, 2, B, 1, INFO )
         CALL CHKXER( 'CPOSV ', INFOT, NOUT, LERR, OK )
*
*        CPOSVX
*
         SRNAMT = 'CPOSVX'
         INFOT = 1
         CALL CPOSVX( '/', 'U', 0, 0, A, 1, AF, 1, EQ, C, B, 1, X, 1,
     $                RCOND, R1, R2, W, RW, INFO )
         CALL CHKXER( 'CPOSVX', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL CPOSVX( 'N', '/', 0, 0, A, 1, AF, 1, EQ, C, B, 1, X, 1,
     $                RCOND, R1, R2, W, RW, INFO )
         CALL CHKXER( 'CPOSVX', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL CPOSVX( 'N', 'U', -1, 0, A, 1, AF, 1, EQ, C, B, 1, X, 1,
     $                RCOND, R1, R2, W, RW, INFO )
         CALL CHKXER( 'CPOSVX', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL CPOSVX( 'N', 'U', 0, -1, A, 1, AF, 1, EQ, C, B, 1, X, 1,
     $                RCOND, R1, R2, W, RW, INFO )
         CALL CHKXER( 'CPOSVX', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL CPOSVX( 'N', 'U', 2, 0, A, 1, AF, 2, EQ, C, B, 2, X, 2,
     $                RCOND, R1, R2, W, RW, INFO )
         CALL CHKXER( 'CPOSVX', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL CPOSVX( 'N', 'U', 2, 0, A, 2, AF, 1, EQ, C, B, 2, X, 2,
     $                RCOND, R1, R2, W, RW, INFO )
         CALL CHKXER( 'CPOSVX', INFOT, NOUT, LERR, OK )
         INFOT = 9
         EQ = '/'
         CALL CPOSVX( 'F', 'U', 0, 0, A, 1, AF, 1, EQ, C, B, 1, X, 1,
     $                RCOND, R1, R2, W, RW, INFO )
         CALL CHKXER( 'CPOSVX', INFOT, NOUT, LERR, OK )
         INFOT = 10
         EQ = 'Y'
         CALL CPOSVX( 'F', 'U', 1, 0, A, 1, AF, 1, EQ, C, B, 1, X, 1,
     $                RCOND, R1, R2, W, RW, INFO )
         CALL CHKXER( 'CPOSVX', INFOT, NOUT, LERR, OK )
         INFOT = 12
         CALL CPOSVX( 'N', 'U', 2, 0, A, 2, AF, 2, EQ, C, B, 1, X, 2,
     $                RCOND, R1, R2, W, RW, INFO )
         CALL CHKXER( 'CPOSVX', INFOT, NOUT, LERR, OK )
         INFOT = 14
         CALL CPOSVX( 'N', 'U', 2, 0, A, 2, AF, 2, EQ, C, B, 2, X, 1,
     $                RCOND, R1, R2, W, RW, INFO )
         CALL CHKXER( 'CPOSVX', INFOT, NOUT, LERR, OK )
*
      ELSE IF( LSAMEN( 2, C2, 'PP' ) ) THEN
*
*        CPPSV
*
         SRNAMT = 'CPPSV '
         INFOT = 1
         CALL CPPSV( '/', 0, 0, A, B, 1, INFO )
         CALL CHKXER( 'CPPSV ', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL CPPSV( 'U', -1, 0, A, B, 1, INFO )
         CALL CHKXER( 'CPPSV ', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL CPPSV( 'U', 0, -1, A, B, 1, INFO )
         CALL CHKXER( 'CPPSV ', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL CPPSV( 'U', 2, 0, A, B, 1, INFO )
         CALL CHKXER( 'CPPSV ', INFOT, NOUT, LERR, OK )
*
*        CPPSVX
*
         SRNAMT = 'CPPSVX'
         INFOT = 1
         CALL CPPSVX( '/', 'U', 0, 0, A, AF, EQ, C, B, 1, X, 1, RCOND,
     $                R1, R2, W, RW, INFO )
         CALL CHKXER( 'CPPSVX', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL CPPSVX( 'N', '/', 0, 0, A, AF, EQ, C, B, 1, X, 1, RCOND,
     $                R1, R2, W, RW, INFO )
         CALL CHKXER( 'CPPSVX', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL CPPSVX( 'N', 'U', -1, 0, A, AF, EQ, C, B, 1, X, 1, RCOND,
     $                R1, R2, W, RW, INFO )
         CALL CHKXER( 'CPPSVX', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL CPPSVX( 'N', 'U', 0, -1, A, AF, EQ, C, B, 1, X, 1, RCOND,
     $                R1, R2, W, RW, INFO )
         CALL CHKXER( 'CPPSVX', INFOT, NOUT, LERR, OK )
         INFOT = 7
         EQ = '/'
         CALL CPPSVX( 'F', 'U', 0, 0, A, AF, EQ, C, B, 1, X, 1, RCOND,
     $                R1, R2, W, RW, INFO )
         CALL CHKXER( 'CPPSVX', INFOT, NOUT, LERR, OK )
         INFOT = 8
         EQ = 'Y'
         CALL CPPSVX( 'F', 'U', 1, 0, A, AF, EQ, C, B, 1, X, 1, RCOND,
     $                R1, R2, W, RW, INFO )
         CALL CHKXER( 'CPPSVX', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL CPPSVX( 'N', 'U', 2, 0, A, AF, EQ, C, B, 1, X, 2, RCOND,
     $                R1, R2, W, RW, INFO )
         CALL CHKXER( 'CPPSVX', INFOT, NOUT, LERR, OK )
         INFOT = 12
         CALL CPPSVX( 'N', 'U', 2, 0, A, AF, EQ, C, B, 2, X, 1, RCOND,
     $                R1, R2, W, RW, INFO )
         CALL CHKXER( 'CPPSVX', INFOT, NOUT, LERR, OK )
*
      ELSE IF( LSAMEN( 2, C2, 'PB' ) ) THEN
*
*        CPBSV
*
         SRNAMT = 'CPBSV '
         INFOT = 1
         CALL CPBSV( '/', 0, 0, 0, A, 1, B, 1, INFO )
         CALL CHKXER( 'CPBSV ', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL CPBSV( 'U', -1, 0, 0, A, 1, B, 1, INFO )
         CALL CHKXER( 'CPBSV ', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL CPBSV( 'U', 1, -1, 0, A, 1, B, 1, INFO )
         CALL CHKXER( 'CPBSV ', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL CPBSV( 'U', 0, 0, -1, A, 1, B, 1, INFO )
         CALL CHKXER( 'CPBSV ', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL CPBSV( 'U', 1, 1, 0, A, 1, B, 2, INFO )
         CALL CHKXER( 'CPBSV ', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL CPBSV( 'U', 2, 0, 0, A, 1, B, 1, INFO )
         CALL CHKXER( 'CPBSV ', INFOT, NOUT, LERR, OK )
*
*        CPBSVX
*
         SRNAMT = 'CPBSVX'
         INFOT = 1
         CALL CPBSVX( '/', 'U', 0, 0, 0, A, 1, AF, 1, EQ, C, B, 1, X, 1,
     $                RCOND, R1, R2, W, RW, INFO )
         CALL CHKXER( 'CPBSVX', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL CPBSVX( 'N', '/', 0, 0, 0, A, 1, AF, 1, EQ, C, B, 1, X, 1,
     $                RCOND, R1, R2, W, RW, INFO )
         CALL CHKXER( 'CPBSVX', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL CPBSVX( 'N', 'U', -1, 0, 0, A, 1, AF, 1, EQ, C, B, 1, X,
     $                1, RCOND, R1, R2, W, RW, INFO )
         CALL CHKXER( 'CPBSVX', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL CPBSVX( 'N', 'U', 1, -1, 0, A, 1, AF, 1, EQ, C, B, 1, X,
     $                1, RCOND, R1, R2, W, RW, INFO )
         CALL CHKXER( 'CPBSVX', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL CPBSVX( 'N', 'U', 0, 0, -1, A, 1, AF, 1, EQ, C, B, 1, X,
     $                1, RCOND, R1, R2, W, RW, INFO )
         CALL CHKXER( 'CPBSVX', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL CPBSVX( 'N', 'U', 1, 1, 0, A, 1, AF, 2, EQ, C, B, 2, X, 2,
     $                RCOND, R1, R2, W, RW, INFO )
         CALL CHKXER( 'CPBSVX', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL CPBSVX( 'N', 'U', 1, 1, 0, A, 2, AF, 1, EQ, C, B, 2, X, 2,
     $                RCOND, R1, R2, W, RW, INFO )
         CALL CHKXER( 'CPBSVX', INFOT, NOUT, LERR, OK )
         INFOT = 10
         EQ = '/'
         CALL CPBSVX( 'F', 'U', 0, 0, 0, A, 1, AF, 1, EQ, C, B, 1, X, 1,
     $                RCOND, R1, R2, W, RW, INFO )
         CALL CHKXER( 'CPBSVX', INFOT, NOUT, LERR, OK )
         INFOT = 11
         EQ = 'Y'
         CALL CPBSVX( 'F', 'U', 1, 0, 0, A, 1, AF, 1, EQ, C, B, 1, X, 1,
     $                RCOND, R1, R2, W, RW, INFO )
         CALL CHKXER( 'CPBSVX', INFOT, NOUT, LERR, OK )
         INFOT = 13
         CALL CPBSVX( 'N', 'U', 2, 0, 0, A, 1, AF, 1, EQ, C, B, 1, X, 2,
     $                RCOND, R1, R2, W, RW, INFO )
         CALL CHKXER( 'CPBSVX', INFOT, NOUT, LERR, OK )
         INFOT = 15
         CALL CPBSVX( 'N', 'U', 2, 0, 0, A, 1, AF, 1, EQ, C, B, 2, X, 1,
     $                RCOND, R1, R2, W, RW, INFO )
         CALL CHKXER( 'CPBSVX', INFOT, NOUT, LERR, OK )
*
      ELSE IF( LSAMEN( 2, C2, 'PT' ) ) THEN
*
*        CPTSV
*
         SRNAMT = 'CPTSV '
         INFOT = 1
         CALL CPTSV( -1, 0, R, A( 1, 1 ), B, 1, INFO )
         CALL CHKXER( 'CPTSV ', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL CPTSV( 0, -1, R, A( 1, 1 ), B, 1, INFO )
         CALL CHKXER( 'CPTSV ', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL CPTSV( 2, 0, R, A( 1, 1 ), B, 1, INFO )
         CALL CHKXER( 'CPTSV ', INFOT, NOUT, LERR, OK )
*
*        CPTSVX
*
         SRNAMT = 'CPTSVX'
         INFOT = 1
         CALL CPTSVX( '/', 0, 0, R, A( 1, 1 ), RF, AF( 1, 1 ), B, 1, X,
     $                1, RCOND, R1, R2, W, RW, INFO )
         CALL CHKXER( 'CPTSVX', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL CPTSVX( 'N', -1, 0, R, A( 1, 1 ), RF, AF( 1, 1 ), B, 1, X,
     $                1, RCOND, R1, R2, W, RW, INFO )
         CALL CHKXER( 'CPTSVX', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL CPTSVX( 'N', 0, -1, R, A( 1, 1 ), RF, AF( 1, 1 ), B, 1, X,
     $                1, RCOND, R1, R2, W, RW, INFO )
         CALL CHKXER( 'CPTSVX', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL CPTSVX( 'N', 2, 0, R, A( 1, 1 ), RF, AF( 1, 1 ), B, 1, X,
     $                2, RCOND, R1, R2, W, RW, INFO )
         CALL CHKXER( 'CPTSVX', INFOT, NOUT, LERR, OK )
         INFOT = 11
         CALL CPTSVX( 'N', 2, 0, R, A( 1, 1 ), RF, AF( 1, 1 ), B, 2, X,
     $                1, RCOND, R1, R2, W, RW, INFO )
         CALL CHKXER( 'CPTSVX', INFOT, NOUT, LERR, OK )
*
      ELSE IF( LSAMEN( 2, C2, 'HE' ) ) THEN
*
*        CHESV
*
         SRNAMT = 'CHESV '
         INFOT = 1
         CALL CHESV( '/', 0, 0, A, 1, IP, B, 1, W, 1, INFO )
         CALL CHKXER( 'CHESV ', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL CHESV( 'U', -1, 0, A, 1, IP, B, 1, W, 1, INFO )
         CALL CHKXER( 'CHESV ', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL CHESV( 'U', 0, -1, A, 1, IP, B, 1, W, 1, INFO )
         CALL CHKXER( 'CHESV ', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL CHESV( 'U', 2, 0, A, 1, IP, B, 2, W, 1, INFO )
         CALL CHKXER( 'CHESV ', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL CHESV( 'U', 2, 0, A, 2, IP, B, 1, W, 1, INFO )
         CALL CHKXER( 'CHESV ', INFOT, NOUT, LERR, OK )
*
*        CHESVX
*
         SRNAMT = 'CHESVX'
         INFOT = 1
         CALL CHESVX( '/', 'U', 0, 0, A, 1, AF, 1, IP, B, 1, X, 1,
     $                RCOND, R1, R2, W, 1, RW, INFO )
         CALL CHKXER( 'CHESVX', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL CHESVX( 'N', '/', 0, 0, A, 1, AF, 1, IP, B, 1, X, 1,
     $                RCOND, R1, R2, W, 1, RW, INFO )
         CALL CHKXER( 'CHESVX', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL CHESVX( 'N', 'U', -1, 0, A, 1, AF, 1, IP, B, 1, X, 1,
     $                RCOND, R1, R2, W, 1, RW, INFO )
         CALL CHKXER( 'CHESVX', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL CHESVX( 'N', 'U', 0, -1, A, 1, AF, 1, IP, B, 1, X, 1,
     $                RCOND, R1, R2, W, 1, RW, INFO )
         CALL CHKXER( 'CHESVX', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL CHESVX( 'N', 'U', 2, 0, A, 1, AF, 2, IP, B, 2, X, 2,
     $                RCOND, R1, R2, W, 4, RW, INFO )
         CALL CHKXER( 'CHESVX', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL CHESVX( 'N', 'U', 2, 0, A, 2, AF, 1, IP, B, 2, X, 2,
     $                RCOND, R1, R2, W, 4, RW, INFO )
         CALL CHKXER( 'CHESVX', INFOT, NOUT, LERR, OK )
         INFOT = 11
         CALL CHESVX( 'N', 'U', 2, 0, A, 2, AF, 2, IP, B, 1, X, 2,
     $                RCOND, R1, R2, W, 4, RW, INFO )
         CALL CHKXER( 'CHESVX', INFOT, NOUT, LERR, OK )
         INFOT = 13
         CALL CHESVX( 'N', 'U', 2, 0, A, 2, AF, 2, IP, B, 2, X, 1,
     $                RCOND, R1, R2, W, 4, RW, INFO )
         CALL CHKXER( 'CHESVX', INFOT, NOUT, LERR, OK )
         INFOT = 18
         CALL CHESVX( 'N', 'U', 2, 0, A, 2, AF, 2, IP, B, 2, X, 2,
     $                RCOND, R1, R2, W, 3, RW, INFO )
         CALL CHKXER( 'CHESVX', INFOT, NOUT, LERR, OK )
*
      ELSE IF( LSAMEN( 2, C2, 'HP' ) ) THEN
*
*        CHPSV
*
         SRNAMT = 'CHPSV '
         INFOT = 1
         CALL CHPSV( '/', 0, 0, A, IP, B, 1, INFO )
         CALL CHKXER( 'CHPSV ', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL CHPSV( 'U', -1, 0, A, IP, B, 1, INFO )
         CALL CHKXER( 'CHPSV ', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL CHPSV( 'U', 0, -1, A, IP, B, 1, INFO )
         CALL CHKXER( 'CHPSV ', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL CHPSV( 'U', 2, 0, A, IP, B, 1, INFO )
         CALL CHKXER( 'CHPSV ', INFOT, NOUT, LERR, OK )
*
*        CHPSVX
*
         SRNAMT = 'CHPSVX'
         INFOT = 1
         CALL CHPSVX( '/', 'U', 0, 0, A, AF, IP, B, 1, X, 1, RCOND, R1,
     $                R2, W, RW, INFO )
         CALL CHKXER( 'CHPSVX', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL CHPSVX( 'N', '/', 0, 0, A, AF, IP, B, 1, X, 1, RCOND, R1,
     $                R2, W, RW, INFO )
         CALL CHKXER( 'CHPSVX', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL CHPSVX( 'N', 'U', -1, 0, A, AF, IP, B, 1, X, 1, RCOND, R1,
     $                R2, W, RW, INFO )
         CALL CHKXER( 'CHPSVX', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL CHPSVX( 'N', 'U', 0, -1, A, AF, IP, B, 1, X, 1, RCOND, R1,
     $                R2, W, RW, INFO )
         CALL CHKXER( 'CHPSVX', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL CHPSVX( 'N', 'U', 2, 0, A, AF, IP, B, 1, X, 2, RCOND, R1,
     $                R2, W, RW, INFO )
         CALL CHKXER( 'CHPSVX', INFOT, NOUT, LERR, OK )
         INFOT = 11
         CALL CHPSVX( 'N', 'U', 2, 0, A, AF, IP, B, 2, X, 1, RCOND, R1,
     $                R2, W, RW, INFO )
         CALL CHKXER( 'CHPSVX', INFOT, NOUT, LERR, OK )
*
      ELSE IF( LSAMEN( 2, C2, 'SY' ) ) THEN
*
*        CSYSV
*
         SRNAMT = 'CSYSV '
         INFOT = 1
         CALL CSYSV( '/', 0, 0, A, 1, IP, B, 1, W, 1, INFO )
         CALL CHKXER( 'CSYSV ', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL CSYSV( 'U', -1, 0, A, 1, IP, B, 1, W, 1, INFO )
         CALL CHKXER( 'CSYSV ', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL CSYSV( 'U', 0, -1, A, 1, IP, B, 1, W, 1, INFO )
         CALL CHKXER( 'CSYSV ', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL CSYSV( 'U', 2, 0, A, 2, IP, B, 1, W, 1, INFO )
         CALL CHKXER( 'CSYSV ', INFOT, NOUT, LERR, OK )
*
*        CSYSVX
*
         SRNAMT = 'CSYSVX'
         INFOT = 1
         CALL CSYSVX( '/', 'U', 0, 0, A, 1, AF, 1, IP, B, 1, X, 1,
     $                RCOND, R1, R2, W, 1, RW, INFO )
         CALL CHKXER( 'CSYSVX', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL CSYSVX( 'N', '/', 0, 0, A, 1, AF, 1, IP, B, 1, X, 1,
     $                RCOND, R1, R2, W, 1, RW, INFO )
         CALL CHKXER( 'CSYSVX', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL CSYSVX( 'N', 'U', -1, 0, A, 1, AF, 1, IP, B, 1, X, 1,
     $                RCOND, R1, R2, W, 1, RW, INFO )
         CALL CHKXER( 'CSYSVX', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL CSYSVX( 'N', 'U', 0, -1, A, 1, AF, 1, IP, B, 1, X, 1,
     $                RCOND, R1, R2, W, 1, RW, INFO )
         CALL CHKXER( 'CSYSVX', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL CSYSVX( 'N', 'U', 2, 0, A, 1, AF, 2, IP, B, 2, X, 2,
     $                RCOND, R1, R2, W, 4, RW, INFO )
         CALL CHKXER( 'CSYSVX', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL CSYSVX( 'N', 'U', 2, 0, A, 2, AF, 1, IP, B, 2, X, 2,
     $                RCOND, R1, R2, W, 4, RW, INFO )
         CALL CHKXER( 'CSYSVX', INFOT, NOUT, LERR, OK )
         INFOT = 11
         CALL CSYSVX( 'N', 'U', 2, 0, A, 2, AF, 2, IP, B, 1, X, 2,
     $                RCOND, R1, R2, W, 4, RW, INFO )
         CALL CHKXER( 'CSYSVX', INFOT, NOUT, LERR, OK )
         INFOT = 13
         CALL CSYSVX( 'N', 'U', 2, 0, A, 2, AF, 2, IP, B, 2, X, 1,
     $                RCOND, R1, R2, W, 4, RW, INFO )
         CALL CHKXER( 'CSYSVX', INFOT, NOUT, LERR, OK )
         INFOT = 18
         CALL CSYSVX( 'N', 'U', 2, 0, A, 2, AF, 2, IP, B, 2, X, 2,
     $                RCOND, R1, R2, W, 3, RW, INFO )
         CALL CHKXER( 'CSYSVX', INFOT, NOUT, LERR, OK )
*
      ELSE IF( LSAMEN( 2, C2, 'SP' ) ) THEN
*
*        CSPSV
*
         SRNAMT = 'CSPSV '
         INFOT = 1
         CALL CSPSV( '/', 0, 0, A, IP, B, 1, INFO )
         CALL CHKXER( 'CSPSV ', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL CSPSV( 'U', -1, 0, A, IP, B, 1, INFO )
         CALL CHKXER( 'CSPSV ', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL CSPSV( 'U', 0, -1, A, IP, B, 1, INFO )
         CALL CHKXER( 'CSPSV ', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL CSPSV( 'U', 2, 0, A, IP, B, 1, INFO )
         CALL CHKXER( 'CSPSV ', INFOT, NOUT, LERR, OK )
*
*        CSPSVX
*
         SRNAMT = 'CSPSVX'
         INFOT = 1
         CALL CSPSVX( '/', 'U', 0, 0, A, AF, IP, B, 1, X, 1, RCOND, R1,
     $                R2, W, RW, INFO )
         CALL CHKXER( 'CSPSVX', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL CSPSVX( 'N', '/', 0, 0, A, AF, IP, B, 1, X, 1, RCOND, R1,
     $                R2, W, RW, INFO )
         CALL CHKXER( 'CSPSVX', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL CSPSVX( 'N', 'U', -1, 0, A, AF, IP, B, 1, X, 1, RCOND, R1,
     $                R2, W, RW, INFO )
         CALL CHKXER( 'CSPSVX', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL CSPSVX( 'N', 'U', 0, -1, A, AF, IP, B, 1, X, 1, RCOND, R1,
     $                R2, W, RW, INFO )
         CALL CHKXER( 'CSPSVX', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL CSPSVX( 'N', 'U', 2, 0, A, AF, IP, B, 1, X, 2, RCOND, R1,
     $                R2, W, RW, INFO )
         CALL CHKXER( 'CSPSVX', INFOT, NOUT, LERR, OK )
         INFOT = 11
         CALL CSPSVX( 'N', 'U', 2, 0, A, AF, IP, B, 2, X, 1, RCOND, R1,
     $                R2, W, RW, INFO )
         CALL CHKXER( 'CSPSVX', INFOT, NOUT, LERR, OK )
      END IF
*
*     Print a summary line.
*
      IF( OK ) THEN
         WRITE( NOUT, FMT = 9999 )PATH
      ELSE
         WRITE( NOUT, FMT = 9998 )PATH
      END IF
*
 9999 FORMAT( 1X, A3, ' drivers passed the tests of the error exits' )
 9998 FORMAT( ' *** ', A3, ' drivers failed the tests of the error ',
     $      'exits ***' )
*
      RETURN
*
*     End of CERRVX
*
      END
