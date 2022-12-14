      SUBROUTINE ZERRVX( PATH, NUNIT )
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
*  ZERRVX tests the error exits for the COMPLEX*16 driver routines
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
      DOUBLE PRECISION   RCOND
*     ..
*     .. Local Arrays ..
      INTEGER            IP( NMAX )
      DOUBLE PRECISION   C( NMAX ), R( NMAX ), R1( NMAX ), R2( NMAX ),
     $                   RF( NMAX ), RW( NMAX )
      COMPLEX*16         A( NMAX, NMAX ), AF( NMAX, NMAX ), B( NMAX ),
     $                   W( 2*NMAX ), X( NMAX )
*     ..
*     .. External Functions ..
      LOGICAL            LSAMEN
      EXTERNAL           LSAMEN
*     ..
*     .. External Subroutines ..
      EXTERNAL           CHKXER, ZGBSV, ZGBSVX, ZGESV, ZGESVX, ZGTSV,
     $                   ZGTSVX, ZHESV, ZHESVX, ZHPSV, ZHPSVX, ZPBSV,
     $                   ZPBSVX, ZPOSV, ZPOSVX, ZPPSV, ZPPSVX, ZPTSV,
     $                   ZPTSVX, ZSPSV, ZSPSVX, ZSYSV, ZSYSVX
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
      INTRINSIC          DBLE, DCMPLX
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
            A( I, J ) = DCMPLX( 1.D0 / DBLE( I+J ),
     $                  -1.D0 / DBLE( I+J ) )
            AF( I, J ) = DCMPLX( 1.D0 / DBLE( I+J ),
     $                   -1.D0 / DBLE( I+J ) )
   10    CONTINUE
         B( J ) = 0.D0
         R1( J ) = 0.D0
         R2( J ) = 0.D0
         W( J ) = 0.D0
         X( J ) = 0.D0
         C( J ) = 0.D0
         R( J ) = 0.D0
         IP( J ) = J
   20 CONTINUE
      OK = .TRUE.
*
      IF( LSAMEN( 2, C2, 'GE' ) ) THEN
*
*        ZGESV
*
         SRNAMT = 'ZGESV '
         INFOT = 1
         CALL ZGESV( -1, 0, A, 1, IP, B, 1, INFO )
         CALL CHKXER( 'ZGESV ', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL ZGESV( 0, -1, A, 1, IP, B, 1, INFO )
         CALL CHKXER( 'ZGESV ', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL ZGESV( 2, 1, A, 1, IP, B, 2, INFO )
         CALL CHKXER( 'ZGESV ', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL ZGESV( 2, 1, A, 2, IP, B, 1, INFO )
         CALL CHKXER( 'ZGESV ', INFOT, NOUT, LERR, OK )
*
*        ZGESVX
*
         SRNAMT = 'ZGESVX'
         INFOT = 1
         CALL ZGESVX( '/', 'N', 0, 0, A, 1, AF, 1, IP, EQ, R, C, B, 1,
     $                X, 1, RCOND, R1, R2, W, RW, INFO )
         CALL CHKXER( 'ZGESVX', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL ZGESVX( 'N', '/', 0, 0, A, 1, AF, 1, IP, EQ, R, C, B, 1,
     $                X, 1, RCOND, R1, R2, W, RW, INFO )
         CALL CHKXER( 'ZGESVX', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL ZGESVX( 'N', 'N', -1, 0, A, 1, AF, 1, IP, EQ, R, C, B, 1,
     $                X, 1, RCOND, R1, R2, W, RW, INFO )
         CALL CHKXER( 'ZGESVX', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL ZGESVX( 'N', 'N', 0, -1, A, 1, AF, 1, IP, EQ, R, C, B, 1,
     $                X, 1, RCOND, R1, R2, W, RW, INFO )
         CALL CHKXER( 'ZGESVX', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL ZGESVX( 'N', 'N', 2, 1, A, 1, AF, 2, IP, EQ, R, C, B, 2,
     $                X, 2, RCOND, R1, R2, W, RW, INFO )
         CALL CHKXER( 'ZGESVX', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL ZGESVX( 'N', 'N', 2, 1, A, 2, AF, 1, IP, EQ, R, C, B, 2,
     $                X, 2, RCOND, R1, R2, W, RW, INFO )
         CALL CHKXER( 'ZGESVX', INFOT, NOUT, LERR, OK )
         INFOT = 10
         EQ = '/'
         CALL ZGESVX( 'F', 'N', 0, 0, A, 1, AF, 1, IP, EQ, R, C, B, 1,
     $                X, 1, RCOND, R1, R2, W, RW, INFO )
         CALL CHKXER( 'ZGESVX', INFOT, NOUT, LERR, OK )
         INFOT = 11
         EQ = 'R'
         CALL ZGESVX( 'F', 'N', 1, 0, A, 1, AF, 1, IP, EQ, R, C, B, 1,
     $                X, 1, RCOND, R1, R2, W, RW, INFO )
         CALL CHKXER( 'ZGESVX', INFOT, NOUT, LERR, OK )
         INFOT = 12
         EQ = 'C'
         CALL ZGESVX( 'F', 'N', 1, 0, A, 1, AF, 1, IP, EQ, R, C, B, 1,
     $                X, 1, RCOND, R1, R2, W, RW, INFO )
         CALL CHKXER( 'ZGESVX', INFOT, NOUT, LERR, OK )
         INFOT = 14
         CALL ZGESVX( 'N', 'N', 2, 1, A, 2, AF, 2, IP, EQ, R, C, B, 1,
     $                X, 2, RCOND, R1, R2, W, RW, INFO )
         CALL CHKXER( 'ZGESVX', INFOT, NOUT, LERR, OK )
         INFOT = 16
         CALL ZGESVX( 'N', 'N', 2, 1, A, 2, AF, 2, IP, EQ, R, C, B, 2,
     $                X, 1, RCOND, R1, R2, W, RW, INFO )
         CALL CHKXER( 'ZGESVX', INFOT, NOUT, LERR, OK )
*
      ELSE IF( LSAMEN( 2, C2, 'GB' ) ) THEN
*
*        ZGBSV
*
         SRNAMT = 'ZGBSV '
         INFOT = 1
         CALL ZGBSV( -1, 0, 0, 0, A, 1, IP, B, 1, INFO )
         CALL CHKXER( 'ZGBSV ', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL ZGBSV( 1, -1, 0, 0, A, 1, IP, B, 1, INFO )
         CALL CHKXER( 'ZGBSV ', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL ZGBSV( 1, 0, -1, 0, A, 1, IP, B, 1, INFO )
         CALL CHKXER( 'ZGBSV ', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL ZGBSV( 0, 0, 0, -1, A, 1, IP, B, 1, INFO )
         CALL CHKXER( 'ZGBSV ', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL ZGBSV( 1, 1, 1, 0, A, 3, IP, B, 1, INFO )
         CALL CHKXER( 'ZGBSV ', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL ZGBSV( 2, 0, 0, 0, A, 1, IP, B, 1, INFO )
         CALL CHKXER( 'ZGBSV ', INFOT, NOUT, LERR, OK )
*
*        ZGBSVX
*
         SRNAMT = 'ZGBSVX'
         INFOT = 1
         CALL ZGBSVX( '/', 'N', 0, 0, 0, 0, A, 1, AF, 1, IP, EQ, R, C,
     $                B, 1, X, 1, RCOND, R1, R2, W, RW, INFO )
         CALL CHKXER( 'ZGBSVX', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL ZGBSVX( 'N', '/', 0, 0, 0, 0, A, 1, AF, 1, IP, EQ, R, C,
     $                B, 1, X, 1, RCOND, R1, R2, W, RW, INFO )
         CALL CHKXER( 'ZGBSVX', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL ZGBSVX( 'N', 'N', -1, 0, 0, 0, A, 1, AF, 1, IP, EQ, R, C,
     $                B, 1, X, 1, RCOND, R1, R2, W, RW, INFO )
         CALL CHKXER( 'ZGBSVX', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL ZGBSVX( 'N', 'N', 1, -1, 0, 0, A, 1, AF, 1, IP, EQ, R, C,
     $                B, 1, X, 1, RCOND, R1, R2, W, RW, INFO )
         CALL CHKXER( 'ZGBSVX', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL ZGBSVX( 'N', 'N', 1, 0, -1, 0, A, 1, AF, 1, IP, EQ, R, C,
     $                B, 1, X, 1, RCOND, R1, R2, W, RW, INFO )
         CALL CHKXER( 'ZGBSVX', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL ZGBSVX( 'N', 'N', 0, 0, 0, -1, A, 1, AF, 1, IP, EQ, R, C,
     $                B, 1, X, 1, RCOND, R1, R2, W, RW, INFO )
         CALL CHKXER( 'ZGBSVX', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL ZGBSVX( 'N', 'N', 1, 1, 1, 0, A, 2, AF, 4, IP, EQ, R, C,
     $                B, 1, X, 1, RCOND, R1, R2, W, RW, INFO )
         CALL CHKXER( 'ZGBSVX', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL ZGBSVX( 'N', 'N', 1, 1, 1, 0, A, 3, AF, 3, IP, EQ, R, C,
     $                B, 1, X, 1, RCOND, R1, R2, W, RW, INFO )
         CALL CHKXER( 'ZGBSVX', INFOT, NOUT, LERR, OK )
         INFOT = 12
         EQ = '/'
         CALL ZGBSVX( 'F', 'N', 0, 0, 0, 0, A, 1, AF, 1, IP, EQ, R, C,
     $                B, 1, X, 1, RCOND, R1, R2, W, RW, INFO )
         CALL CHKXER( 'ZGBSVX', INFOT, NOUT, LERR, OK )
         INFOT = 13
         EQ = 'R'
         CALL ZGBSVX( 'F', 'N', 1, 0, 0, 0, A, 1, AF, 1, IP, EQ, R, C,
     $                B, 1, X, 1, RCOND, R1, R2, W, RW, INFO )
         CALL CHKXER( 'ZGBSVX', INFOT, NOUT, LERR, OK )
         INFOT = 14
         EQ = 'C'
         CALL ZGBSVX( 'F', 'N', 1, 0, 0, 0, A, 1, AF, 1, IP, EQ, R, C,
     $                B, 1, X, 1, RCOND, R1, R2, W, RW, INFO )
         CALL CHKXER( 'ZGBSVX', INFOT, NOUT, LERR, OK )
         INFOT = 16
         CALL ZGBSVX( 'N', 'N', 2, 0, 0, 0, A, 1, AF, 1, IP, EQ, R, C,
     $                B, 1, X, 2, RCOND, R1, R2, W, RW, INFO )
         CALL CHKXER( 'ZGBSVX', INFOT, NOUT, LERR, OK )
         INFOT = 18
         CALL ZGBSVX( 'N', 'N', 2, 0, 0, 0, A, 1, AF, 1, IP, EQ, R, C,
     $                B, 2, X, 1, RCOND, R1, R2, W, RW, INFO )
         CALL CHKXER( 'ZGBSVX', INFOT, NOUT, LERR, OK )
*
      ELSE IF( LSAMEN( 2, C2, 'GT' ) ) THEN
*
*        ZGTSV
*
         SRNAMT = 'ZGTSV '
         INFOT = 1
         CALL ZGTSV( -1, 0, A( 1, 1 ), A( 1, 2 ), A( 1, 3 ), B, 1,
     $               INFO )
         CALL CHKXER( 'ZGTSV ', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL ZGTSV( 0, -1, A( 1, 1 ), A( 1, 2 ), A( 1, 3 ), B, 1,
     $               INFO )
         CALL CHKXER( 'ZGTSV ', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL ZGTSV( 2, 0, A( 1, 1 ), A( 1, 2 ), A( 1, 3 ), B, 1, INFO )
         CALL CHKXER( 'ZGTSV ', INFOT, NOUT, LERR, OK )
*
*        ZGTSVX
*
         SRNAMT = 'ZGTSVX'
         INFOT = 1
         CALL ZGTSVX( '/', 'N', 0, 0, A( 1, 1 ), A( 1, 2 ), A( 1, 3 ),
     $                AF( 1, 1 ), AF( 1, 2 ), AF( 1, 3 ), AF( 1, 4 ),
     $                IP, B, 1, X, 1, RCOND, R1, R2, W, RW, INFO )
         CALL CHKXER( 'ZGTSVX', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL ZGTSVX( 'N', '/', 0, 0, A( 1, 1 ), A( 1, 2 ), A( 1, 3 ),
     $                AF( 1, 1 ), AF( 1, 2 ), AF( 1, 3 ), AF( 1, 4 ),
     $                IP, B, 1, X, 1, RCOND, R1, R2, W, RW, INFO )
         CALL CHKXER( 'ZGTSVX', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL ZGTSVX( 'N', 'N', -1, 0, A( 1, 1 ), A( 1, 2 ), A( 1, 3 ),
     $                AF( 1, 1 ), AF( 1, 2 ), AF( 1, 3 ), AF( 1, 4 ),
     $                IP, B, 1, X, 1, RCOND, R1, R2, W, RW, INFO )
         CALL CHKXER( 'ZGTSVX', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL ZGTSVX( 'N', 'N', 0, -1, A( 1, 1 ), A( 1, 2 ), A( 1, 3 ),
     $                AF( 1, 1 ), AF( 1, 2 ), AF( 1, 3 ), AF( 1, 4 ),
     $                IP, B, 1, X, 1, RCOND, R1, R2, W, RW, INFO )
         CALL CHKXER( 'ZGTSVX', INFOT, NOUT, LERR, OK )
         INFOT = 14
         CALL ZGTSVX( 'N', 'N', 2, 0, A( 1, 1 ), A( 1, 2 ), A( 1, 3 ),
     $                AF( 1, 1 ), AF( 1, 2 ), AF( 1, 3 ), AF( 1, 4 ),
     $                IP, B, 1, X, 2, RCOND, R1, R2, W, RW, INFO )
         CALL CHKXER( 'ZGTSVX', INFOT, NOUT, LERR, OK )
         INFOT = 16
         CALL ZGTSVX( 'N', 'N', 2, 0, A( 1, 1 ), A( 1, 2 ), A( 1, 3 ),
     $                AF( 1, 1 ), AF( 1, 2 ), AF( 1, 3 ), AF( 1, 4 ),
     $                IP, B, 2, X, 1, RCOND, R1, R2, W, RW, INFO )
         CALL CHKXER( 'ZGTSVX', INFOT, NOUT, LERR, OK )
*
      ELSE IF( LSAMEN( 2, C2, 'PO' ) ) THEN
*
*        ZPOSV
*
         SRNAMT = 'ZPOSV '
         INFOT = 1
         CALL ZPOSV( '/', 0, 0, A, 1, B, 1, INFO )
         CALL CHKXER( 'ZPOSV ', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL ZPOSV( 'U', -1, 0, A, 1, B, 1, INFO )
         CALL CHKXER( 'ZPOSV ', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL ZPOSV( 'U', 0, -1, A, 1, B, 1, INFO )
         CALL CHKXER( 'ZPOSV ', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL ZPOSV( 'U', 2, 0, A, 1, B, 2, INFO )
         CALL CHKXER( 'ZPOSV ', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL ZPOSV( 'U', 2, 0, A, 2, B, 1, INFO )
         CALL CHKXER( 'ZPOSV ', INFOT, NOUT, LERR, OK )
*
*        ZPOSVX
*
         SRNAMT = 'ZPOSVX'
         INFOT = 1
         CALL ZPOSVX( '/', 'U', 0, 0, A, 1, AF, 1, EQ, C, B, 1, X, 1,
     $                RCOND, R1, R2, W, RW, INFO )
         CALL CHKXER( 'ZPOSVX', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL ZPOSVX( 'N', '/', 0, 0, A, 1, AF, 1, EQ, C, B, 1, X, 1,
     $                RCOND, R1, R2, W, RW, INFO )
         CALL CHKXER( 'ZPOSVX', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL ZPOSVX( 'N', 'U', -1, 0, A, 1, AF, 1, EQ, C, B, 1, X, 1,
     $                RCOND, R1, R2, W, RW, INFO )
         CALL CHKXER( 'ZPOSVX', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL ZPOSVX( 'N', 'U', 0, -1, A, 1, AF, 1, EQ, C, B, 1, X, 1,
     $                RCOND, R1, R2, W, RW, INFO )
         CALL CHKXER( 'ZPOSVX', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL ZPOSVX( 'N', 'U', 2, 0, A, 1, AF, 2, EQ, C, B, 2, X, 2,
     $                RCOND, R1, R2, W, RW, INFO )
         CALL CHKXER( 'ZPOSVX', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL ZPOSVX( 'N', 'U', 2, 0, A, 2, AF, 1, EQ, C, B, 2, X, 2,
     $                RCOND, R1, R2, W, RW, INFO )
         CALL CHKXER( 'ZPOSVX', INFOT, NOUT, LERR, OK )
         INFOT = 9
         EQ = '/'
         CALL ZPOSVX( 'F', 'U', 0, 0, A, 1, AF, 1, EQ, C, B, 1, X, 1,
     $                RCOND, R1, R2, W, RW, INFO )
         CALL CHKXER( 'ZPOSVX', INFOT, NOUT, LERR, OK )
         INFOT = 10
         EQ = 'Y'
         CALL ZPOSVX( 'F', 'U', 1, 0, A, 1, AF, 1, EQ, C, B, 1, X, 1,
     $                RCOND, R1, R2, W, RW, INFO )
         CALL CHKXER( 'ZPOSVX', INFOT, NOUT, LERR, OK )
         INFOT = 12
         CALL ZPOSVX( 'N', 'U', 2, 0, A, 2, AF, 2, EQ, C, B, 1, X, 2,
     $                RCOND, R1, R2, W, RW, INFO )
         CALL CHKXER( 'ZPOSVX', INFOT, NOUT, LERR, OK )
         INFOT = 14
         CALL ZPOSVX( 'N', 'U', 2, 0, A, 2, AF, 2, EQ, C, B, 2, X, 1,
     $                RCOND, R1, R2, W, RW, INFO )
         CALL CHKXER( 'ZPOSVX', INFOT, NOUT, LERR, OK )
*
      ELSE IF( LSAMEN( 2, C2, 'PP' ) ) THEN
*
*        ZPPSV
*
         SRNAMT = 'ZPPSV '
         INFOT = 1
         CALL ZPPSV( '/', 0, 0, A, B, 1, INFO )
         CALL CHKXER( 'ZPPSV ', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL ZPPSV( 'U', -1, 0, A, B, 1, INFO )
         CALL CHKXER( 'ZPPSV ', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL ZPPSV( 'U', 0, -1, A, B, 1, INFO )
         CALL CHKXER( 'ZPPSV ', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL ZPPSV( 'U', 2, 0, A, B, 1, INFO )
         CALL CHKXER( 'ZPPSV ', INFOT, NOUT, LERR, OK )
*
*        ZPPSVX
*
         SRNAMT = 'ZPPSVX'
         INFOT = 1
         CALL ZPPSVX( '/', 'U', 0, 0, A, AF, EQ, C, B, 1, X, 1, RCOND,
     $                R1, R2, W, RW, INFO )
         CALL CHKXER( 'ZPPSVX', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL ZPPSVX( 'N', '/', 0, 0, A, AF, EQ, C, B, 1, X, 1, RCOND,
     $                R1, R2, W, RW, INFO )
         CALL CHKXER( 'ZPPSVX', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL ZPPSVX( 'N', 'U', -1, 0, A, AF, EQ, C, B, 1, X, 1, RCOND,
     $                R1, R2, W, RW, INFO )
         CALL CHKXER( 'ZPPSVX', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL ZPPSVX( 'N', 'U', 0, -1, A, AF, EQ, C, B, 1, X, 1, RCOND,
     $                R1, R2, W, RW, INFO )
         CALL CHKXER( 'ZPPSVX', INFOT, NOUT, LERR, OK )
         INFOT = 7
         EQ = '/'
         CALL ZPPSVX( 'F', 'U', 0, 0, A, AF, EQ, C, B, 1, X, 1, RCOND,
     $                R1, R2, W, RW, INFO )
         CALL CHKXER( 'ZPPSVX', INFOT, NOUT, LERR, OK )
         INFOT = 8
         EQ = 'Y'
         CALL ZPPSVX( 'F', 'U', 1, 0, A, AF, EQ, C, B, 1, X, 1, RCOND,
     $                R1, R2, W, RW, INFO )
         CALL CHKXER( 'ZPPSVX', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL ZPPSVX( 'N', 'U', 2, 0, A, AF, EQ, C, B, 1, X, 2, RCOND,
     $                R1, R2, W, RW, INFO )
         CALL CHKXER( 'ZPPSVX', INFOT, NOUT, LERR, OK )
         INFOT = 12
         CALL ZPPSVX( 'N', 'U', 2, 0, A, AF, EQ, C, B, 2, X, 1, RCOND,
     $                R1, R2, W, RW, INFO )
         CALL CHKXER( 'ZPPSVX', INFOT, NOUT, LERR, OK )
*
      ELSE IF( LSAMEN( 2, C2, 'PB' ) ) THEN
*
*        ZPBSV
*
         SRNAMT = 'ZPBSV '
         INFOT = 1
         CALL ZPBSV( '/', 0, 0, 0, A, 1, B, 1, INFO )
         CALL CHKXER( 'ZPBSV ', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL ZPBSV( 'U', -1, 0, 0, A, 1, B, 1, INFO )
         CALL CHKXER( 'ZPBSV ', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL ZPBSV( 'U', 1, -1, 0, A, 1, B, 1, INFO )
         CALL CHKXER( 'ZPBSV ', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL ZPBSV( 'U', 0, 0, -1, A, 1, B, 1, INFO )
         CALL CHKXER( 'ZPBSV ', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL ZPBSV( 'U', 1, 1, 0, A, 1, B, 2, INFO )
         CALL CHKXER( 'ZPBSV ', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL ZPBSV( 'U', 2, 0, 0, A, 1, B, 1, INFO )
         CALL CHKXER( 'ZPBSV ', INFOT, NOUT, LERR, OK )
*
*        ZPBSVX
*
         SRNAMT = 'ZPBSVX'
         INFOT = 1
         CALL ZPBSVX( '/', 'U', 0, 0, 0, A, 1, AF, 1, EQ, C, B, 1, X, 1,
     $                RCOND, R1, R2, W, RW, INFO )
         CALL CHKXER( 'ZPBSVX', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL ZPBSVX( 'N', '/', 0, 0, 0, A, 1, AF, 1, EQ, C, B, 1, X, 1,
     $                RCOND, R1, R2, W, RW, INFO )
         CALL CHKXER( 'ZPBSVX', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL ZPBSVX( 'N', 'U', -1, 0, 0, A, 1, AF, 1, EQ, C, B, 1, X,
     $                1, RCOND, R1, R2, W, RW, INFO )
         CALL CHKXER( 'ZPBSVX', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL ZPBSVX( 'N', 'U', 1, -1, 0, A, 1, AF, 1, EQ, C, B, 1, X,
     $                1, RCOND, R1, R2, W, RW, INFO )
         CALL CHKXER( 'ZPBSVX', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL ZPBSVX( 'N', 'U', 0, 0, -1, A, 1, AF, 1, EQ, C, B, 1, X,
     $                1, RCOND, R1, R2, W, RW, INFO )
         CALL CHKXER( 'ZPBSVX', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL ZPBSVX( 'N', 'U', 1, 1, 0, A, 1, AF, 2, EQ, C, B, 2, X, 2,
     $                RCOND, R1, R2, W, RW, INFO )
         CALL CHKXER( 'ZPBSVX', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL ZPBSVX( 'N', 'U', 1, 1, 0, A, 2, AF, 1, EQ, C, B, 2, X, 2,
     $                RCOND, R1, R2, W, RW, INFO )
         CALL CHKXER( 'ZPBSVX', INFOT, NOUT, LERR, OK )
         INFOT = 10
         EQ = '/'
         CALL ZPBSVX( 'F', 'U', 0, 0, 0, A, 1, AF, 1, EQ, C, B, 1, X, 1,
     $                RCOND, R1, R2, W, RW, INFO )
         CALL CHKXER( 'ZPBSVX', INFOT, NOUT, LERR, OK )
         INFOT = 11
         EQ = 'Y'
         CALL ZPBSVX( 'F', 'U', 1, 0, 0, A, 1, AF, 1, EQ, C, B, 1, X, 1,
     $                RCOND, R1, R2, W, RW, INFO )
         CALL CHKXER( 'ZPBSVX', INFOT, NOUT, LERR, OK )
         INFOT = 13
         CALL ZPBSVX( 'N', 'U', 2, 0, 0, A, 1, AF, 1, EQ, C, B, 1, X, 2,
     $                RCOND, R1, R2, W, RW, INFO )
         CALL CHKXER( 'ZPBSVX', INFOT, NOUT, LERR, OK )
         INFOT = 15
         CALL ZPBSVX( 'N', 'U', 2, 0, 0, A, 1, AF, 1, EQ, C, B, 2, X, 1,
     $                RCOND, R1, R2, W, RW, INFO )
         CALL CHKXER( 'ZPBSVX', INFOT, NOUT, LERR, OK )
*
      ELSE IF( LSAMEN( 2, C2, 'PT' ) ) THEN
*
*        ZPTSV
*
         SRNAMT = 'ZPTSV '
         INFOT = 1
         CALL ZPTSV( -1, 0, R, A( 1, 1 ), B, 1, INFO )
         CALL CHKXER( 'ZPTSV ', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL ZPTSV( 0, -1, R, A( 1, 1 ), B, 1, INFO )
         CALL CHKXER( 'ZPTSV ', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL ZPTSV( 2, 0, R, A( 1, 1 ), B, 1, INFO )
         CALL CHKXER( 'ZPTSV ', INFOT, NOUT, LERR, OK )
*
*        ZPTSVX
*
         SRNAMT = 'ZPTSVX'
         INFOT = 1
         CALL ZPTSVX( '/', 0, 0, R, A( 1, 1 ), RF, AF( 1, 1 ), B, 1, X,
     $                1, RCOND, R1, R2, W, RW, INFO )
         CALL CHKXER( 'ZPTSVX', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL ZPTSVX( 'N', -1, 0, R, A( 1, 1 ), RF, AF( 1, 1 ), B, 1, X,
     $                1, RCOND, R1, R2, W, RW, INFO )
         CALL CHKXER( 'ZPTSVX', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL ZPTSVX( 'N', 0, -1, R, A( 1, 1 ), RF, AF( 1, 1 ), B, 1, X,
     $                1, RCOND, R1, R2, W, RW, INFO )
         CALL CHKXER( 'ZPTSVX', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL ZPTSVX( 'N', 2, 0, R, A( 1, 1 ), RF, AF( 1, 1 ), B, 1, X,
     $                2, RCOND, R1, R2, W, RW, INFO )
         CALL CHKXER( 'ZPTSVX', INFOT, NOUT, LERR, OK )
         INFOT = 11
         CALL ZPTSVX( 'N', 2, 0, R, A( 1, 1 ), RF, AF( 1, 1 ), B, 2, X,
     $                1, RCOND, R1, R2, W, RW, INFO )
         CALL CHKXER( 'ZPTSVX', INFOT, NOUT, LERR, OK )
*
      ELSE IF( LSAMEN( 2, C2, 'HE' ) ) THEN
*
*        ZHESV
*
         SRNAMT = 'ZHESV '
         INFOT = 1
         CALL ZHESV( '/', 0, 0, A, 1, IP, B, 1, W, 1, INFO )
         CALL CHKXER( 'ZHESV ', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL ZHESV( 'U', -1, 0, A, 1, IP, B, 1, W, 1, INFO )
         CALL CHKXER( 'ZHESV ', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL ZHESV( 'U', 0, -1, A, 1, IP, B, 1, W, 1, INFO )
         CALL CHKXER( 'ZHESV ', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL ZHESV( 'U', 2, 0, A, 1, IP, B, 2, W, 1, INFO )
         CALL CHKXER( 'ZHESV ', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL ZHESV( 'U', 2, 0, A, 2, IP, B, 1, W, 1, INFO )
         CALL CHKXER( 'ZHESV ', INFOT, NOUT, LERR, OK )
*
*        ZHESVX
*
         SRNAMT = 'ZHESVX'
         INFOT = 1
         CALL ZHESVX( '/', 'U', 0, 0, A, 1, AF, 1, IP, B, 1, X, 1,
     $                RCOND, R1, R2, W, 1, RW, INFO )
         CALL CHKXER( 'ZHESVX', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL ZHESVX( 'N', '/', 0, 0, A, 1, AF, 1, IP, B, 1, X, 1,
     $                RCOND, R1, R2, W, 1, RW, INFO )
         CALL CHKXER( 'ZHESVX', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL ZHESVX( 'N', 'U', -1, 0, A, 1, AF, 1, IP, B, 1, X, 1,
     $                RCOND, R1, R2, W, 1, RW, INFO )
         CALL CHKXER( 'ZHESVX', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL ZHESVX( 'N', 'U', 0, -1, A, 1, AF, 1, IP, B, 1, X, 1,
     $                RCOND, R1, R2, W, 1, RW, INFO )
         CALL CHKXER( 'ZHESVX', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL ZHESVX( 'N', 'U', 2, 0, A, 1, AF, 2, IP, B, 2, X, 2,
     $                RCOND, R1, R2, W, 4, RW, INFO )
         CALL CHKXER( 'ZHESVX', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL ZHESVX( 'N', 'U', 2, 0, A, 2, AF, 1, IP, B, 2, X, 2,
     $                RCOND, R1, R2, W, 4, RW, INFO )
         CALL CHKXER( 'ZHESVX', INFOT, NOUT, LERR, OK )
         INFOT = 11
         CALL ZHESVX( 'N', 'U', 2, 0, A, 2, AF, 2, IP, B, 1, X, 2,
     $                RCOND, R1, R2, W, 4, RW, INFO )
         CALL CHKXER( 'ZHESVX', INFOT, NOUT, LERR, OK )
         INFOT = 13
         CALL ZHESVX( 'N', 'U', 2, 0, A, 2, AF, 2, IP, B, 2, X, 1,
     $                RCOND, R1, R2, W, 4, RW, INFO )
         CALL CHKXER( 'ZHESVX', INFOT, NOUT, LERR, OK )
         INFOT = 18
         CALL ZHESVX( 'N', 'U', 2, 0, A, 2, AF, 2, IP, B, 2, X, 2,
     $                RCOND, R1, R2, W, 3, RW, INFO )
         CALL CHKXER( 'ZHESVX', INFOT, NOUT, LERR, OK )
*
      ELSE IF( LSAMEN( 2, C2, 'HP' ) ) THEN
*
*        ZHPSV
*
         SRNAMT = 'ZHPSV '
         INFOT = 1
         CALL ZHPSV( '/', 0, 0, A, IP, B, 1, INFO )
         CALL CHKXER( 'ZHPSV ', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL ZHPSV( 'U', -1, 0, A, IP, B, 1, INFO )
         CALL CHKXER( 'ZHPSV ', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL ZHPSV( 'U', 0, -1, A, IP, B, 1, INFO )
         CALL CHKXER( 'ZHPSV ', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL ZHPSV( 'U', 2, 0, A, IP, B, 1, INFO )
         CALL CHKXER( 'ZHPSV ', INFOT, NOUT, LERR, OK )
*
*        ZHPSVX
*
         SRNAMT = 'ZHPSVX'
         INFOT = 1
         CALL ZHPSVX( '/', 'U', 0, 0, A, AF, IP, B, 1, X, 1, RCOND, R1,
     $                R2, W, RW, INFO )
         CALL CHKXER( 'ZHPSVX', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL ZHPSVX( 'N', '/', 0, 0, A, AF, IP, B, 1, X, 1, RCOND, R1,
     $                R2, W, RW, INFO )
         CALL CHKXER( 'ZHPSVX', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL ZHPSVX( 'N', 'U', -1, 0, A, AF, IP, B, 1, X, 1, RCOND, R1,
     $                R2, W, RW, INFO )
         CALL CHKXER( 'ZHPSVX', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL ZHPSVX( 'N', 'U', 0, -1, A, AF, IP, B, 1, X, 1, RCOND, R1,
     $                R2, W, RW, INFO )
         CALL CHKXER( 'ZHPSVX', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL ZHPSVX( 'N', 'U', 2, 0, A, AF, IP, B, 1, X, 2, RCOND, R1,
     $                R2, W, RW, INFO )
         CALL CHKXER( 'ZHPSVX', INFOT, NOUT, LERR, OK )
         INFOT = 11
         CALL ZHPSVX( 'N', 'U', 2, 0, A, AF, IP, B, 2, X, 1, RCOND, R1,
     $                R2, W, RW, INFO )
         CALL CHKXER( 'ZHPSVX', INFOT, NOUT, LERR, OK )
*
      ELSE IF( LSAMEN( 2, C2, 'SY' ) ) THEN
*
*        ZSYSV
*
         SRNAMT = 'ZSYSV '
         INFOT = 1
         CALL ZSYSV( '/', 0, 0, A, 1, IP, B, 1, W, 1, INFO )
         CALL CHKXER( 'ZSYSV ', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL ZSYSV( 'U', -1, 0, A, 1, IP, B, 1, W, 1, INFO )
         CALL CHKXER( 'ZSYSV ', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL ZSYSV( 'U', 0, -1, A, 1, IP, B, 1, W, 1, INFO )
         CALL CHKXER( 'ZSYSV ', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL ZSYSV( 'U', 2, 0, A, 2, IP, B, 1, W, 1, INFO )
         CALL CHKXER( 'ZSYSV ', INFOT, NOUT, LERR, OK )
*
*        ZSYSVX
*
         SRNAMT = 'ZSYSVX'
         INFOT = 1
         CALL ZSYSVX( '/', 'U', 0, 0, A, 1, AF, 1, IP, B, 1, X, 1,
     $                RCOND, R1, R2, W, 1, RW, INFO )
         CALL CHKXER( 'ZSYSVX', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL ZSYSVX( 'N', '/', 0, 0, A, 1, AF, 1, IP, B, 1, X, 1,
     $                RCOND, R1, R2, W, 1, RW, INFO )
         CALL CHKXER( 'ZSYSVX', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL ZSYSVX( 'N', 'U', -1, 0, A, 1, AF, 1, IP, B, 1, X, 1,
     $                RCOND, R1, R2, W, 1, RW, INFO )
         CALL CHKXER( 'ZSYSVX', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL ZSYSVX( 'N', 'U', 0, -1, A, 1, AF, 1, IP, B, 1, X, 1,
     $                RCOND, R1, R2, W, 1, RW, INFO )
         CALL CHKXER( 'ZSYSVX', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL ZSYSVX( 'N', 'U', 2, 0, A, 1, AF, 2, IP, B, 2, X, 2,
     $                RCOND, R1, R2, W, 4, RW, INFO )
         CALL CHKXER( 'ZSYSVX', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL ZSYSVX( 'N', 'U', 2, 0, A, 2, AF, 1, IP, B, 2, X, 2,
     $                RCOND, R1, R2, W, 4, RW, INFO )
         CALL CHKXER( 'ZSYSVX', INFOT, NOUT, LERR, OK )
         INFOT = 11
         CALL ZSYSVX( 'N', 'U', 2, 0, A, 2, AF, 2, IP, B, 1, X, 2,
     $                RCOND, R1, R2, W, 4, RW, INFO )
         CALL CHKXER( 'ZSYSVX', INFOT, NOUT, LERR, OK )
         INFOT = 13
         CALL ZSYSVX( 'N', 'U', 2, 0, A, 2, AF, 2, IP, B, 2, X, 1,
     $                RCOND, R1, R2, W, 4, RW, INFO )
         CALL CHKXER( 'ZSYSVX', INFOT, NOUT, LERR, OK )
         INFOT = 18
         CALL ZSYSVX( 'N', 'U', 2, 0, A, 2, AF, 2, IP, B, 2, X, 2,
     $                RCOND, R1, R2, W, 3, RW, INFO )
         CALL CHKXER( 'ZSYSVX', INFOT, NOUT, LERR, OK )
*
      ELSE IF( LSAMEN( 2, C2, 'SP' ) ) THEN
*
*        ZSPSV
*
         SRNAMT = 'ZSPSV '
         INFOT = 1
         CALL ZSPSV( '/', 0, 0, A, IP, B, 1, INFO )
         CALL CHKXER( 'ZSPSV ', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL ZSPSV( 'U', -1, 0, A, IP, B, 1, INFO )
         CALL CHKXER( 'ZSPSV ', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL ZSPSV( 'U', 0, -1, A, IP, B, 1, INFO )
         CALL CHKXER( 'ZSPSV ', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL ZSPSV( 'U', 2, 0, A, IP, B, 1, INFO )
         CALL CHKXER( 'ZSPSV ', INFOT, NOUT, LERR, OK )
*
*        ZSPSVX
*
         SRNAMT = 'ZSPSVX'
         INFOT = 1
         CALL ZSPSVX( '/', 'U', 0, 0, A, AF, IP, B, 1, X, 1, RCOND, R1,
     $                R2, W, RW, INFO )
         CALL CHKXER( 'ZSPSVX', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL ZSPSVX( 'N', '/', 0, 0, A, AF, IP, B, 1, X, 1, RCOND, R1,
     $                R2, W, RW, INFO )
         CALL CHKXER( 'ZSPSVX', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL ZSPSVX( 'N', 'U', -1, 0, A, AF, IP, B, 1, X, 1, RCOND, R1,
     $                R2, W, RW, INFO )
         CALL CHKXER( 'ZSPSVX', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL ZSPSVX( 'N', 'U', 0, -1, A, AF, IP, B, 1, X, 1, RCOND, R1,
     $                R2, W, RW, INFO )
         CALL CHKXER( 'ZSPSVX', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL ZSPSVX( 'N', 'U', 2, 0, A, AF, IP, B, 1, X, 2, RCOND, R1,
     $                R2, W, RW, INFO )
         CALL CHKXER( 'ZSPSVX', INFOT, NOUT, LERR, OK )
         INFOT = 11
         CALL ZSPSVX( 'N', 'U', 2, 0, A, AF, IP, B, 2, X, 1, RCOND, R1,
     $                R2, W, RW, INFO )
         CALL CHKXER( 'ZSPSVX', INFOT, NOUT, LERR, OK )
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
*     End of ZERRVX
*
      END
