      SUBROUTINE ZERRQP( PATH, NUNIT )
*
*  -- LAPACK test routine (version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     June 30, 1999
*
*     .. Scalar Arguments ..
      CHARACTER*3        PATH
      INTEGER            NUNIT
*     ..
*
*  Purpose
*  =======
*
*  ZERRQP tests the error exits for ZGEQPF and CGEQP3.
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
      PARAMETER          ( NMAX = 2 )
*     ..
*     .. Local Scalars ..
      CHARACTER*2        C2
      INTEGER            INFO, LW
*     ..
*     .. Local Arrays ..
      INTEGER            IP( NMAX )
      DOUBLE PRECISION   RW( 2*NMAX )
      COMPLEX*16         A( NMAX, NMAX ), TAU( NMAX ),
     $                   W( 2*NMAX+3*NMAX )
*     ..
*     .. External Functions ..
      LOGICAL            LSAMEN
      EXTERNAL           LSAMEN
*     ..
*     .. External Subroutines ..
      EXTERNAL           ALAESM, CHKXER, ZGEQP3, ZGEQPF
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
      INTRINSIC          DCMPLX
*     ..
*     .. Executable Statements ..
*
      NOUT = NUNIT
      C2 = PATH( 2: 3 )
      LW = NMAX + 1
      A( 1, 1 ) = DCMPLX( 1.0D+0, -1.0D+0 )
      A( 1, 2 ) = DCMPLX( 2.0D+0, -2.0D+0 )
      A( 2, 2 ) = DCMPLX( 3.0D+0, -3.0D+0 )
      A( 2, 1 ) = DCMPLX( 4.0D+0, -4.0D+0 )
      OK = .TRUE.
      WRITE( NOUT, FMT = * )
*
*     Test error exits for QR factorization with pivoting
*
      IF( LSAMEN( 2, C2, 'QP' ) ) THEN
*
*        ZGEQPF
*
         SRNAMT = 'ZGEQPF'
         INFOT = 1
         CALL ZGEQPF( -1, 0, A, 1, IP, TAU, W, RW, INFO )
         CALL CHKXER( 'ZGEQPF', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL ZGEQPF( 0, -1, A, 1, IP, TAU, W, RW, INFO )
         CALL CHKXER( 'ZGEQPF', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL ZGEQPF( 2, 0, A, 1, IP, TAU, W, RW, INFO )
         CALL CHKXER( 'ZGEQPF', INFOT, NOUT, LERR, OK )
*
*        ZGEQP3
*
         SRNAMT = 'ZGEQP3'
         INFOT = 1
         CALL ZGEQP3( -1, 0, A, 1, IP, TAU, W, LW, RW, INFO )
         CALL CHKXER( 'ZGEQP3', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL ZGEQP3( 1, -1, A, 1, IP, TAU, W, LW, RW, INFO )
         CALL CHKXER( 'ZGEQP3', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL ZGEQP3( 1, 1, A, 0, IP, TAU, W, LW, RW, INFO )
         CALL CHKXER( 'ZGEQP3', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL ZGEQP3( 2, 2, A, 2, IP, TAU, W, LW-1, RW, INFO )
         CALL CHKXER( 'ZGEQP3', INFOT, NOUT, LERR, OK )
      END IF
*
*     Print a summary line.
*
      CALL ALAESM( PATH, OK, NOUT )
*
      RETURN
*
*     End of ZERRQP
*
      END
