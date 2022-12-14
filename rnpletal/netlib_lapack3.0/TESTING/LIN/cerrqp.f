      SUBROUTINE CERRQP( PATH, NUNIT )
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
*  CERRQP tests the error exits for CGEQPF and CGEQP3.
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
      REAL               RW( 2*NMAX )
      COMPLEX            A( NMAX, NMAX ), TAU( NMAX ),
     $                   W( 2*NMAX+3*NMAX )
*     ..
*     .. External Functions ..
      LOGICAL            LSAMEN
      EXTERNAL           LSAMEN
*     ..
*     .. External Subroutines ..
      EXTERNAL           ALAESM, CGEQP3, CGEQPF, CHKXER
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
      INTRINSIC          CMPLX
*     ..
*     .. Executable Statements ..
*
      NOUT = NUNIT
      C2 = PATH( 2: 3 )
      LW = NMAX + 1
      A( 1, 1 ) = CMPLX( 1.0E+0, -1.0E+0 )
      A( 1, 2 ) = CMPLX( 2.0E+0, -2.0E+0 )
      A( 2, 2 ) = CMPLX( 3.0E+0, -3.0E+0 )
      A( 2, 1 ) = CMPLX( 4.0E+0, -4.0E+0 )
      OK = .TRUE.
      WRITE( NOUT, FMT = * )
*
*     Test error exits for QR factorization with pivoting
*
      IF( LSAMEN( 2, C2, 'QP' ) ) THEN
*
*        CGEQPF
*
         SRNAMT = 'CGEQPF'
         INFOT = 1
         CALL CGEQPF( -1, 0, A, 1, IP, TAU, W, RW, INFO )
         CALL CHKXER( 'CGEQPF', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL CGEQPF( 0, -1, A, 1, IP, TAU, W, RW, INFO )
         CALL CHKXER( 'CGEQPF', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL CGEQPF( 2, 0, A, 1, IP, TAU, W, RW, INFO )
         CALL CHKXER( 'CGEQPF', INFOT, NOUT, LERR, OK )
*
*        CGEQP3
*
         SRNAMT = 'CGEQP3'
         INFOT = 1
         CALL CGEQP3( -1, 0, A, 1, IP, TAU, W, LW, RW, INFO )
         CALL CHKXER( 'CGEQP3', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL CGEQP3( 1, -1, A, 1, IP, TAU, W, LW, RW, INFO )
         CALL CHKXER( 'CGEQP3', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL CGEQP3( 1, 1, A, 0, IP, TAU, W, LW, RW, INFO )
         CALL CHKXER( 'CGEQP3', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL CGEQP3( 2, 2, A, 2, IP, TAU, W, LW-1, RW, INFO )
         CALL CHKXER( 'CGEQP3', INFOT, NOUT, LERR, OK )
      END IF
*
*     Print a summary line.
*
      CALL ALAESM( PATH, OK, NOUT )
*
      RETURN
*
*     End of CERRQP
*
      END
