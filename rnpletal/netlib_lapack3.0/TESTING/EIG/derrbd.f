      SUBROUTINE DERRBD( PATH, NUNIT )
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
*  DERRBD tests the error exits for DGEBRD, DORGBR, DORMBR, DBDSQR and
*  DBDSDC.
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
      INTEGER            NMAX, LW
      PARAMETER          ( NMAX = 4, LW = NMAX )
*     ..
*     .. Local Scalars ..
      CHARACTER*2        C2
      INTEGER            I, INFO, J, NT
*     ..
*     .. Local Arrays ..
      INTEGER            IQ( NMAX, NMAX ), IW( NMAX )
      DOUBLE PRECISION   A( NMAX, NMAX ), D( NMAX ), E( NMAX ),
     $                   Q( NMAX, NMAX ), TP( NMAX ), TQ( NMAX ),
     $                   U( NMAX, NMAX ), V( NMAX, NMAX ), W( LW )
*     ..
*     .. External Functions ..
      LOGICAL            LSAMEN
      EXTERNAL           LSAMEN
*     ..
*     .. External Subroutines ..
      EXTERNAL           CHKXER, DBDSDC, DBDSQR, DGEBD2, DGEBRD, DORGBR,
     $                   DORMBR
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
      INTRINSIC          DBLE
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
            A( I, J ) = 1.D0 / DBLE( I+J )
   10    CONTINUE
   20 CONTINUE
      OK = .TRUE.
      NT = 0
*
*     Test error exits of the SVD routines.
*
      IF( LSAMEN( 2, C2, 'BD' ) ) THEN
*
*        DGEBRD
*
         SRNAMT = 'DGEBRD'
         INFOT = 1
         CALL DGEBRD( -1, 0, A, 1, D, E, TQ, TP, W, 1, INFO )
         CALL CHKXER( 'DGEBRD', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL DGEBRD( 0, -1, A, 1, D, E, TQ, TP, W, 1, INFO )
         CALL CHKXER( 'DGEBRD', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL DGEBRD( 2, 1, A, 1, D, E, TQ, TP, W, 2, INFO )
         CALL CHKXER( 'DGEBRD', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL DGEBRD( 2, 1, A, 2, D, E, TQ, TP, W, 1, INFO )
         CALL CHKXER( 'DGEBRD', INFOT, NOUT, LERR, OK )
         NT = NT + 4
*
*        DGEBD2
*
         SRNAMT = 'DGEBD2'
         INFOT = 1
         CALL DGEBD2( -1, 0, A, 1, D, E, TQ, TP, W, INFO )
         CALL CHKXER( 'DGEBD2', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL DGEBD2( 0, -1, A, 1, D, E, TQ, TP, W, INFO )
         CALL CHKXER( 'DGEBD2', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL DGEBD2( 2, 1, A, 1, D, E, TQ, TP, W, INFO )
         CALL CHKXER( 'DGEBD2', INFOT, NOUT, LERR, OK )
         NT = NT + 3
*
*        DORGBR
*
         SRNAMT = 'DORGBR'
         INFOT = 1
         CALL DORGBR( '/', 0, 0, 0, A, 1, TQ, W, 1, INFO )
         CALL CHKXER( 'DORGBR', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL DORGBR( 'Q', -1, 0, 0, A, 1, TQ, W, 1, INFO )
         CALL CHKXER( 'DORGBR', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL DORGBR( 'Q', 0, -1, 0, A, 1, TQ, W, 1, INFO )
         CALL CHKXER( 'DORGBR', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL DORGBR( 'Q', 0, 1, 0, A, 1, TQ, W, 1, INFO )
         CALL CHKXER( 'DORGBR', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL DORGBR( 'Q', 1, 0, 1, A, 1, TQ, W, 1, INFO )
         CALL CHKXER( 'DORGBR', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL DORGBR( 'P', 1, 0, 0, A, 1, TQ, W, 1, INFO )
         CALL CHKXER( 'DORGBR', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL DORGBR( 'P', 0, 1, 1, A, 1, TQ, W, 1, INFO )
         CALL CHKXER( 'DORGBR', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL DORGBR( 'Q', 0, 0, -1, A, 1, TQ, W, 1, INFO )
         CALL CHKXER( 'DORGBR', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL DORGBR( 'Q', 2, 1, 1, A, 1, TQ, W, 1, INFO )
         CALL CHKXER( 'DORGBR', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL DORGBR( 'Q', 2, 2, 1, A, 2, TQ, W, 1, INFO )
         CALL CHKXER( 'DORGBR', INFOT, NOUT, LERR, OK )
         NT = NT + 10
*
*        DORMBR
*
         SRNAMT = 'DORMBR'
         INFOT = 1
         CALL DORMBR( '/', 'L', 'T', 0, 0, 0, A, 1, TQ, U, 1, W, 1,
     $                INFO )
         CALL CHKXER( 'DORMBR', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL DORMBR( 'Q', '/', 'T', 0, 0, 0, A, 1, TQ, U, 1, W, 1,
     $                INFO )
         CALL CHKXER( 'DORMBR', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL DORMBR( 'Q', 'L', '/', 0, 0, 0, A, 1, TQ, U, 1, W, 1,
     $                INFO )
         CALL CHKXER( 'DORMBR', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL DORMBR( 'Q', 'L', 'T', -1, 0, 0, A, 1, TQ, U, 1, W, 1,
     $                INFO )
         CALL CHKXER( 'DORMBR', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL DORMBR( 'Q', 'L', 'T', 0, -1, 0, A, 1, TQ, U, 1, W, 1,
     $                INFO )
         CALL CHKXER( 'DORMBR', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL DORMBR( 'Q', 'L', 'T', 0, 0, -1, A, 1, TQ, U, 1, W, 1,
     $                INFO )
         CALL CHKXER( 'DORMBR', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL DORMBR( 'Q', 'L', 'T', 2, 0, 0, A, 1, TQ, U, 2, W, 1,
     $                INFO )
         CALL CHKXER( 'DORMBR', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL DORMBR( 'Q', 'R', 'T', 0, 2, 0, A, 1, TQ, U, 1, W, 1,
     $                INFO )
         CALL CHKXER( 'DORMBR', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL DORMBR( 'P', 'L', 'T', 2, 0, 2, A, 1, TQ, U, 2, W, 1,
     $                INFO )
         CALL CHKXER( 'DORMBR', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL DORMBR( 'P', 'R', 'T', 0, 2, 2, A, 1, TQ, U, 1, W, 1,
     $                INFO )
         CALL CHKXER( 'DORMBR', INFOT, NOUT, LERR, OK )
         INFOT = 11
         CALL DORMBR( 'Q', 'R', 'T', 2, 0, 0, A, 1, TQ, U, 1, W, 1,
     $                INFO )
         CALL CHKXER( 'DORMBR', INFOT, NOUT, LERR, OK )
         INFOT = 13
         CALL DORMBR( 'Q', 'L', 'T', 0, 2, 0, A, 1, TQ, U, 1, W, 1,
     $                INFO )
         CALL CHKXER( 'DORMBR', INFOT, NOUT, LERR, OK )
         INFOT = 13
         CALL DORMBR( 'Q', 'R', 'T', 2, 0, 0, A, 1, TQ, U, 2, W, 1,
     $                INFO )
         CALL CHKXER( 'DORMBR', INFOT, NOUT, LERR, OK )
         NT = NT + 13
*
*        DBDSQR
*
         SRNAMT = 'DBDSQR'
         INFOT = 1
         CALL DBDSQR( '/', 0, 0, 0, 0, D, E, V, 1, U, 1, A, 1, W, INFO )
         CALL CHKXER( 'DBDSQR', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL DBDSQR( 'U', -1, 0, 0, 0, D, E, V, 1, U, 1, A, 1, W,
     $                INFO )
         CALL CHKXER( 'DBDSQR', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL DBDSQR( 'U', 0, -1, 0, 0, D, E, V, 1, U, 1, A, 1, W,
     $                INFO )
         CALL CHKXER( 'DBDSQR', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL DBDSQR( 'U', 0, 0, -1, 0, D, E, V, 1, U, 1, A, 1, W,
     $                INFO )
         CALL CHKXER( 'DBDSQR', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL DBDSQR( 'U', 0, 0, 0, -1, D, E, V, 1, U, 1, A, 1, W,
     $                INFO )
         CALL CHKXER( 'DBDSQR', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL DBDSQR( 'U', 2, 1, 0, 0, D, E, V, 1, U, 1, A, 1, W, INFO )
         CALL CHKXER( 'DBDSQR', INFOT, NOUT, LERR, OK )
         INFOT = 11
         CALL DBDSQR( 'U', 0, 0, 2, 0, D, E, V, 1, U, 1, A, 1, W, INFO )
         CALL CHKXER( 'DBDSQR', INFOT, NOUT, LERR, OK )
         INFOT = 13
         CALL DBDSQR( 'U', 2, 0, 0, 1, D, E, V, 1, U, 1, A, 1, W, INFO )
         CALL CHKXER( 'DBDSQR', INFOT, NOUT, LERR, OK )
         NT = NT + 8
*
*        DBDSDC
*
         SRNAMT = 'DBDSDC'
         INFOT = 1
         CALL DBDSDC( '/', 'N', 0, D, E, U, 1, V, 1, Q, IQ, W, IW,
     $                INFO )
         CALL CHKXER( 'DBDSDC', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL DBDSDC( 'U', '/', 0, D, E, U, 1, V, 1, Q, IQ, W, IW,
     $                INFO )
         CALL CHKXER( 'DBDSDC', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL DBDSDC( 'U', 'N', -1, D, E, U, 1, V, 1, Q, IQ, W, IW,
     $                INFO )
         CALL CHKXER( 'DBDSDC', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL DBDSDC( 'U', 'I', 2, D, E, U, 1, V, 1, Q, IQ, W, IW,
     $                INFO )
         CALL CHKXER( 'DBDSDC', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL DBDSDC( 'U', 'I', 2, D, E, U, 2, V, 1, Q, IQ, W, IW,
     $                INFO )
         CALL CHKXER( 'DBDSDC', INFOT, NOUT, LERR, OK )
         NT = NT + 5
      END IF
*
*     Print a summary line.
*
      IF( OK ) THEN
         WRITE( NOUT, FMT = 9999 )PATH, NT
      ELSE
         WRITE( NOUT, FMT = 9998 )PATH
      END IF
*
 9999 FORMAT( 1X, A3, ' routines passed the tests of the error exits',
     $      ' (', I3, ' tests done)' )
 9998 FORMAT( ' *** ', A3, ' routines failed the tests of the error ',
     $      'exits ***' )
*
      RETURN
*
*     End of DERRBD
*
      END
