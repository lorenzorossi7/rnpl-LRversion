      SUBROUTINE CERRST( PATH, NUNIT )
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
*  CERRST tests the error exits for CHETRD, CUNGTR, CUNMTR, CHPTRD,
*  CUNGTR, CUPMTR, CSTEQR, CSTEIN, CPTEQR, CHBTRD,
*  CHEEV, CHEEVX, CHEEVD, CHBEV, CHBEVX, CHBEVD,
*  CHPEV, CHPEVX, CHPEVD, and CSTEDC.
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
      INTEGER            NMAX, LIW, LW
      PARAMETER          ( NMAX = 3, LIW = 12*NMAX, LW = 20*NMAX )
*     ..
*     .. Local Scalars ..
      CHARACTER*2        C2
      INTEGER            I, INFO, J, M, N, NT
*     ..
*     .. Local Arrays ..
      INTEGER            I1( NMAX ), I2( NMAX ), I3( NMAX ), IW( LIW )
      REAL               D( NMAX ), E( NMAX ), R( LW ), RW( LW ),
     $                   X( NMAX )
      COMPLEX            A( NMAX, NMAX ), C( NMAX, NMAX ),
     $                   Q( NMAX, NMAX ), TAU( NMAX ), W( LW ),
     $                   Z( NMAX, NMAX )
*     ..
*     .. External Functions ..
      LOGICAL            LSAMEN
      EXTERNAL           LSAMEN
*     ..
*     .. External Subroutines ..
      EXTERNAL           CHBEV, CHBEVD, CHBEVX, CHBTRD, CHEEV, CHEEVD,
     $                   CHEEVR, CHEEVX, CHETRD, CHKXER, CHPEV, CHPEVD,
     $                   CHPEVX, CHPTRD, CPTEQR, CSTEDC, CSTEIN, CSTEQR,
     $                   CUNGTR, CUNMTR, CUPGTR, CUPMTR
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
      INTRINSIC          REAL
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
            A( I, J ) = 1. / REAL( I+J )
   10    CONTINUE
   20 CONTINUE
      DO 30 J = 1, NMAX
         D( J ) = REAL( J )
         E( J ) = 0.0
         I1( J ) = J
         I2( J ) = J
         TAU( J ) = 1.
   30 CONTINUE
      OK = .TRUE.
      NT = 0
*
*     Test error exits for the ST path.
*
      IF( LSAMEN( 2, C2, 'ST' ) ) THEN
*
*        CHETRD
*
         SRNAMT = 'CHETRD'
         INFOT = 1
         CALL CHETRD( '/', 0, A, 1, D, E, TAU, W, 1, INFO )
         CALL CHKXER( 'CHETRD', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL CHETRD( 'U', -1, A, 1, D, E, TAU, W, 1, INFO )
         CALL CHKXER( 'CHETRD', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL CHETRD( 'U', 2, A, 1, D, E, TAU, W, 1, INFO )
         CALL CHKXER( 'CHETRD', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL CHETRD( 'U', 0, A, 1, D, E, TAU, W, 0, INFO )
         CALL CHKXER( 'CHETRD', INFOT, NOUT, LERR, OK )
         NT = NT + 4
*
*        CUNGTR
*
         SRNAMT = 'CUNGTR'
         INFOT = 1
         CALL CUNGTR( '/', 0, A, 1, TAU, W, 1, INFO )
         CALL CHKXER( 'CUNGTR', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL CUNGTR( 'U', -1, A, 1, TAU, W, 1, INFO )
         CALL CHKXER( 'CUNGTR', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL CUNGTR( 'U', 2, A, 1, TAU, W, 1, INFO )
         CALL CHKXER( 'CUNGTR', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL CUNGTR( 'U', 3, A, 3, TAU, W, 1, INFO )
         CALL CHKXER( 'CUNGTR', INFOT, NOUT, LERR, OK )
         NT = NT + 4
*
*        CUNMTR
*
         SRNAMT = 'CUNMTR'
         INFOT = 1
         CALL CUNMTR( '/', 'U', 'N', 0, 0, A, 1, TAU, C, 1, W, 1, INFO )
         CALL CHKXER( 'CUNMTR', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL CUNMTR( 'L', '/', 'N', 0, 0, A, 1, TAU, C, 1, W, 1, INFO )
         CALL CHKXER( 'CUNMTR', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL CUNMTR( 'L', 'U', '/', 0, 0, A, 1, TAU, C, 1, W, 1, INFO )
         CALL CHKXER( 'CUNMTR', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL CUNMTR( 'L', 'U', 'N', -1, 0, A, 1, TAU, C, 1, W, 1,
     $                INFO )
         CALL CHKXER( 'CUNMTR', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL CUNMTR( 'L', 'U', 'N', 0, -1, A, 1, TAU, C, 1, W, 1,
     $                INFO )
         CALL CHKXER( 'CUNMTR', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL CUNMTR( 'L', 'U', 'N', 2, 0, A, 1, TAU, C, 2, W, 1, INFO )
         CALL CHKXER( 'CUNMTR', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL CUNMTR( 'R', 'U', 'N', 0, 2, A, 1, TAU, C, 1, W, 1, INFO )
         CALL CHKXER( 'CUNMTR', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL CUNMTR( 'L', 'U', 'N', 2, 0, A, 2, TAU, C, 1, W, 1, INFO )
         CALL CHKXER( 'CUNMTR', INFOT, NOUT, LERR, OK )
         INFOT = 12
         CALL CUNMTR( 'L', 'U', 'N', 0, 2, A, 1, TAU, C, 1, W, 1, INFO )
         CALL CHKXER( 'CUNMTR', INFOT, NOUT, LERR, OK )
         INFOT = 12
         CALL CUNMTR( 'R', 'U', 'N', 2, 0, A, 1, TAU, C, 2, W, 1, INFO )
         CALL CHKXER( 'CUNMTR', INFOT, NOUT, LERR, OK )
         NT = NT + 10
*
*        CHPTRD
*
         SRNAMT = 'CHPTRD'
         INFOT = 1
         CALL CHPTRD( '/', 0, A, D, E, TAU, INFO )
         CALL CHKXER( 'CHPTRD', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL CHPTRD( 'U', -1, A, D, E, TAU, INFO )
         CALL CHKXER( 'CHPTRD', INFOT, NOUT, LERR, OK )
         NT = NT + 2
*
*        CUPGTR
*
         SRNAMT = 'CUPGTR'
         INFOT = 1
         CALL CUPGTR( '/', 0, A, TAU, Z, 1, W, INFO )
         CALL CHKXER( 'CUPGTR', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL CUPGTR( 'U', -1, A, TAU, Z, 1, W, INFO )
         CALL CHKXER( 'CUPGTR', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL CUPGTR( 'U', 2, A, TAU, Z, 1, W, INFO )
         CALL CHKXER( 'CUPGTR', INFOT, NOUT, LERR, OK )
         NT = NT + 3
*
*        CUPMTR
*
         SRNAMT = 'CUPMTR'
         INFOT = 1
         CALL CUPMTR( '/', 'U', 'N', 0, 0, A, TAU, C, 1, W, INFO )
         CALL CHKXER( 'CUPMTR', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL CUPMTR( 'L', '/', 'N', 0, 0, A, TAU, C, 1, W, INFO )
         CALL CHKXER( 'CUPMTR', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL CUPMTR( 'L', 'U', '/', 0, 0, A, TAU, C, 1, W, INFO )
         CALL CHKXER( 'CUPMTR', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL CUPMTR( 'L', 'U', 'N', -1, 0, A, TAU, C, 1, W, INFO )
         CALL CHKXER( 'CUPMTR', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL CUPMTR( 'L', 'U', 'N', 0, -1, A, TAU, C, 1, W, INFO )
         CALL CHKXER( 'CUPMTR', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL CUPMTR( 'L', 'U', 'N', 2, 0, A, TAU, C, 1, W, INFO )
         CALL CHKXER( 'CUPMTR', INFOT, NOUT, LERR, OK )
         NT = NT + 6
*
*        CPTEQR
*
         SRNAMT = 'CPTEQR'
         INFOT = 1
         CALL CPTEQR( '/', 0, D, E, Z, 1, RW, INFO )
         CALL CHKXER( 'CPTEQR', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL CPTEQR( 'N', -1, D, E, Z, 1, RW, INFO )
         CALL CHKXER( 'CPTEQR', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL CPTEQR( 'V', 2, D, E, Z, 1, RW, INFO )
         CALL CHKXER( 'CPTEQR', INFOT, NOUT, LERR, OK )
         NT = NT + 3
*
*        CSTEIN
*
         SRNAMT = 'CSTEIN'
         INFOT = 1
         CALL CSTEIN( -1, D, E, 0, X, I1, I2, Z, 1, RW, IW, I3, INFO )
         CALL CHKXER( 'CSTEIN', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL CSTEIN( 0, D, E, -1, X, I1, I2, Z, 1, RW, IW, I3, INFO )
         CALL CHKXER( 'CSTEIN', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL CSTEIN( 0, D, E, 1, X, I1, I2, Z, 1, RW, IW, I3, INFO )
         CALL CHKXER( 'CSTEIN', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL CSTEIN( 2, D, E, 0, X, I1, I2, Z, 1, RW, IW, I3, INFO )
         CALL CHKXER( 'CSTEIN', INFOT, NOUT, LERR, OK )
         NT = NT + 4
*
*        CSTEQR
*
         SRNAMT = 'CSTEQR'
         INFOT = 1
         CALL CSTEQR( '/', 0, D, E, Z, 1, RW, INFO )
         CALL CHKXER( 'CSTEQR', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL CSTEQR( 'N', -1, D, E, Z, 1, RW, INFO )
         CALL CHKXER( 'CSTEQR', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL CSTEQR( 'V', 2, D, E, Z, 1, RW, INFO )
         CALL CHKXER( 'CSTEQR', INFOT, NOUT, LERR, OK )
         NT = NT + 3
*
*        CSTEDC
*
         SRNAMT = 'CSTEDC'
         INFOT = 1
         CALL CSTEDC( '/', 0, D, E, Z, 1, W, 1, RW, 1, IW, 1, INFO )
         CALL CHKXER( 'CSTEDC', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL CSTEDC( 'N', -1, D, E, Z, 1, W, 1, RW, 1, IW, 1, INFO )
         CALL CHKXER( 'CSTEDC', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL CSTEDC( 'V', 2, D, E, Z, 1, W, 4, RW, 23, IW, 28, INFO )
         CALL CHKXER( 'CSTEDC', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL CSTEDC( 'N', 2, D, E, Z, 1, W, 0, RW, 1, IW, 1, INFO )
         CALL CHKXER( 'CSTEDC', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL CSTEDC( 'V', 2, D, E, Z, 2, W, 3, RW, 23, IW, 28, INFO )
         CALL CHKXER( 'CSTEDC', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL CSTEDC( 'N', 2, D, E, Z, 1, W, 1, RW, 0, IW, 1, INFO )
         CALL CHKXER( 'CSTEDC', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL CSTEDC( 'I', 2, D, E, Z, 2, W, 1, RW, 16, IW, 12, INFO )
         CALL CHKXER( 'CSTEDC', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL CSTEDC( 'V', 2, D, E, Z, 2, W, 4, RW, 22, IW, 28, INFO )
         CALL CHKXER( 'CSTEDC', INFOT, NOUT, LERR, OK )
         INFOT = 12
         CALL CSTEDC( 'N', 2, D, E, Z, 1, W, 1, RW, 1, IW, 0, INFO )
         CALL CHKXER( 'CSTEDC', INFOT, NOUT, LERR, OK )
         INFOT = 12
         CALL CSTEDC( 'I', 2, D, E, Z, 2, W, 1, RW, 23, IW, 11, INFO )
         CALL CHKXER( 'CSTEDC', INFOT, NOUT, LERR, OK )
         INFOT = 12
         CALL CSTEDC( 'V', 2, D, E, Z, 2, W, 4, RW, 23, IW, 27, INFO )
         CALL CHKXER( 'CSTEDC', INFOT, NOUT, LERR, OK )
         NT = NT + 11
*
*        CHEEVD
*
         SRNAMT = 'CHEEVD'
         INFOT = 1
         CALL CHEEVD( '/', 'U', 0, A, 1, X, W, 1, RW, 1, IW, 1, INFO )
         CALL CHKXER( 'CHEEVD', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL CHEEVD( 'N', '/', 0, A, 1, X, W, 1, RW, 1, IW, 1, INFO )
         CALL CHKXER( 'CHEEVD', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL CHEEVD( 'N', 'U', -1, A, 1, X, W, 1, RW, 1, IW, 1, INFO )
         CALL CHKXER( 'CHEEVD', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL CHEEVD( 'N', 'U', 2, A, 1, X, W, 3, RW, 2, IW, 1, INFO )
         CALL CHKXER( 'CHEEVD', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL CHEEVD( 'N', 'U', 1, A, 1, X, W, 0, RW, 1, IW, 1, INFO )
         CALL CHKXER( 'CHEEVD', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL CHEEVD( 'N', 'U', 2, A, 2, X, W, 2, RW, 2, IW, 1, INFO )
         CALL CHKXER( 'CHEEVD', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL CHEEVD( 'V', 'U', 2, A, 2, X, W, 3, RW, 25, IW, 12, INFO )
         CALL CHKXER( 'CHEEVD', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL CHEEVD( 'N', 'U', 1, A, 1, X, W, 1, RW, 0, IW, 1, INFO )
         CALL CHKXER( 'CHEEVD', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL CHEEVD( 'N', 'U', 2, A, 2, X, W, 3, RW, 1, IW, 1, INFO )
         CALL CHKXER( 'CHEEVD', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL CHEEVD( 'V', 'U', 2, A, 2, X, W, 8, RW, 18, IW, 12, INFO )
         CALL CHKXER( 'CHEEVD', INFOT, NOUT, LERR, OK )
         INFOT = 12
         CALL CHEEVD( 'N', 'U', 1, A, 1, X, W, 1, RW, 1, IW, 0, INFO )
         CALL CHKXER( 'CHEEVD', INFOT, NOUT, LERR, OK )
         INFOT = 12
         CALL CHEEVD( 'V', 'U', 2, A, 2, X, W, 8, RW, 25, IW, 11, INFO )
         CALL CHKXER( 'CHEEVD', INFOT, NOUT, LERR, OK )
         NT = NT + 12
*
*        CHEEV
*
         SRNAMT = 'CHEEV '
         INFOT = 1
         CALL CHEEV( '/', 'U', 0, A, 1, X, W, 1, RW, INFO )
         CALL CHKXER( 'CHEEV ', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL CHEEV( 'N', '/', 0, A, 1, X, W, 1, RW, INFO )
         CALL CHKXER( 'CHEEV ', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL CHEEV( 'N', 'U', -1, A, 1, X, W, 1, RW, INFO )
         CALL CHKXER( 'CHEEV ', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL CHEEV( 'N', 'U', 2, A, 1, X, W, 3, RW, INFO )
         CALL CHKXER( 'CHEEV ', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL CHEEV( 'N', 'U', 2, A, 2, X, W, 2, RW, INFO )
         CALL CHKXER( 'CHEEV ', INFOT, NOUT, LERR, OK )
         NT = NT + 5
*
*        CHEEVX
*
         SRNAMT = 'CHEEVX'
         INFOT = 1
         CALL CHEEVX( '/', 'A', 'U', 0, A, 1, 0.0, 0.0, 0, 0, 0.0, M, X,
     $                Z, 1, W, 1, RW, IW, I3, INFO )
         CALL CHKXER( 'CHEEVX', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL CHEEVX( 'V', '/', 'U', 0, A, 1, 0.0, 1.0, 1, 0, 0.0, M, X,
     $                Z, 1, W, 1, RW, IW, I3, INFO )
         CALL CHKXER( 'CHEEVX', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL CHEEVX( 'V', 'A', '/', 0, A, 1, 0.0, 0.0, 0, 0, 0.0, M, X,
     $                Z, 1, W, 1, RW, IW, I3, INFO )
         INFOT = 4
         CALL CHEEVX( 'V', 'A', 'U', -1, A, 1, 0.0, 0.0, 0, 0, 0.0, M,
     $                X, Z, 1, W, 1, RW, IW, I3, INFO )
         CALL CHKXER( 'CHEEVX', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL CHEEVX( 'V', 'A', 'U', 2, A, 1, 0.0, 0.0, 0, 0, 0.0, M, X,
     $                Z, 2, W, 3, RW, IW, I3, INFO )
         CALL CHKXER( 'CHEEVX', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL CHEEVX( 'V', 'V', 'U', 1, A, 1, 0.0, 0.0, 0, 0, 0.0, M, X,
     $                Z, 1, W, 1, RW, IW, I3, INFO )
         CALL CHKXER( 'CHEEVX', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL CHEEVX( 'V', 'I', 'U', 1, A, 1, 0.0, 0.0, 0, 0, 0.0, M, X,
     $                Z, 1, W, 1, RW, IW, I3, INFO )
         CALL CHKXER( 'CHEEVX', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL CHEEVX( 'V', 'I', 'U', 2, A, 2, 0.0, 0.0, 2, 1, 0.0, M, X,
     $                Z, 2, W, 3, RW, IW, I3, INFO )
         CALL CHKXER( 'CHEEVX', INFOT, NOUT, LERR, OK )
         INFOT = 15
         CALL CHEEVX( 'V', 'A', 'U', 2, A, 2, 0.0, 0.0, 0, 0, 0.0, M, X,
     $                Z, 1, W, 3, RW, IW, I3, INFO )
         CALL CHKXER( 'CHEEVX', INFOT, NOUT, LERR, OK )
         INFOT = 17
         CALL CHEEVX( 'V', 'A', 'U', 2, A, 2, 0.0, 0.0, 0, 0, 0.0, M, X,
     $                Z, 2, W, 2, RW, IW, I1, INFO )
         CALL CHKXER( 'CHEEVX', INFOT, NOUT, LERR, OK )
         NT = NT + 10
*
*        CHEEVR
*
         SRNAMT = 'CHEEVR'
         N = 1
         INFOT = 1
         CALL CHEEVR( '/', 'A', 'U', 0, A, 1, 0.0, 0.0, 1, 1, 0.0, M, R,
     $                Z, 1, IW, Q, 2*N, RW, 24*N, IW( 2*N+1 ), 10*N,
     $                INFO )
         CALL CHKXER( 'CHEEVR', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL CHEEVR( 'V', '/', 'U', 0, A, 1, 0.0, 0.0, 1, 1, 0.0, M, R,
     $                Z, 1, IW, Q, 2*N, RW, 24*N, IW( 2*N+1 ), 10*N,
     $                INFO )
         CALL CHKXER( 'CHEEVR', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL CHEEVR( 'V', 'A', '/', -1, A, 1, 0.0, 0.0, 1, 1, 0.0, M,
     $                R, Z, 1, IW, Q, 2*N, RW, 24*N, IW( 2*N+1 ), 10*N,
     $                INFO )
         CALL CHKXER( 'CHEEVR', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL CHEEVR( 'V', 'A', 'U', -1, A, 1, 0.0, 0.0, 1, 1, 0.0, M,
     $                R, Z, 1, IW, Q, 2*N, RW, 24*N, IW( 2*N+1 ), 10*N,
     $                INFO )
         CALL CHKXER( 'CHEEVR', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL CHEEVR( 'V', 'A', 'U', 2, A, 1, 0.0, 0.0, 1, 1, 0.0, M, R,
     $                Z, 1, IW, Q, 2*N, RW, 24*N, IW( 2*N+1 ), 10*N,
     $                INFO )
         CALL CHKXER( 'CHEEVR', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL CHEEVR( 'V', 'V', 'U', 1, A, 1, 0.0E0, 0.0E0, 1, 1, 0.0,
     $                M, R, Z, 1, IW, Q, 2*N, RW, 24*N, IW( 2*N+1 ),
     $                10*N, INFO )
         CALL CHKXER( 'CHEEVR', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL CHEEVR( 'V', 'I', 'U', 1, A, 1, 0.0E0, 0.0E0, 0, 1, 0.0,
     $                M, R, Z, 1, IW, Q, 2*N, RW, 24*N, IW( 2*N+1 ),
     $                10*N, INFO )
         CALL CHKXER( 'CHEEVR', INFOT, NOUT, LERR, OK )
         INFOT = 10
*
         CALL CHEEVR( 'V', 'I', 'U', 2, A, 2, 0.0E0, 0.0E0, 2, 1, 0.0,
     $                M, R, Z, 1, IW, Q, 2*N, RW, 24*N, IW( 2*N+1 ),
     $                10*N, INFO )
         CALL CHKXER( 'CHEEVR', INFOT, NOUT, LERR, OK )
         INFOT = 15
         CALL CHEEVR( 'V', 'I', 'U', 1, A, 1, 0.0E0, 0.0E0, 1, 1, 0.0,
     $                M, R, Z, 0, IW, Q, 2*N, RW, 24*N, IW( 2*N+1 ),
     $                10*N, INFO )
         CALL CHKXER( 'CHEEVR', INFOT, NOUT, LERR, OK )
         INFOT = 18
         CALL CHEEVR( 'V', 'I', 'U', 1, A, 1, 0.0E0, 0.0E0, 1, 1, 0.0,
     $                M, R, Z, 1, IW, Q, 2*N-1, RW, 24*N, IW( 2*N+1 ),
     $                10*N, INFO )
         CALL CHKXER( 'CHEEVR', INFOT, NOUT, LERR, OK )
         INFOT = 20
         CALL CHEEVR( 'V', 'I', 'U', 1, A, 1, 0.0E0, 0.0E0, 1, 1, 0.0,
     $                M, R, Z, 1, IW, Q, 2*N, RW, 24*N-1, IW( 2*N-1 ),
     $                10*N, INFO )
         CALL CHKXER( 'CHEEVR', INFOT, NOUT, LERR, OK )
         INFOT = 22
         CALL CHEEVR( 'V', 'I', 'U', 1, A, 1, 0.0E0, 0.0E0, 1, 1, 0.0,
     $                M, R, Z, 1, IW, Q, 2*N, RW, 24*N, IW, 10*N-1,
     $                INFO )
         CALL CHKXER( 'CHEEVR', INFOT, NOUT, LERR, OK )
         NT = NT + 12
*
*        CHPEVD
*
         SRNAMT = 'CHPEVD'
         INFOT = 1
         CALL CHPEVD( '/', 'U', 0, A, X, Z, 1, W, 1, RW, 1, IW, 1,
     $                INFO )
         CALL CHKXER( 'CHPEVD', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL CHPEVD( 'N', '/', 0, A, X, Z, 1, W, 1, RW, 1, IW, 1,
     $                INFO )
         CALL CHKXER( 'CHPEVD', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL CHPEVD( 'N', 'U', -1, A, X, Z, 1, W, 1, RW, 1, IW, 1,
     $                INFO )
         CALL CHKXER( 'CHPEVD', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL CHPEVD( 'V', 'U', 2, A, X, Z, 1, W, 4, RW, 25, IW, 12,
     $                INFO )
         CALL CHKXER( 'CHPEVD', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL CHPEVD( 'N', 'U', 1, A, X, Z, 1, W, 0, RW, 1, IW, 1,
     $                INFO )
         CALL CHKXER( 'CHPEVD', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL CHPEVD( 'N', 'U', 2, A, X, Z, 2, W, 1, RW, 2, IW, 1,
     $                INFO )
         CALL CHKXER( 'CHPEVD', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL CHPEVD( 'V', 'U', 2, A, X, Z, 2, W, 2, RW, 25, IW, 12,
     $                INFO )
         CALL CHKXER( 'CHPEVD', INFOT, NOUT, LERR, OK )
         INFOT = 11
         CALL CHPEVD( 'N', 'U', 1, A, X, Z, 1, W, 1, RW, 0, IW, 1,
     $                INFO )
         CALL CHKXER( 'CHPEVD', INFOT, NOUT, LERR, OK )
         INFOT = 11
         CALL CHPEVD( 'N', 'U', 2, A, X, Z, 2, W, 2, RW, 1, IW, 1,
     $                INFO )
         CALL CHKXER( 'CHPEVD', INFOT, NOUT, LERR, OK )
         INFOT = 11
         CALL CHPEVD( 'V', 'U', 2, A, X, Z, 2, W, 4, RW, 18, IW, 12,
     $                INFO )
         CALL CHKXER( 'CHPEVD', INFOT, NOUT, LERR, OK )
         INFOT = 13
         CALL CHPEVD( 'N', 'U', 1, A, X, Z, 1, W, 1, RW, 1, IW, 0,
     $                INFO )
         CALL CHKXER( 'CHPEVD', INFOT, NOUT, LERR, OK )
         INFOT = 13
         CALL CHPEVD( 'N', 'U', 2, A, X, Z, 2, W, 2, RW, 2, IW, 0,
     $                INFO )
         CALL CHKXER( 'CHPEVD', INFOT, NOUT, LERR, OK )
         INFOT = 13
         CALL CHPEVD( 'V', 'U', 2, A, X, Z, 2, W, 4, RW, 25, IW, 2,
     $                INFO )
         CALL CHKXER( 'CHPEVD', INFOT, NOUT, LERR, OK )
         NT = NT + 13
*
*        CHPEV
*
         SRNAMT = 'CHPEV '
         INFOT = 1
         CALL CHPEV( '/', 'U', 0, A, X, Z, 1, W, RW, INFO )
         CALL CHKXER( 'CHPEV ', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL CHPEV( 'N', '/', 0, A, X, Z, 1, W, RW, INFO )
         CALL CHKXER( 'CHPEV ', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL CHPEV( 'N', 'U', -1, A, X, Z, 1, W, RW, INFO )
         CALL CHKXER( 'CHPEV ', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL CHPEV( 'V', 'U', 2, A, X, Z, 1, W, RW, INFO )
         CALL CHKXER( 'CHPEV ', INFOT, NOUT, LERR, OK )
         NT = NT + 4
*
*        CHPEVX
*
         SRNAMT = 'CHPEVX'
         INFOT = 1
         CALL CHPEVX( '/', 'A', 'U', 0, A, 0.0, 0.0, 0, 0, 0.0, M, X, Z,
     $                1, W, RW, IW, I3, INFO )
         CALL CHKXER( 'CHPEVX', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL CHPEVX( 'V', '/', 'U', 0, A, 0.0, 1.0, 1, 0, 0.0, M, X, Z,
     $                1, W, RW, IW, I3, INFO )
         CALL CHKXER( 'CHPEVX', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL CHPEVX( 'V', 'A', '/', 0, A, 0.0, 0.0, 0, 0, 0.0, M, X, Z,
     $                1, W, RW, IW, I3, INFO )
         CALL CHKXER( 'CHPEVX', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL CHPEVX( 'V', 'A', 'U', -1, A, 0.0, 0.0, 0, 0, 0.0, M, X,
     $                Z, 1, W, RW, IW, I3, INFO )
         CALL CHKXER( 'CHPEVX', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL CHPEVX( 'V', 'V', 'U', 1, A, 0.0, 0.0, 0, 0, 0.0, M, X, Z,
     $                1, W, RW, IW, I3, INFO )
         CALL CHKXER( 'CHPEVX', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL CHPEVX( 'V', 'I', 'U', 1, A, 0.0, 0.0, 0, 0, 0.0, M, X, Z,
     $                1, W, RW, IW, I3, INFO )
         CALL CHKXER( 'CHPEVX', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL CHPEVX( 'V', 'I', 'U', 2, A, 0.0, 0.0, 2, 1, 0.0, M, X, Z,
     $                2, W, RW, IW, I3, INFO )
         CALL CHKXER( 'CHPEVX', INFOT, NOUT, LERR, OK )
         INFOT = 14
         CALL CHPEVX( 'V', 'A', 'U', 2, A, 0.0, 0.0, 0, 0, 0.0, M, X, Z,
     $                1, W, RW, IW, I3, INFO )
         CALL CHKXER( 'CHPEVX', INFOT, NOUT, LERR, OK )
         NT = NT + 8
*
*     Test error exits for the HB path.
*
      ELSE IF( LSAMEN( 2, C2, 'HB' ) ) THEN
*
*        CHBTRD
*
         SRNAMT = 'CHBTRD'
         INFOT = 1
         CALL CHBTRD( '/', 'U', 0, 0, A, 1, D, E, Z, 1, W, INFO )
         CALL CHKXER( 'CHBTRD', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL CHBTRD( 'N', '/', 0, 0, A, 1, D, E, Z, 1, W, INFO )
         CALL CHKXER( 'CHBTRD', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL CHBTRD( 'N', 'U', -1, 0, A, 1, D, E, Z, 1, W, INFO )
         CALL CHKXER( 'CHBTRD', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL CHBTRD( 'N', 'U', 0, -1, A, 1, D, E, Z, 1, W, INFO )
         CALL CHKXER( 'CHBTRD', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL CHBTRD( 'N', 'U', 1, 1, A, 1, D, E, Z, 1, W, INFO )
         CALL CHKXER( 'CHBTRD', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL CHBTRD( 'V', 'U', 2, 0, A, 1, D, E, Z, 1, W, INFO )
         CALL CHKXER( 'CHBTRD', INFOT, NOUT, LERR, OK )
         NT = NT + 6
*
*        CHBEVD
*
         SRNAMT = 'CHBEVD'
         INFOT = 1
         CALL CHBEVD( '/', 'U', 0, 0, A, 1, X, Z, 1, W, 1, RW, 1, IW, 1,
     $                INFO )
         CALL CHKXER( 'CHBEVD', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL CHBEVD( 'N', '/', 0, 0, A, 1, X, Z, 1, W, 1, RW, 1, IW, 1,
     $                INFO )
         CALL CHKXER( 'CHBEVD', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL CHBEVD( 'N', 'U', -1, 0, A, 1, X, Z, 1, W, 1, RW, 1, IW,
     $                1, INFO )
         CALL CHKXER( 'CHBEVD', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL CHBEVD( 'N', 'U', 0, -1, A, 1, X, Z, 1, W, 1, RW, 1, IW,
     $                1, INFO )
         CALL CHKXER( 'CHBEVD', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL CHBEVD( 'N', 'U', 2, 1, A, 1, X, Z, 1, W, 2, RW, 2, IW, 1,
     $                INFO )
         CALL CHKXER( 'CHBEVD', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL CHBEVD( 'V', 'U', 2, 1, A, 2, X, Z, 1, W, 8, RW, 25, IW,
     $                12, INFO )
         CALL CHKXER( 'CHBEVD', INFOT, NOUT, LERR, OK )
         INFOT = 11
         CALL CHBEVD( 'N', 'U', 1, 0, A, 1, X, Z, 1, W, 0, RW, 1, IW, 1,
     $                INFO )
         CALL CHKXER( 'CHBEVD', INFOT, NOUT, LERR, OK )
         INFOT = 11
         CALL CHBEVD( 'N', 'U', 2, 1, A, 2, X, Z, 2, W, 1, RW, 2, IW, 1,
     $                INFO )
         CALL CHKXER( 'CHBEVD', INFOT, NOUT, LERR, OK )
         INFOT = 11
         CALL CHBEVD( 'V', 'U', 2, 1, A, 2, X, Z, 2, W, 2, RW, 25, IW,
     $                12, INFO )
         CALL CHKXER( 'CHBEVD', INFOT, NOUT, LERR, OK )
         INFOT = 13
         CALL CHBEVD( 'N', 'U', 1, 0, A, 1, X, Z, 1, W, 1, RW, 0, IW, 1,
     $                INFO )
         CALL CHKXER( 'CHBEVD', INFOT, NOUT, LERR, OK )
         INFOT = 13
         CALL CHBEVD( 'N', 'U', 2, 1, A, 2, X, Z, 2, W, 2, RW, 1, IW, 1,
     $                INFO )
         CALL CHKXER( 'CHBEVD', INFOT, NOUT, LERR, OK )
         INFOT = 13
         CALL CHBEVD( 'V', 'U', 2, 1, A, 2, X, Z, 2, W, 8, RW, 2, IW,
     $                12, INFO )
         CALL CHKXER( 'CHBEVD', INFOT, NOUT, LERR, OK )
         INFOT = 15
         CALL CHBEVD( 'N', 'U', 1, 0, A, 1, X, Z, 1, W, 1, RW, 1, IW, 0,
     $                INFO )
         CALL CHKXER( 'CHBEVD', INFOT, NOUT, LERR, OK )
         INFOT = 15
         CALL CHBEVD( 'N', 'U', 2, 1, A, 2, X, Z, 2, W, 2, RW, 2, IW, 0,
     $                INFO )
         CALL CHKXER( 'CHBEVD', INFOT, NOUT, LERR, OK )
         INFOT = 15
         CALL CHBEVD( 'V', 'U', 2, 1, A, 2, X, Z, 2, W, 8, RW, 25, IW,
     $                2, INFO )
         CALL CHKXER( 'CHBEVD', INFOT, NOUT, LERR, OK )
         NT = NT + 15
*
*        CHBEV
*
         SRNAMT = 'CHBEV '
         INFOT = 1
         CALL CHBEV( '/', 'U', 0, 0, A, 1, X, Z, 1, W, RW, INFO )
         CALL CHKXER( 'CHBEV ', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL CHBEV( 'N', '/', 0, 0, A, 1, X, Z, 1, W, RW, INFO )
         CALL CHKXER( 'CHBEV ', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL CHBEV( 'N', 'U', -1, 0, A, 1, X, Z, 1, W, RW, INFO )
         CALL CHKXER( 'CHBEV ', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL CHBEV( 'N', 'U', 0, -1, A, 1, X, Z, 1, W, RW, INFO )
         CALL CHKXER( 'CHBEV ', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL CHBEV( 'N', 'U', 2, 1, A, 1, X, Z, 1, W, RW, INFO )
         CALL CHKXER( 'CHBEV ', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL CHBEV( 'V', 'U', 2, 0, A, 1, X, Z, 1, W, RW, INFO )
         CALL CHKXER( 'CHBEV ', INFOT, NOUT, LERR, OK )
         NT = NT + 6
*
*        CHBEVX
*
         SRNAMT = 'CHBEVX'
         INFOT = 1
         CALL CHBEVX( '/', 'A', 'U', 0, 0, A, 1, Q, 1, 0.0, 0.0, 0, 0,
     $                0.0, M, X, Z, 1, W, RW, IW, I3, INFO )
         CALL CHKXER( 'CHBEVX', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL CHBEVX( 'V', '/', 'U', 0, 0, A, 1, Q, 1, 0.0, 1.0, 1, 0,
     $                0.0, M, X, Z, 1, W, RW, IW, I3, INFO )
         CALL CHKXER( 'CHBEVX', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL CHBEVX( 'V', 'A', '/', 0, 0, A, 1, Q, 1, 0.0, 0.0, 0, 0,
     $                0.0, M, X, Z, 1, W, RW, IW, I3, INFO )
         INFOT = 4
         CALL CHBEVX( 'V', 'A', 'U', -1, 0, A, 1, Q, 1, 0.0, 0.0, 0, 0,
     $                0.0, M, X, Z, 1, W, RW, IW, I3, INFO )
         CALL CHKXER( 'CHBEVX', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL CHBEVX( 'V', 'A', 'U', 0, -1, A, 1, Q, 1, 0.0, 0.0, 0, 0,
     $                0.0, M, X, Z, 1, W, RW, IW, I3, INFO )
         CALL CHKXER( 'CHBEVX', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL CHBEVX( 'V', 'A', 'U', 2, 1, A, 1, Q, 2, 0.0, 0.0, 0, 0,
     $                0.0, M, X, Z, 2, W, RW, IW, I3, INFO )
         CALL CHKXER( 'CHBEVX', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL CHBEVX( 'V', 'A', 'U', 2, 0, A, 1, Q, 1, 0.0, 0.0, 0, 0,
     $                0.0, M, X, Z, 2, W, RW, IW, I3, INFO )
         CALL CHKXER( 'CHBEVX', INFOT, NOUT, LERR, OK )
         INFOT = 11
         CALL CHBEVX( 'V', 'V', 'U', 1, 0, A, 1, Q, 1, 0.0, 0.0, 0, 0,
     $                0.0, M, X, Z, 1, W, RW, IW, I3, INFO )
         CALL CHKXER( 'CHBEVX', INFOT, NOUT, LERR, OK )
         INFOT = 12
         CALL CHBEVX( 'V', 'I', 'U', 1, 0, A, 1, Q, 1, 0.0, 0.0, 0, 0,
     $                0.0, M, X, Z, 1, W, RW, IW, I3, INFO )
         CALL CHKXER( 'CHBEVX', INFOT, NOUT, LERR, OK )
         INFOT = 13
         CALL CHBEVX( 'V', 'I', 'U', 1, 0, A, 1, Q, 1, 0.0, 0.0, 1, 2,
     $                0.0, M, X, Z, 1, W, RW, IW, I3, INFO )
         CALL CHKXER( 'CHBEVX', INFOT, NOUT, LERR, OK )
         INFOT = 18
         CALL CHBEVX( 'V', 'A', 'U', 2, 0, A, 1, Q, 2, 0.0, 0.0, 0, 0,
     $                0.0, M, X, Z, 1, W, RW, IW, I3, INFO )
         CALL CHKXER( 'CHBEVX', INFOT, NOUT, LERR, OK )
         NT = NT + 11
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
*     End of CERRST
*
      END
