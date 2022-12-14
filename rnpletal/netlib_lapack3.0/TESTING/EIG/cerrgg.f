      SUBROUTINE CERRGG( PATH, NUNIT )
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
*  CERRGG tests the error exits for CGGES, CGGESX, CGGEV, CGGEVX,
*  CGGGLM, CGGHRD, CGGLSE, CGGQRF, CGGRQF, CGGSVD, CGGSVP, CHGEQZ,
*  CTGEVC, CTGEXC, CTGSEN, CTGSJA, CTGSNA, and CTGSYL.
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
      PARAMETER          ( NMAX = 3, LW = 6*NMAX )
      REAL               ONE, ZERO
      PARAMETER          ( ONE = 1.0E+0, ZERO = 0.0E+0 )
*     ..
*     .. Local Scalars ..
      CHARACTER*2        C2
      INTEGER            DUMMYK, DUMMYL, I, IFST, ILST, INFO, J, M,
     $                   NCYCLE, NT, SDIM
      REAL               ANRM, BNRM, DIF, SCALE, TOLA, TOLB
*     ..
*     .. Local Arrays ..
      LOGICAL            BW( NMAX ), SEL( NMAX )
      INTEGER            IW( LW )
      REAL               LS( NMAX ), R1( NMAX ), R2( NMAX ),
     $                   RCE( NMAX ), RCV( NMAX ), RS( NMAX ), RW( LW )
      COMPLEX            A( NMAX, NMAX ), ALPHA( NMAX ),
     $                   B( NMAX, NMAX ), BETA( NMAX ), Q( NMAX, NMAX ),
     $                   TAU( NMAX ), U( NMAX, NMAX ), V( NMAX, NMAX ),
     $                   W( LW ), Z( NMAX, NMAX )
*     ..
*     .. External Functions ..
      LOGICAL            CLCTES, CLCTSX, LSAMEN
      EXTERNAL           CLCTES, CLCTSX, LSAMEN
*     ..
*     .. External Subroutines ..
      EXTERNAL           CGGES, CGGESX, CGGEV, CGGEVX, CGGGLM, CGGHRD,
     $                   CGGLSE, CGGQRF, CGGRQF, CGGSVD, CGGSVP, CHGEQZ,
     $                   CHKXER, CTGEVC, CTGEXC, CTGSEN, CTGSJA, CTGSNA,
     $                   CTGSYL
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
*     .. Executable Statements ..
*
      NOUT = NUNIT
      WRITE( NOUT, FMT = * )
      C2 = PATH( 2: 3 )
*
*     Set the variables to innocuous values.
*
      DO 20 J = 1, NMAX
         SEL( J ) = .TRUE.
         DO 10 I = 1, NMAX
            A( I, J ) = ZERO
            B( I, J ) = ZERO
   10    CONTINUE
   20 CONTINUE
      DO 30 I = 1, NMAX
         A( I, I ) = ONE
         B( I, I ) = ONE
   30 CONTINUE
      OK = .TRUE.
      TOLA = 1.0E0
      TOLB = 1.0E0
      IFST = 1
      ILST = 1
      NT = 0
*
*     Test error exits for the GG path.
*
      IF( LSAMEN( 2, C2, 'GG' ) ) THEN
*
*        CGGHRD
*
         SRNAMT = 'CGGHRD'
         INFOT = 1
         CALL CGGHRD( '/', 'N', 0, 1, 0, A, 1, B, 1, Q, 1, Z, 1, INFO )
         CALL CHKXER( 'CGGHRD', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL CGGHRD( 'N', '/', 0, 1, 0, A, 1, B, 1, Q, 1, Z, 1, INFO )
         CALL CHKXER( 'CGGHRD', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL CGGHRD( 'N', 'N', -1, 0, 0, A, 1, B, 1, Q, 1, Z, 1, INFO )
         CALL CHKXER( 'CGGHRD', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL CGGHRD( 'N', 'N', 0, 0, 0, A, 1, B, 1, Q, 1, Z, 1, INFO )
         CALL CHKXER( 'CGGHRD', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL CGGHRD( 'N', 'N', 0, 1, 1, A, 1, B, 1, Q, 1, Z, 1, INFO )
         CALL CHKXER( 'CGGHRD', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL CGGHRD( 'N', 'N', 2, 1, 1, A, 1, B, 2, Q, 1, Z, 1, INFO )
         CALL CHKXER( 'CGGHRD', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL CGGHRD( 'N', 'N', 2, 1, 1, A, 2, B, 1, Q, 1, Z, 1, INFO )
         CALL CHKXER( 'CGGHRD', INFOT, NOUT, LERR, OK )
         INFOT = 11
         CALL CGGHRD( 'V', 'N', 2, 1, 1, A, 2, B, 2, Q, 1, Z, 1, INFO )
         CALL CHKXER( 'CGGHRD', INFOT, NOUT, LERR, OK )
         INFOT = 13
         CALL CGGHRD( 'N', 'V', 2, 1, 1, A, 2, B, 2, Q, 1, Z, 1, INFO )
         CALL CHKXER( 'CGGHRD', INFOT, NOUT, LERR, OK )
         NT = NT + 9
*
*        CHGEQZ
*
         SRNAMT = 'CHGEQZ'
         INFOT = 1
         CALL CHGEQZ( '/', 'N', 'N', 0, 1, 0, A, 1, B, 1, ALPHA, BETA,
     $                Q, 1, Z, 1, W, 1, RW, INFO )
         CALL CHKXER( 'CHGEQZ', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL CHGEQZ( 'E', '/', 'N', 0, 1, 0, A, 1, B, 1, ALPHA, BETA,
     $                Q, 1, Z, 1, W, 1, RW, INFO )
         CALL CHKXER( 'CHGEQZ', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL CHGEQZ( 'E', 'N', '/', 0, 1, 0, A, 1, B, 1, ALPHA, BETA,
     $                Q, 1, Z, 1, W, 1, RW, INFO )
         CALL CHKXER( 'CHGEQZ', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL CHGEQZ( 'E', 'N', 'N', -1, 0, 0, A, 1, B, 1, ALPHA, BETA,
     $                Q, 1, Z, 1, W, 1, RW, INFO )
         CALL CHKXER( 'CHGEQZ', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL CHGEQZ( 'E', 'N', 'N', 0, 0, 0, A, 1, B, 1, ALPHA, BETA,
     $                Q, 1, Z, 1, W, 1, RW, INFO )
         CALL CHKXER( 'CHGEQZ', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL CHGEQZ( 'E', 'N', 'N', 0, 1, 1, A, 1, B, 1, ALPHA, BETA,
     $                Q, 1, Z, 1, W, 1, RW, INFO )
         CALL CHKXER( 'CHGEQZ', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL CHGEQZ( 'E', 'N', 'N', 2, 1, 1, A, 1, B, 2, ALPHA, BETA,
     $                Q, 1, Z, 1, W, 1, RW, INFO )
         CALL CHKXER( 'CHGEQZ', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL CHGEQZ( 'E', 'N', 'N', 2, 1, 1, A, 2, B, 1, ALPHA, BETA,
     $                Q, 1, Z, 1, W, 1, RW, INFO )
         CALL CHKXER( 'CHGEQZ', INFOT, NOUT, LERR, OK )
         INFOT = 14
         CALL CHGEQZ( 'E', 'V', 'N', 2, 1, 1, A, 2, B, 2, ALPHA, BETA,
     $                Q, 1, Z, 1, W, 1, RW, INFO )
         CALL CHKXER( 'CHGEQZ', INFOT, NOUT, LERR, OK )
         INFOT = 16
         CALL CHGEQZ( 'E', 'N', 'V', 2, 1, 1, A, 2, B, 2, ALPHA, BETA,
     $                Q, 1, Z, 1, W, 1, RW, INFO )
         CALL CHKXER( 'CHGEQZ', INFOT, NOUT, LERR, OK )
         NT = NT + 10
*
*        CTGEVC
*
         SRNAMT = 'CTGEVC'
         INFOT = 1
         CALL CTGEVC( '/', 'A', SEL, 0, A, 1, B, 1, Q, 1, Z, 1, 0, M, W,
     $                RW, INFO )
         CALL CHKXER( 'CTGEVC', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL CTGEVC( 'R', '/', SEL, 0, A, 1, B, 1, Q, 1, Z, 1, 0, M, W,
     $                RW, INFO )
         CALL CHKXER( 'CTGEVC', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL CTGEVC( 'R', 'A', SEL, -1, A, 1, B, 1, Q, 1, Z, 1, 0, M,
     $                W, RW, INFO )
         CALL CHKXER( 'CTGEVC', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL CTGEVC( 'R', 'A', SEL, 2, A, 1, B, 2, Q, 1, Z, 2, 0, M, W,
     $                RW, INFO )
         CALL CHKXER( 'CTGEVC', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL CTGEVC( 'R', 'A', SEL, 2, A, 2, B, 1, Q, 1, Z, 2, 0, M, W,
     $                RW, INFO )
         CALL CHKXER( 'CTGEVC', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL CTGEVC( 'L', 'A', SEL, 2, A, 2, B, 2, Q, 1, Z, 1, 0, M, W,
     $                RW, INFO )
         CALL CHKXER( 'CTGEVC', INFOT, NOUT, LERR, OK )
         INFOT = 12
         CALL CTGEVC( 'R', 'A', SEL, 2, A, 2, B, 2, Q, 1, Z, 1, 0, M, W,
     $                RW, INFO )
         CALL CHKXER( 'CTGEVC', INFOT, NOUT, LERR, OK )
         INFOT = 13
         CALL CTGEVC( 'R', 'A', SEL, 2, A, 2, B, 2, Q, 1, Z, 2, 1, M, W,
     $                RW, INFO )
         CALL CHKXER( 'CTGEVC', INFOT, NOUT, LERR, OK )
         NT = NT + 8
*
*     Test error exits for the GSV path.
*
      ELSE IF( LSAMEN( 3, PATH, 'GSV' ) ) THEN
*
*        CGGSVD
*
         SRNAMT = 'CGGSVD'
         INFOT = 1
         CALL CGGSVD( '/', 'N', 'N', 0, 0, 0, DUMMYK, DUMMYL, A, 1, B,
     $                1, R1, R2, U, 1, V, 1, Q, 1, W, RW, IW, INFO )
         CALL CHKXER( 'CGGSVD', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL CGGSVD( 'N', '/', 'N', 0, 0, 0, DUMMYK, DUMMYL, A, 1, B,
     $                1, R1, R2, U, 1, V, 1, Q, 1, W, RW, IW, INFO )
         CALL CHKXER( 'CGGSVD', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL CGGSVD( 'N', 'N', '/', 0, 0, 0, DUMMYK, DUMMYL, A, 1, B,
     $                1, R1, R2, U, 1, V, 1, Q, 1, W, RW, IW, INFO )
         CALL CHKXER( 'CGGSVD', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL CGGSVD( 'N', 'N', 'N', -1, 0, 0, DUMMYK, DUMMYL, A, 1, B,
     $                1, R1, R2, U, 1, V, 1, Q, 1, W, RW, IW, INFO )
         CALL CHKXER( 'CGGSVD', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL CGGSVD( 'N', 'N', 'N', 0, -1, 0, DUMMYK, DUMMYL, A, 1, B,
     $                1, R1, R2, U, 1, V, 1, Q, 1, W, RW, IW, INFO )
         CALL CHKXER( 'CGGSVD', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL CGGSVD( 'N', 'N', 'N', 0, 0, -1, DUMMYK, DUMMYL, A, 1, B,
     $                1, R1, R2, U, 1, V, 1, Q, 1, W, RW, IW, INFO )
         CALL CHKXER( 'CGGSVD', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL CGGSVD( 'N', 'N', 'N', 0, 0, 0, DUMMYK, DUMMYL, A, 0, B,
     $                1, R1, R2, U, 1, V, 1, Q, 1, W, RW, IW, INFO )
         CALL CHKXER( 'CGGSVD', INFOT, NOUT, LERR, OK )
         INFOT = 12
         CALL CGGSVD( 'N', 'N', 'N', 0, 0, 0, DUMMYK, DUMMYL, A, 1, B,
     $                0, R1, R2, U, 1, V, 1, Q, 1, W, RW, IW, INFO )
         CALL CHKXER( 'CGGSVD', INFOT, NOUT, LERR, OK )
         INFOT = 16
         CALL CGGSVD( 'U', 'N', 'N', 0, 0, 0, DUMMYK, DUMMYL, A, 1, B,
     $                1, R1, R2, U, 0, V, 1, Q, 1, W, RW, IW, INFO )
         CALL CHKXER( 'CGGSVD', INFOT, NOUT, LERR, OK )
         INFOT = 18
         CALL CGGSVD( 'N', 'V', 'N', 0, 0, 0, DUMMYK, DUMMYL, A, 1, B,
     $                1, R1, R2, U, 1, V, 0, Q, 1, W, RW, IW, INFO )
         CALL CHKXER( 'CGGSVD', INFOT, NOUT, LERR, OK )
         INFOT = 20
         CALL CGGSVD( 'N', 'N', 'Q', 0, 0, 0, DUMMYK, DUMMYL, A, 1, B,
     $                1, R1, R2, U, 1, V, 1, Q, 0, W, RW, IW, INFO )
         CALL CHKXER( 'CGGSVD', INFOT, NOUT, LERR, OK )
         NT = NT + 11
*
*        CGGSVP
*
         SRNAMT = 'CGGSVP'
         INFOT = 1
         CALL CGGSVP( '/', 'N', 'N', 0, 0, 0, A, 1, B, 1, TOLA, TOLB,
     $                DUMMYK, DUMMYL, U, 1, V, 1, Q, 1, IW, RW, TAU, W,
     $                INFO )
         CALL CHKXER( 'CGGSVP', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL CGGSVP( 'N', '/', 'N', 0, 0, 0, A, 1, B, 1, TOLA, TOLB,
     $                DUMMYK, DUMMYL, U, 1, V, 1, Q, 1, IW, RW, TAU, W,
     $                INFO )
         CALL CHKXER( 'CGGSVP', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL CGGSVP( 'N', 'N', '/', 0, 0, 0, A, 1, B, 1, TOLA, TOLB,
     $                DUMMYK, DUMMYL, U, 1, V, 1, Q, 1, IW, RW, TAU, W,
     $                INFO )
         CALL CHKXER( 'CGGSVP', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL CGGSVP( 'N', 'N', 'N', -1, 0, 0, A, 1, B, 1, TOLA, TOLB,
     $                DUMMYK, DUMMYL, U, 1, V, 1, Q, 1, IW, RW, TAU, W,
     $                INFO )
         CALL CHKXER( 'CGGSVP', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL CGGSVP( 'N', 'N', 'N', 0, -1, 0, A, 1, B, 1, TOLA, TOLB,
     $                DUMMYK, DUMMYL, U, 1, V, 1, Q, 1, IW, RW, TAU, W,
     $                INFO )
         CALL CHKXER( 'CGGSVP', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL CGGSVP( 'N', 'N', 'N', 0, 0, -1, A, 1, B, 1, TOLA, TOLB,
     $                DUMMYK, DUMMYL, U, 1, V, 1, Q, 1, IW, RW, TAU, W,
     $                INFO )
         CALL CHKXER( 'CGGSVP', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL CGGSVP( 'N', 'N', 'N', 0, 0, 0, A, 0, B, 1, TOLA, TOLB,
     $                DUMMYK, DUMMYL, U, 1, V, 1, Q, 1, IW, RW, TAU, W,
     $                INFO )
         CALL CHKXER( 'CGGSVP', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL CGGSVP( 'N', 'N', 'N', 0, 0, 0, A, 1, B, 0, TOLA, TOLB,
     $                DUMMYK, DUMMYL, U, 1, V, 1, Q, 1, IW, RW, TAU, W,
     $                INFO )
         CALL CHKXER( 'CGGSVP', INFOT, NOUT, LERR, OK )
         INFOT = 16
         CALL CGGSVP( 'U', 'N', 'N', 0, 0, 0, A, 1, B, 1, TOLA, TOLB,
     $                DUMMYK, DUMMYL, U, 0, V, 1, Q, 1, IW, RW, TAU, W,
     $                INFO )
         CALL CHKXER( 'CGGSVP', INFOT, NOUT, LERR, OK )
         INFOT = 18
         CALL CGGSVP( 'N', 'V', 'N', 0, 0, 0, A, 1, B, 1, TOLA, TOLB,
     $                DUMMYK, DUMMYL, U, 1, V, 0, Q, 1, IW, RW, TAU, W,
     $                INFO )
         CALL CHKXER( 'CGGSVP', INFOT, NOUT, LERR, OK )
         INFOT = 20
         CALL CGGSVP( 'N', 'N', 'Q', 0, 0, 0, A, 1, B, 1, TOLA, TOLB,
     $                DUMMYK, DUMMYL, U, 1, V, 1, Q, 0, IW, RW, TAU, W,
     $                INFO )
         CALL CHKXER( 'CGGSVP', INFOT, NOUT, LERR, OK )
         NT = NT + 11
*
*        CTGSJA
*
         SRNAMT = 'CTGSJA'
         INFOT = 1
         CALL CTGSJA( '/', 'N', 'N', 0, 0, 0, DUMMYK, DUMMYL, A, 1, B,
     $                1, TOLA, TOLB, R1, R2, U, 1, V, 1, Q, 1, W,
     $                NCYCLE, INFO )
         CALL CHKXER( 'CTGSJA', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL CTGSJA( 'N', '/', 'N', 0, 0, 0, DUMMYK, DUMMYL, A, 1, B,
     $                1, TOLA, TOLB, R1, R2, U, 1, V, 1, Q, 1, W,
     $                NCYCLE, INFO )
         CALL CHKXER( 'CTGSJA', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL CTGSJA( 'N', 'N', '/', 0, 0, 0, DUMMYK, DUMMYL, A, 1, B,
     $                1, TOLA, TOLB, R1, R2, U, 1, V, 1, Q, 1, W,
     $                NCYCLE, INFO )
         CALL CHKXER( 'CTGSJA', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL CTGSJA( 'N', 'N', 'N', -1, 0, 0, DUMMYK, DUMMYL, A, 1, B,
     $                1, TOLA, TOLB, R1, R2, U, 1, V, 1, Q, 1, W,
     $                NCYCLE, INFO )
         CALL CHKXER( 'CTGSJA', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL CTGSJA( 'N', 'N', 'N', 0, -1, 0, DUMMYK, DUMMYL, A, 1, B,
     $                1, TOLA, TOLB, R1, R2, U, 1, V, 1, Q, 1, W,
     $                NCYCLE, INFO )
         CALL CHKXER( 'CTGSJA', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL CTGSJA( 'N', 'N', 'N', 0, 0, -1, DUMMYK, DUMMYL, A, 1, B,
     $                1, TOLA, TOLB, R1, R2, U, 1, V, 1, Q, 1, W,
     $                NCYCLE, INFO )
         CALL CHKXER( 'CTGSJA', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL CTGSJA( 'N', 'N', 'N', 0, 0, 0, DUMMYK, DUMMYL, A, 0, B,
     $                1, TOLA, TOLB, R1, R2, U, 1, V, 1, Q, 1, W,
     $                NCYCLE, INFO )
         CALL CHKXER( 'CTGSJA', INFOT, NOUT, LERR, OK )
         INFOT = 12
         CALL CTGSJA( 'N', 'N', 'N', 0, 0, 0, DUMMYK, DUMMYL, A, 1, B,
     $                0, TOLA, TOLB, R1, R2, U, 1, V, 1, Q, 1, W,
     $                NCYCLE, INFO )
         CALL CHKXER( 'CTGSJA', INFOT, NOUT, LERR, OK )
         INFOT = 18
         CALL CTGSJA( 'U', 'N', 'N', 0, 0, 0, DUMMYK, DUMMYL, A, 1, B,
     $                1, TOLA, TOLB, R1, R2, U, 0, V, 1, Q, 1, W,
     $                NCYCLE, INFO )
         CALL CHKXER( 'CTGSJA', INFOT, NOUT, LERR, OK )
         INFOT = 20
         CALL CTGSJA( 'N', 'V', 'N', 0, 0, 0, DUMMYK, DUMMYL, A, 1, B,
     $                1, TOLA, TOLB, R1, R2, U, 1, V, 0, Q, 1, W,
     $                NCYCLE, INFO )
         CALL CHKXER( 'CTGSJA', INFOT, NOUT, LERR, OK )
         INFOT = 22
         CALL CTGSJA( 'N', 'N', 'Q', 0, 0, 0, DUMMYK, DUMMYL, A, 1, B,
     $                1, TOLA, TOLB, R1, R2, U, 1, V, 1, Q, 0, W,
     $                NCYCLE, INFO )
         CALL CHKXER( 'CTGSJA', INFOT, NOUT, LERR, OK )
         NT = NT + 11
*
*     Test error exits for the GLM path.
*
      ELSE IF( LSAMEN( 3, PATH, 'GLM' ) ) THEN
*
*        CGGGLM
*
         SRNAMT = 'CGGGLM'
         INFOT = 1
         CALL CGGGLM( -1, 0, 0, A, 1, B, 1, TAU, ALPHA, BETA, W, LW,
     $                INFO )
         CALL CHKXER( 'CGGGLM', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL CGGGLM( 0, -1, 0, A, 1, B, 1, TAU, ALPHA, BETA, W, LW,
     $                INFO )
         CALL CHKXER( 'CGGGLM', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL CGGGLM( 0, 1, 0, A, 1, B, 1, TAU, ALPHA, BETA, W, LW,
     $                INFO )
         CALL CHKXER( 'CGGGLM', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL CGGGLM( 0, 0, -1, A, 1, B, 1, TAU, ALPHA, BETA, W, LW,
     $                INFO )
         CALL CHKXER( 'CGGGLM', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL CGGGLM( 1, 0, 0, A, 1, B, 1, TAU, ALPHA, BETA, W, LW,
     $                INFO )
         CALL CHKXER( 'CGGGLM', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL CGGGLM( 0, 0, 0, A, 0, B, 1, TAU, ALPHA, BETA, W, LW,
     $                INFO )
         CALL CHKXER( 'CGGGLM', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL CGGGLM( 0, 0, 0, A, 1, B, 0, TAU, ALPHA, BETA, W, LW,
     $                INFO )
         CALL CHKXER( 'CGGGLM', INFOT, NOUT, LERR, OK )
         INFOT = 12
         CALL CGGGLM( 1, 1, 1, A, 1, B, 1, TAU, ALPHA, BETA, W, 1,
     $                INFO )
         CALL CHKXER( 'CGGGLM', INFOT, NOUT, LERR, OK )
         NT = NT + 8
*
*     Test error exits for the LSE path.
*
      ELSE IF( LSAMEN( 3, PATH, 'LSE' ) ) THEN
*
*        CGGLSE
*
         SRNAMT = 'CGGLSE'
         INFOT = 1
         CALL CGGLSE( -1, 0, 0, A, 1, B, 1, TAU, ALPHA, BETA, W, LW,
     $                INFO )
         CALL CHKXER( 'CGGLSE', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL CGGLSE( 0, -1, 0, A, 1, B, 1, TAU, ALPHA, BETA, W, LW,
     $                INFO )
         CALL CHKXER( 'CGGLSE', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL CGGLSE( 0, 0, -1, A, 1, B, 1, TAU, ALPHA, BETA, W, LW,
     $                INFO )
         CALL CHKXER( 'CGGLSE', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL CGGLSE( 0, 0, 1, A, 1, B, 1, TAU, ALPHA, BETA, W, LW,
     $                INFO )
         CALL CHKXER( 'CGGLSE', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL CGGLSE( 0, 1, 0, A, 1, B, 1, TAU, ALPHA, BETA, W, LW,
     $                INFO )
         CALL CHKXER( 'CGGLSE', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL CGGLSE( 0, 0, 0, A, 0, B, 1, TAU, ALPHA, BETA, W, LW,
     $                INFO )
         CALL CHKXER( 'CGGLSE', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL CGGLSE( 0, 0, 0, A, 1, B, 0, TAU, ALPHA, BETA, W, LW,
     $                INFO )
         CALL CHKXER( 'CGGLSE', INFOT, NOUT, LERR, OK )
         INFOT = 12
         CALL CGGLSE( 1, 1, 1, A, 1, B, 1, TAU, ALPHA, BETA, W, 1,
     $                INFO )
         CALL CHKXER( 'CGGLSE', INFOT, NOUT, LERR, OK )
         NT = NT + 8
*
*     Test error exits for the GQR path.
*
      ELSE IF( LSAMEN( 3, PATH, 'GQR' ) ) THEN
*
*        CGGQRF
*
         SRNAMT = 'CGGQRF'
         INFOT = 1
         CALL CGGQRF( -1, 0, 0, A, 1, ALPHA, B, 1, BETA, W, LW, INFO )
         CALL CHKXER( 'CGGQRF', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL CGGQRF( 0, -1, 0, A, 1, ALPHA, B, 1, BETA, W, LW, INFO )
         CALL CHKXER( 'CGGQRF', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL CGGQRF( 0, 0, -1, A, 1, ALPHA, B, 1, BETA, W, LW, INFO )
         CALL CHKXER( 'CGGQRF', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL CGGQRF( 0, 0, 0, A, 0, ALPHA, B, 1, BETA, W, LW, INFO )
         CALL CHKXER( 'CGGQRF', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL CGGQRF( 0, 0, 0, A, 1, ALPHA, B, 0, BETA, W, LW, INFO )
         CALL CHKXER( 'CGGQRF', INFOT, NOUT, LERR, OK )
         INFOT = 11
         CALL CGGQRF( 1, 1, 2, A, 1, ALPHA, B, 1, BETA, W, 1, INFO )
         CALL CHKXER( 'CGGQRF', INFOT, NOUT, LERR, OK )
         NT = NT + 6
*
*        CGGRQF
*
         SRNAMT = 'CGGRQF'
         INFOT = 1
         CALL CGGRQF( -1, 0, 0, A, 1, ALPHA, B, 1, BETA, W, LW, INFO )
         CALL CHKXER( 'CGGRQF', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL CGGRQF( 0, -1, 0, A, 1, ALPHA, B, 1, BETA, W, LW, INFO )
         CALL CHKXER( 'CGGRQF', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL CGGRQF( 0, 0, -1, A, 1, ALPHA, B, 1, BETA, W, LW, INFO )
         CALL CHKXER( 'CGGRQF', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL CGGRQF( 0, 0, 0, A, 0, ALPHA, B, 1, BETA, W, LW, INFO )
         CALL CHKXER( 'CGGRQF', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL CGGRQF( 0, 0, 0, A, 1, ALPHA, B, 0, BETA, W, LW, INFO )
         CALL CHKXER( 'CGGRQF', INFOT, NOUT, LERR, OK )
         INFOT = 11
         CALL CGGRQF( 1, 1, 2, A, 1, ALPHA, B, 1, BETA, W, 1, INFO )
         CALL CHKXER( 'CGGRQF', INFOT, NOUT, LERR, OK )
         NT = NT + 6
*
*     Test error exits for the CGS, CGV, CGX, and CXV paths.
*
      ELSE IF( LSAMEN( 3, PATH, 'CGS' ) .OR.
     $         LSAMEN( 3, PATH, 'CGV' ) .OR.
     $         LSAMEN( 3, PATH, 'CGX' ) .OR. LSAMEN( 3, PATH, 'CXV' ) )
     $          THEN
*
*        CGGES
*
         SRNAMT = 'CGGES '
         INFOT = 1
         CALL CGGES( '/', 'N', 'S', CLCTES, 1, A, 1, B, 1, SDIM, ALPHA,
     $               BETA, Q, 1, U, 1, W, 1, RW, BW, INFO )
         CALL CHKXER( 'CGGES ', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL CGGES( 'N', '/', 'S', CLCTES, 1, A, 1, B, 1, SDIM, ALPHA,
     $               BETA, Q, 1, U, 1, W, 1, RW, BW, INFO )
         CALL CHKXER( 'CGGES ', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL CGGES( 'N', 'V', '/', CLCTES, 1, A, 1, B, 1, SDIM, ALPHA,
     $               BETA, Q, 1, U, 1, W, 1, RW, BW, INFO )
         CALL CHKXER( 'CGGES ', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL CGGES( 'N', 'V', 'S', CLCTES, -1, A, 1, B, 1, SDIM, ALPHA,
     $               BETA, Q, 1, U, 1, W, 1, RW, BW, INFO )
         CALL CHKXER( 'CGGES ', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL CGGES( 'N', 'V', 'S', CLCTES, 1, A, 0, B, 1, SDIM, ALPHA,
     $               BETA, Q, 1, U, 1, W, 1, RW, BW, INFO )
         CALL CHKXER( 'CGGES ', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL CGGES( 'N', 'V', 'S', CLCTES, 1, A, 1, B, 0, SDIM, ALPHA,
     $               BETA, Q, 1, U, 1, W, 1, RW, BW, INFO )
         CALL CHKXER( 'CGGES ', INFOT, NOUT, LERR, OK )
         INFOT = 14
         CALL CGGES( 'N', 'V', 'S', CLCTES, 1, A, 1, B, 1, SDIM, ALPHA,
     $               BETA, Q, 0, U, 1, W, 1, RW, BW, INFO )
         CALL CHKXER( 'CGGES ', INFOT, NOUT, LERR, OK )
         INFOT = 14
         CALL CGGES( 'V', 'V', 'S', CLCTES, 2, A, 2, B, 2, SDIM, ALPHA,
     $               BETA, Q, 1, U, 2, W, 1, RW, BW, INFO )
         CALL CHKXER( 'CGGES ', INFOT, NOUT, LERR, OK )
         INFOT = 16
         CALL CGGES( 'N', 'V', 'S', CLCTES, 1, A, 1, B, 1, SDIM, ALPHA,
     $               BETA, Q, 1, U, 0, W, 1, RW, BW, INFO )
         CALL CHKXER( 'CGGES ', INFOT, NOUT, LERR, OK )
         INFOT = 16
         CALL CGGES( 'V', 'V', 'S', CLCTES, 2, A, 2, B, 2, SDIM, ALPHA,
     $               BETA, Q, 2, U, 1, W, 1, RW, BW, INFO )
         CALL CHKXER( 'CGGES ', INFOT, NOUT, LERR, OK )
         INFOT = 18
         CALL CGGES( 'V', 'V', 'S', CLCTES, 2, A, 2, B, 2, SDIM, ALPHA,
     $               BETA, Q, 2, U, 2, W, 1, RW, BW, INFO )
         CALL CHKXER( 'CGGES ', INFOT, NOUT, LERR, OK )
         NT = NT + 11
*
*        CGGESX
*
         SRNAMT = 'CGGESX'
         INFOT = 1
         CALL CGGESX( '/', 'N', 'S', CLCTSX, 'N', 1, A, 1, B, 1, SDIM,
     $                ALPHA, BETA, Q, 1, U, 1, RCE, RCV, W, 1, RW, IW,
     $                1, BW, INFO )
         CALL CHKXER( 'CGGESX', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL CGGESX( 'N', '/', 'S', CLCTSX, 'N', 1, A, 1, B, 1, SDIM,
     $                ALPHA, BETA, Q, 1, U, 1, RCE, RCV, W, 1, RW, IW,
     $                1, BW, INFO )
         CALL CHKXER( 'CGGESX', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL CGGESX( 'V', 'V', '/', CLCTSX, 'N', 1, A, 1, B, 1, SDIM,
     $                ALPHA, BETA, Q, 1, U, 1, RCE, RCV, W, 1, RW, IW,
     $                1, BW, INFO )
         CALL CHKXER( 'CGGESX', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL CGGESX( 'V', 'V', 'S', CLCTSX, '/', 1, A, 1, B, 1, SDIM,
     $                ALPHA, BETA, Q, 1, U, 1, RCE, RCV, W, 1, RW, IW,
     $                1, BW, INFO )
         CALL CHKXER( 'CGGESX', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL CGGESX( 'V', 'V', 'S', CLCTSX, 'B', -1, A, 1, B, 1, SDIM,
     $                ALPHA, BETA, Q, 1, U, 1, RCE, RCV, W, 1, RW, IW,
     $                1, BW, INFO )
         CALL CHKXER( 'CGGESX', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL CGGESX( 'V', 'V', 'S', CLCTSX, 'B', 1, A, 0, B, 1, SDIM,
     $                ALPHA, BETA, Q, 1, U, 1, RCE, RCV, W, 1, RW, IW,
     $                1, BW, INFO )
         CALL CHKXER( 'CGGESX', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL CGGESX( 'V', 'V', 'S', CLCTSX, 'B', 1, A, 1, B, 0, SDIM,
     $                ALPHA, BETA, Q, 1, U, 1, RCE, RCV, W, 1, RW, IW,
     $                1, BW, INFO )
         CALL CHKXER( 'CGGESX', INFOT, NOUT, LERR, OK )
         INFOT = 15
         CALL CGGESX( 'V', 'V', 'S', CLCTSX, 'B', 1, A, 1, B, 1, SDIM,
     $                ALPHA, BETA, Q, 0, U, 1, RCE, RCV, W, 1, RW, IW,
     $                1, BW, INFO )
         CALL CHKXER( 'CGGESX', INFOT, NOUT, LERR, OK )
         INFOT = 15
         CALL CGGESX( 'V', 'V', 'S', CLCTSX, 'B', 2, A, 2, B, 2, SDIM,
     $                ALPHA, BETA, Q, 1, U, 1, RCE, RCV, W, 1, RW, IW,
     $                1, BW, INFO )
         CALL CHKXER( 'CGGESX', INFOT, NOUT, LERR, OK )
         INFOT = 17
         CALL CGGESX( 'V', 'V', 'S', CLCTSX, 'B', 1, A, 1, B, 1, SDIM,
     $                ALPHA, BETA, Q, 1, U, 0, RCE, RCV, W, 1, RW, IW,
     $                1, BW, INFO )
         CALL CHKXER( 'CGGESX', INFOT, NOUT, LERR, OK )
         INFOT = 17
         CALL CGGESX( 'V', 'V', 'S', CLCTSX, 'B', 2, A, 2, B, 2, SDIM,
     $                ALPHA, BETA, Q, 2, U, 1, RCE, RCV, W, 1, RW, IW,
     $                1, BW, INFO )
         CALL CHKXER( 'CGGESX', INFOT, NOUT, LERR, OK )
         INFOT = 21
         CALL CGGESX( 'V', 'V', 'S', CLCTSX, 'B', 2, A, 2, B, 2, SDIM,
     $                ALPHA, BETA, Q, 2, U, 2, RCE, RCV, W, 1, RW, IW,
     $                1, BW, INFO )
         CALL CHKXER( 'CGGESX', INFOT, NOUT, LERR, OK )
         INFOT = 24
         CALL CGGESX( 'V', 'V', 'S', CLCTSX, 'V', 1, A, 1, B, 1, SDIM,
     $                ALPHA, BETA, Q, 1, U, 1, RCE, RCV, W, 32, RW, IW,
     $                -1, BW, INFO )
         CALL CHKXER( 'CGGESX', INFOT, NOUT, LERR, OK )
         NT = NT + 13
*
*        CGGEV
*
         SRNAMT = 'CGGEV '
         INFOT = 1
         CALL CGGEV( '/', 'N', 1, A, 1, B, 1, ALPHA, BETA, Q, 1, U, 1,
     $               W, 1, RW, INFO )
         CALL CHKXER( 'CGGEV ', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL CGGEV( 'N', '/', 1, A, 1, B, 1, ALPHA, BETA, Q, 1, U, 1,
     $               W, 1, RW, INFO )
         CALL CHKXER( 'CGGEV ', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL CGGEV( 'V', 'V', -1, A, 1, B, 1, ALPHA, BETA, Q, 1, U, 1,
     $               W, 1, RW, INFO )
         CALL CHKXER( 'CGGEV ', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL CGGEV( 'V', 'V', 1, A, 0, B, 1, ALPHA, BETA, Q, 1, U, 1,
     $               W, 1, RW, INFO )
         CALL CHKXER( 'CGGEV ', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL CGGEV( 'V', 'V', 1, A, 1, B, 0, ALPHA, BETA, Q, 1, U, 1,
     $               W, 1, RW, INFO )
         CALL CHKXER( 'CGGEV ', INFOT, NOUT, LERR, OK )
         INFOT = 11
         CALL CGGEV( 'N', 'V', 1, A, 1, B, 1, ALPHA, BETA, Q, 0, U, 1,
     $               W, 1, RW, INFO )
         CALL CHKXER( 'CGGEV ', INFOT, NOUT, LERR, OK )
         INFOT = 11
         CALL CGGEV( 'V', 'V', 2, A, 2, B, 2, ALPHA, BETA, Q, 1, U, 2,
     $               W, 1, RW, INFO )
         CALL CHKXER( 'CGGEV ', INFOT, NOUT, LERR, OK )
         INFOT = 13
         CALL CGGEV( 'V', 'N', 2, A, 2, B, 2, ALPHA, BETA, Q, 2, U, 0,
     $               W, 1, RW, INFO )
         CALL CHKXER( 'CGGEV ', INFOT, NOUT, LERR, OK )
         INFOT = 13
         CALL CGGEV( 'V', 'V', 2, A, 2, B, 2, ALPHA, BETA, Q, 2, U, 1,
     $               W, 1, RW, INFO )
         CALL CHKXER( 'CGGEV ', INFOT, NOUT, LERR, OK )
         INFOT = 15
         CALL CGGEV( 'V', 'V', 1, A, 1, B, 1, ALPHA, BETA, Q, 1, U, 1,
     $               W, 1, RW, INFO )
         CALL CHKXER( 'CGGEV ', INFOT, NOUT, LERR, OK )
         NT = NT + 10
*
*        CGGEVX
*
         SRNAMT = 'CGGEVX'
         INFOT = 1
         CALL CGGEVX( '/', 'N', 'N', 'N', 1, A, 1, B, 1, ALPHA, BETA, Q,
     $                1, U, 1, 1, 1, LS, RS, ANRM, BNRM, RCE, RCV, W, 1,
     $                RW, IW, BW, INFO )
         CALL CHKXER( 'CGGEVX', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL CGGEVX( 'N', '/', 'N', 'N', 1, A, 1, B, 1, ALPHA, BETA, Q,
     $                1, U, 1, 1, 1, LS, RS, ANRM, BNRM, RCE, RCV, W, 1,
     $                RW, IW, BW, INFO )
         CALL CHKXER( 'CGGEVX', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL CGGEVX( 'N', 'N', '/', 'N', 1, A, 1, B, 1, ALPHA, BETA, Q,
     $                1, U, 1, 1, 1, LS, RS, ANRM, BNRM, RCE, RCV, W, 1,
     $                RW, IW, BW, INFO )
         CALL CHKXER( 'CGGEVX', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL CGGEVX( 'N', 'N', 'N', '/', 1, A, 1, B, 1, ALPHA, BETA, Q,
     $                1, U, 1, 1, 1, LS, RS, ANRM, BNRM, RCE, RCV, W, 1,
     $                RW, IW, BW, INFO )
         CALL CHKXER( 'CGGEVX', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL CGGEVX( 'N', 'N', 'N', 'N', -1, A, 1, B, 1, ALPHA, BETA,
     $                Q, 1, U, 1, 1, 1, LS, RS, ANRM, BNRM, RCE, RCV, W,
     $                1, RW, IW, BW, INFO )
         CALL CHKXER( 'CGGEVX', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL CGGEVX( 'N', 'N', 'N', 'N', 1, A, 0, B, 1, ALPHA, BETA, Q,
     $                1, U, 1, 1, 1, LS, RS, ANRM, BNRM, RCE, RCV, W, 1,
     $                RW, IW, BW, INFO )
         CALL CHKXER( 'CGGEVX', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL CGGEVX( 'N', 'N', 'N', 'N', 1, A, 1, B, 0, ALPHA, BETA, Q,
     $                1, U, 1, 1, 1, LS, RS, ANRM, BNRM, RCE, RCV, W, 1,
     $                RW, IW, BW, INFO )
         CALL CHKXER( 'CGGEVX', INFOT, NOUT, LERR, OK )
         INFOT = 13
         CALL CGGEVX( 'N', 'N', 'N', 'N', 1, A, 1, B, 1, ALPHA, BETA, Q,
     $                0, U, 1, 1, 1, LS, RS, ANRM, BNRM, RCE, RCV, W, 1,
     $                RW, IW, BW, INFO )
         CALL CHKXER( 'CGGEVX', INFOT, NOUT, LERR, OK )
         INFOT = 13
         CALL CGGEVX( 'N', 'V', 'N', 'N', 2, A, 2, B, 2, ALPHA, BETA, Q,
     $                1, U, 2, 1, 2, LS, RS, ANRM, BNRM, RCE, RCV, W, 1,
     $                RW, IW, BW, INFO )
         CALL CHKXER( 'CGGEVX', INFOT, NOUT, LERR, OK )
         INFOT = 15
         CALL CGGEVX( 'N', 'N', 'N', 'N', 1, A, 1, B, 1, ALPHA, BETA, Q,
     $                1, U, 0, 1, 1, LS, RS, ANRM, BNRM, RCE, RCV, W, 1,
     $                RW, IW, BW, INFO )
         CALL CHKXER( 'CGGEVX', INFOT, NOUT, LERR, OK )
         INFOT = 15
         CALL CGGEVX( 'N', 'N', 'V', 'N', 2, A, 2, B, 2, ALPHA, BETA, Q,
     $                2, U, 1, 1, 2, LS, RS, ANRM, BNRM, RCE, RCV, W, 1,
     $                RW, IW, BW, INFO )
         CALL CHKXER( 'CGGEVX', INFOT, NOUT, LERR, OK )
         INFOT = 25
         CALL CGGEVX( 'N', 'N', 'V', 'N', 2, A, 2, B, 2, ALPHA, BETA, Q,
     $                2, U, 2, 1, 2, LS, RS, ANRM, BNRM, RCE, RCV, W, 0,
     $                RW, IW, BW, INFO )
         CALL CHKXER( 'CGGEVX', INFOT, NOUT, LERR, OK )
         NT = NT + 12
*
*        CTGEXC
*
         SRNAMT = 'CTGEXC'
         INFOT = 3
         CALL CTGEXC( .TRUE., .TRUE., -1, A, 1, B, 1, Q, 1, Z, 1, IFST,
     $                ILST, INFO )
         CALL CHKXER( 'CTGEXC', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL CTGEXC( .TRUE., .TRUE., 1, A, 0, B, 1, Q, 1, Z, 1, IFST,
     $                ILST, INFO )
         CALL CHKXER( 'CTGEXC', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL CTGEXC( .TRUE., .TRUE., 1, A, 1, B, 0, Q, 1, Z, 1, IFST,
     $                ILST, INFO )
         CALL CHKXER( 'CTGEXC', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL CTGEXC( .FALSE., .TRUE., 1, A, 1, B, 1, Q, 0, Z, 1, IFST,
     $                ILST, INFO )
         CALL CHKXER( 'CTGEXC', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL CTGEXC( .TRUE., .TRUE., 1, A, 1, B, 1, Q, 0, Z, 1, IFST,
     $                ILST, INFO )
         CALL CHKXER( 'CTGEXC', INFOT, NOUT, LERR, OK )
         INFOT = 11
         CALL CTGEXC( .TRUE., .FALSE., 1, A, 1, B, 1, Q, 1, Z, 0, IFST,
     $                ILST, INFO )
         CALL CHKXER( 'CTGEXC', INFOT, NOUT, LERR, OK )
         INFOT = 11
         CALL CTGEXC( .TRUE., .TRUE., 1, A, 1, B, 1, Q, 1, Z, 0, IFST,
     $                ILST, INFO )
         CALL CHKXER( 'CTGEXC', INFOT, NOUT, LERR, OK )
         NT = NT + 7
*
*        CTGSEN
*
         SRNAMT = 'CTGSEN'
         INFOT = 1
         CALL CTGSEN( -1, .TRUE., .TRUE., SEL, 1, A, 1, B, 1, ALPHA,
     $                BETA, Q, 1, Z, 1, M, TOLA, TOLB, RCV, W, 1, IW, 1,
     $                INFO )
         CALL CHKXER( 'CTGSEN', INFOT, NOUT, LERR, OK )
         INFOT = 5
         CALL CTGSEN( 1, .TRUE., .TRUE., SEL, -1, A, 1, B, 1, ALPHA,
     $                BETA, Q, 1, Z, 1, M, TOLA, TOLB, RCV, W, 1, IW, 1,
     $                INFO )
         CALL CHKXER( 'CTGSEN', INFOT, NOUT, LERR, OK )
         INFOT = 7
         CALL CTGSEN( 1, .TRUE., .TRUE., SEL, 1, A, 0, B, 1, ALPHA,
     $                BETA, Q, 1, Z, 1, M, TOLA, TOLB, RCV, W, 1, IW, 1,
     $                INFO )
         CALL CHKXER( 'CTGSEN', INFOT, NOUT, LERR, OK )
         INFOT = 9
         CALL CTGSEN( 1, .TRUE., .TRUE., SEL, 1, A, 1, B, 0, ALPHA,
     $                BETA, Q, 1, Z, 1, M, TOLA, TOLB, RCV, W, 1, IW, 1,
     $                INFO )
         CALL CHKXER( 'CTGSEN', INFOT, NOUT, LERR, OK )
         INFOT = 13
         CALL CTGSEN( 1, .TRUE., .TRUE., SEL, 1, A, 1, B, 1, ALPHA,
     $                BETA, Q, 0, Z, 1, M, TOLA, TOLB, RCV, W, 1, IW, 1,
     $                INFO )
         CALL CHKXER( 'CTGSEN', INFOT, NOUT, LERR, OK )
         INFOT = 15
         CALL CTGSEN( 1, .TRUE., .TRUE., SEL, 1, A, 1, B, 1, ALPHA,
     $                BETA, Q, 1, Z, 0, M, TOLA, TOLB, RCV, W, 1, IW, 1,
     $                INFO )
         CALL CHKXER( 'CTGSEN', INFOT, NOUT, LERR, OK )
         INFOT = 21
         CALL CTGSEN( 3, .TRUE., .TRUE., SEL, 1, A, 1, B, 1, ALPHA,
     $                BETA, Q, 1, Z, 1, M, TOLA, TOLB, RCV, W, -5, IW,
     $                1, INFO )
         CALL CHKXER( 'CTGSEN', INFOT, NOUT, LERR, OK )
         INFOT = 23
         CALL CTGSEN( 0, .TRUE., .TRUE., SEL, 1, A, 1, B, 1, ALPHA,
     $                BETA, Q, 1, Z, 1, M, TOLA, TOLB, RCV, W, 20, IW,
     $                0, INFO )
         CALL CHKXER( 'CTGSEN', INFOT, NOUT, LERR, OK )
         INFOT = 23
         CALL CTGSEN( 1, .TRUE., .TRUE., SEL, 1, A, 1, B, 1, ALPHA,
     $                BETA, Q, 1, Z, 1, M, TOLA, TOLB, RCV, W, 20, IW,
     $                0, INFO )
         CALL CHKXER( 'CTGSEN', INFOT, NOUT, LERR, OK )
         INFOT = 23
         CALL CTGSEN( 5, .TRUE., .TRUE., SEL, 1, A, 1, B, 1, ALPHA,
     $                BETA, Q, 1, Z, 1, M, TOLA, TOLB, RCV, W, 20, IW,
     $                1, INFO )
         CALL CHKXER( 'CTGSEN', INFOT, NOUT, LERR, OK )
         NT = NT + 11
*
*        CTGSNA
*
         SRNAMT = 'CTGSNA'
         INFOT = 1
         CALL CTGSNA( '/', 'A', SEL, 1, A, 1, B, 1, Q, 1, U, 1, R1, R2,
     $                1, M, W, 1, IW, INFO )
         CALL CHKXER( 'CTGSNA', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL CTGSNA( 'B', '/', SEL, 1, A, 1, B, 1, Q, 1, U, 1, R1, R2,
     $                1, M, W, 1, IW, INFO )
         CALL CHKXER( 'CTGSNA', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL CTGSNA( 'B', 'A', SEL, -1, A, 1, B, 1, Q, 1, U, 1, R1, R2,
     $                1, M, W, 1, IW, INFO )
         CALL CHKXER( 'CTGSNA', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL CTGSNA( 'B', 'A', SEL, 1, A, 0, B, 1, Q, 1, U, 1, R1, R2,
     $                1, M, W, 1, IW, INFO )
         CALL CHKXER( 'CTGSNA', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL CTGSNA( 'B', 'A', SEL, 1, A, 1, B, 0, Q, 1, U, 1, R1, R2,
     $                1, M, W, 1, IW, INFO )
         CALL CHKXER( 'CTGSNA', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL CTGSNA( 'E', 'A', SEL, 1, A, 1, B, 1, Q, 0, U, 1, R1, R2,
     $                1, M, W, 1, IW, INFO )
         CALL CHKXER( 'CTGSNA', INFOT, NOUT, LERR, OK )
         INFOT = 12
         CALL CTGSNA( 'E', 'A', SEL, 1, A, 1, B, 1, Q, 1, U, 0, R1, R2,
     $                1, M, W, 1, IW, INFO )
         CALL CHKXER( 'CTGSNA', INFOT, NOUT, LERR, OK )
         INFOT = 15
         CALL CTGSNA( 'E', 'A', SEL, 1, A, 1, B, 1, Q, 1, U, 1, R1, R2,
     $                0, M, W, 1, IW, INFO )
         CALL CHKXER( 'CTGSNA', INFOT, NOUT, LERR, OK )
         INFOT = 18
         CALL CTGSNA( 'E', 'A', SEL, 1, A, 1, B, 1, Q, 1, U, 1, R1, R2,
     $                1, M, W, 0, IW, INFO )
         CALL CHKXER( 'CTGSNA', INFOT, NOUT, LERR, OK )
         NT = NT + 9
*
*        CTGSYL
*
         SRNAMT = 'CTGSYL'
         INFOT = 1
         CALL CTGSYL( '/', 0, 1, 1, A, 1, B, 1, Q, 1, U, 1, V, 1, Z, 1,
     $                SCALE, DIF, W, 1, IW, INFO )
         CALL CHKXER( 'CTGSYL', INFOT, NOUT, LERR, OK )
         INFOT = 2
         CALL CTGSYL( 'N', -1, 1, 1, A, 1, B, 1, Q, 1, U, 1, V, 1, Z, 1,
     $                SCALE, DIF, W, 1, IW, INFO )
         CALL CHKXER( 'CTGSYL', INFOT, NOUT, LERR, OK )
         INFOT = 3
         CALL CTGSYL( 'N', 0, 0, 1, A, 1, B, 1, Q, 1, U, 1, V, 1, Z, 1,
     $                SCALE, DIF, W, 1, IW, INFO )
         CALL CHKXER( 'CTGSYL', INFOT, NOUT, LERR, OK )
         INFOT = 4
         CALL CTGSYL( 'N', 0, 1, 0, A, 1, B, 1, Q, 1, U, 1, V, 1, Z, 1,
     $                SCALE, DIF, W, 1, IW, INFO )
         CALL CHKXER( 'CTGSYL', INFOT, NOUT, LERR, OK )
         INFOT = 6
         CALL CTGSYL( 'N', 0, 1, 1, A, 0, B, 1, Q, 1, U, 1, V, 1, Z, 1,
     $                SCALE, DIF, W, 1, IW, INFO )
         CALL CHKXER( 'CTGSYL', INFOT, NOUT, LERR, OK )
         INFOT = 8
         CALL CTGSYL( 'N', 0, 1, 1, A, 1, B, 0, Q, 1, U, 1, V, 1, Z, 1,
     $                SCALE, DIF, W, 1, IW, INFO )
         CALL CHKXER( 'CTGSYL', INFOT, NOUT, LERR, OK )
         INFOT = 10
         CALL CTGSYL( 'N', 0, 1, 1, A, 1, B, 1, Q, 0, U, 1, V, 1, Z, 1,
     $                SCALE, DIF, W, 1, IW, INFO )
         CALL CHKXER( 'CTGSYL', INFOT, NOUT, LERR, OK )
         INFOT = 12
         CALL CTGSYL( 'N', 0, 1, 1, A, 1, B, 1, Q, 1, U, 0, V, 1, Z, 1,
     $                SCALE, DIF, W, 1, IW, INFO )
         CALL CHKXER( 'CTGSYL', INFOT, NOUT, LERR, OK )
         INFOT = 14
         CALL CTGSYL( 'N', 0, 1, 1, A, 1, B, 1, Q, 1, U, 1, V, 0, Z, 1,
     $                SCALE, DIF, W, 1, IW, INFO )
         CALL CHKXER( 'CTGSYL', INFOT, NOUT, LERR, OK )
         INFOT = 16
         CALL CTGSYL( 'N', 0, 1, 1, A, 1, B, 1, Q, 1, U, 1, V, 1, Z, 0,
     $                SCALE, DIF, W, 1, IW, INFO )
         CALL CHKXER( 'CTGSYL', INFOT, NOUT, LERR, OK )
         INFOT = 20
         CALL CTGSYL( 'N', 1, 1, 1, A, 1, B, 1, Q, 1, U, 1, V, 1, Z, 1,
     $                SCALE, DIF, W, 1, IW, INFO )
         CALL CHKXER( 'CTGSYL', INFOT, NOUT, LERR, OK )
         INFOT = 20
         CALL CTGSYL( 'N', 2, 1, 1, A, 1, B, 1, Q, 1, U, 1, V, 1, Z, 1,
     $                SCALE, DIF, W, 1, IW, INFO )
         CALL CHKXER( 'CTGSYL', INFOT, NOUT, LERR, OK )
         NT = NT + 12
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
 9999 FORMAT( 1X, A3, ' routines passed the tests of the error exits (',
     $      I3, ' tests done)' )
 9998 FORMAT( ' *** ', A3, ' routines failed the tests of the error ',
     $      'exits ***' )
*
      RETURN
*
*     End of CERRGG
*
      END
