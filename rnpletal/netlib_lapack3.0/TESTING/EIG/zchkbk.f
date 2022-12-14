      SUBROUTINE ZCHKBK( NIN, NOUT )
*
*  -- LAPACK test routine (version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     September 30, 1994
*
*     .. Scalar Arguments ..
      INTEGER            NIN, NOUT
*     ..
*
*  Purpose
*  =======
*
*  ZCHKBK tests ZGEBAK, a routine for backward transformation of
*  the computed right or left eigenvectors if the orginal matrix
*  was preprocessed by balance subroutine ZGEBAL.
*
*  Arguments
*  =========
*
*  NIN     (input) INTEGER
*          The logical unit number for input.  NIN > 0.
*
*  NOUT    (input) INTEGER
*          The logical unit number for output.  NOUT > 0.
*
* ======================================================================
*
*     .. Parameters ..
      INTEGER            LDE
      PARAMETER          ( LDE = 20 )
      DOUBLE PRECISION   ZERO
      PARAMETER          ( ZERO = 0.0D0 )
*     ..
*     .. Local Scalars ..
      INTEGER            I, IHI, ILO, INFO, J, KNT, N, NINFO
      DOUBLE PRECISION   EPS, RMAX, SAFMIN, VMAX, X
      COMPLEX*16         CDUM
*     ..
*     .. Local Arrays ..
      INTEGER            LMAX( 2 )
      DOUBLE PRECISION   SCALE( LDE )
      COMPLEX*16         E( LDE, LDE ), EIN( LDE, LDE )
*     ..
*     .. External Functions ..
      DOUBLE PRECISION   DLAMCH
      EXTERNAL           DLAMCH
*     ..
*     .. External Subroutines ..
      EXTERNAL           ZGEBAK
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, DBLE, DIMAG, MAX
*     ..
*     .. Statement Functions ..
      DOUBLE PRECISION   CABS1
*     ..
*     .. Statement Function definitions ..
      CABS1( CDUM ) = ABS( DBLE( CDUM ) ) + ABS( DIMAG( CDUM ) )
*     ..
*     .. Executable Statements ..
*
      LMAX( 1 ) = 0
      LMAX( 2 ) = 0
      NINFO = 0
      KNT = 0
      RMAX = ZERO
      EPS = DLAMCH( 'E' )
      SAFMIN = DLAMCH( 'S' )
*
   10 CONTINUE
*
      READ( NIN, FMT = * )N, ILO, IHI
      IF( N.EQ.0 )
     $   GO TO 60
*
      READ( NIN, FMT = * )( SCALE( I ), I = 1, N )
      DO 20 I = 1, N
         READ( NIN, FMT = * )( E( I, J ), J = 1, N )
   20 CONTINUE
*
      DO 30 I = 1, N
         READ( NIN, FMT = * )( EIN( I, J ), J = 1, N )
   30 CONTINUE
*
      KNT = KNT + 1
      CALL ZGEBAK( 'B', 'R', N, ILO, IHI, SCALE, N, E, LDE, INFO )
*
      IF( INFO.NE.0 ) THEN
         NINFO = NINFO + 1
         LMAX( 1 ) = KNT
      END IF
*
      VMAX = ZERO
      DO 50 I = 1, N
         DO 40 J = 1, N
            X = CABS1( E( I, J )-EIN( I, J ) ) / EPS
            IF( CABS1( E( I, J ) ).GT.SAFMIN )
     $         X = X / CABS1( E( I, J ) )
            VMAX = MAX( VMAX, X )
   40    CONTINUE
   50 CONTINUE
*
      IF( VMAX.GT.RMAX ) THEN
         LMAX( 2 ) = KNT
         RMAX = VMAX
      END IF
*
      GO TO 10
*
   60 CONTINUE
*
      WRITE( NOUT, FMT = 9999 )
 9999 FORMAT( 1X, '.. test output of ZGEBAK .. ' )
*
      WRITE( NOUT, FMT = 9998 )RMAX
 9998 FORMAT( 1X, 'value of largest test error             = ', D12.3 )
      WRITE( NOUT, FMT = 9997 )LMAX( 1 )
 9997 FORMAT( 1X, 'example number where info is not zero   = ', I4 )
      WRITE( NOUT, FMT = 9996 )LMAX( 2 )
 9996 FORMAT( 1X, 'example number having largest error     = ', I4 )
      WRITE( NOUT, FMT = 9995 )NINFO
 9995 FORMAT( 1X, 'number of examples where info is not 0  = ', I4 )
      WRITE( NOUT, FMT = 9994 )KNT
 9994 FORMAT( 1X, 'total number of examples tested         = ', I4 )
*
      RETURN
*
*     End of ZCHKBK
*
      END
