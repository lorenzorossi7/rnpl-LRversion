      LOGICAL          FUNCTION SSLECT( ZR, ZI )
*
*  -- LAPACK test routine (version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     February 29, 1992
*
*     .. Scalar Arguments ..
      REAL               ZI, ZR
*     ..
*
*  Purpose
*  =======
*
*  SSLECT returns .TRUE. if the eigenvalue ZR+sqrt(-1)*ZI is to be
*  selected, and otherwise it returns .FALSE.
*  It is used by SCHK41 to test if SGEES succesfully sorts eigenvalues,
*  and by SCHK43 to test if SGEESX succesfully sorts eigenvalues.
*
*  The common block /SSLCT/ controls how eigenvalues are selected.
*  If SELOPT = 0, then SSLECT return .TRUE. when ZR is less than zero,
*  and .FALSE. otherwise.
*  If SELOPT is at least 1, SSLECT returns SELVAL(SELOPT) and adds 1
*  to SELOPT, cycling back to 1 at SELMAX.
*
*  Arguments
*  =========
*
*  ZR      (input) REAL
*          The real part of a complex eigenvalue ZR + i*ZI.
*
*  ZI      (input) REAL
*          The imaginary part of a complex eigenvalue ZR + i*ZI.
*
*  =====================================================================
*
*     .. Arrays in Common ..
      LOGICAL            SELVAL( 20 )
      REAL               SELWI( 20 ), SELWR( 20 )
*     ..
*     .. Scalars in Common ..
      INTEGER            SELDIM, SELOPT
*     ..
*     .. Common blocks ..
      COMMON             / SSLCT / SELOPT, SELDIM, SELVAL, SELWR, SELWI
*     ..
*     .. Local Scalars ..
      INTEGER            I
      REAL               RMIN, X
*     ..
*     .. Parameters ..
      REAL               ZERO
      PARAMETER          ( ZERO = 0.0E0 )
*     ..
*     .. External Functions ..
      REAL               SLAPY2
      EXTERNAL           SLAPY2
*     ..
*     .. Executable Statements ..
*
      IF( SELOPT.EQ.0 ) THEN
         SSLECT = ( ZR.LT.ZERO )
      ELSE
         RMIN = ZERO
         DO 10 I = 1, SELDIM
            X = SLAPY2( ZR-SELWR( I ), ZI-SELWI( I ) )
            IF( X.LE.RMIN ) THEN
               RMIN = X
               SSLECT = SELVAL( I )
            END IF
   10    CONTINUE
      END IF
      RETURN
*
*     End of SSLECT
*
      END
