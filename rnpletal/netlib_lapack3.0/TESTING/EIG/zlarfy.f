      SUBROUTINE ZLARFY( UPLO, N, V, INCV, TAU, C, LDC, WORK )
*
*  -- LAPACK auxiliary test routine (version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     September 30, 1994
*
*     .. Scalar Arguments ..
      CHARACTER          UPLO
      INTEGER            INCV, LDC, N
      COMPLEX*16         TAU
*     ..
*     .. Array Arguments ..
      COMPLEX*16         C( LDC, * ), V( * ), WORK( * )
*     ..
*
*  Purpose
*  =======
*
*  ZLARFY applies an elementary reflector, or Householder matrix, H,
*  to an n x n Hermitian matrix C, from both the left and the right.
*
*  H is represented in the form
*
*     H = I - tau * v * v'
*
*  where  tau  is a scalar and  v  is a vector.
*
*  If  tau  is  zero, then  H  is taken to be the unit matrix.
*
*  Arguments
*  =========
*
*  UPLO    (input) CHARACTER*1
*          Specifies whether the upper or lower triangular part of the
*          Hermitian matrix C is stored.
*          = 'U':  Upper triangle
*          = 'L':  Lower triangle
*
*  N       (input) INTEGER
*          The number of rows and columns of the matrix C.  N >= 0.
*
*  V       (input) COMPLEX*16 array, dimension
*                  (1 + (N-1)*abs(INCV))
*          The vector v as described above.
*
*  INCV    (input) INTEGER
*          The increment between successive elements of v.  INCV must
*          not be zero.
*
*  TAU     (input) COMPLEX*16
*          The value tau as described above.
*
*  C       (input/output) COMPLEX*16 array, dimension (LDC, N)
*          On entry, the matrix C.
*          On exit, C is overwritten by H * C * H'.
*
*  LDC     (input) INTEGER
*          The leading dimension of the array C.  LDC >= max( 1, N ).
*
*  WORK    (workspace) COMPLEX*16 array, dimension (N)
*
*  =====================================================================
*
*     .. Parameters ..
      COMPLEX*16         ONE, ZERO, HALF
      PARAMETER          ( ONE = ( 1.0D+0, 0.0D+0 ),
     $                   ZERO = ( 0.0D+0, 0.0D+0 ),
     $                   HALF = ( 0.5D+0, 0.0D+0 ) )
*     ..
*     .. Local Scalars ..
      COMPLEX*16         ALPHA
*     ..
*     .. External Subroutines ..
      EXTERNAL           ZAXPY, ZHEMV, ZHER2
*     ..
*     .. External Functions ..
      COMPLEX*16         ZDOTC
      EXTERNAL           ZDOTC
*     ..
*     .. Executable Statements ..
*
      IF( TAU.EQ.ZERO )
     $   RETURN
*
*     Form  w:= C * v
*
      CALL ZHEMV( UPLO, N, ONE, C, LDC, V, INCV, ZERO, WORK, 1 )
*
      ALPHA = -HALF*TAU*ZDOTC( N, WORK, 1, V, INCV )
      CALL ZAXPY( N, ALPHA, V, INCV, WORK, 1 )
*
*     C := C - v * w' - w * v'
*
      CALL ZHER2( UPLO, N, -TAU, V, INCV, WORK, 1, C, LDC )
*
      RETURN
*
*     End of ZLARFY
*
      END
