      SUBROUTINE DDRVBD( NSIZES, MM, NN, NTYPES, DOTYPE, ISEED, THRESH,
     $                   A, LDA, U, LDU, VT, LDVT, ASAV, USAV, VTSAV, S,
     $                   SSAV, E, WORK, LWORK, IWORK, NOUT, INFO )
*
*  -- LAPACK test routine (version 3.0) --
*     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
*     Courant Institute, Argonne National Lab, and Rice University
*     October 31, 1999
*
*     .. Scalar Arguments ..
      INTEGER            INFO, LDA, LDU, LDVT, LWORK, NOUT, NSIZES,
     $                   NTYPES
      DOUBLE PRECISION   THRESH
*     ..
*     .. Array Arguments ..
      LOGICAL            DOTYPE( * )
      INTEGER            ISEED( 4 ), IWORK( * ), MM( * ), NN( * )
      DOUBLE PRECISION   A( LDA, * ), ASAV( LDA, * ), E( * ), S( * ),
     $                   SSAV( * ), U( LDU, * ), USAV( LDU, * ),
     $                   VT( LDVT, * ), VTSAV( LDVT, * ), WORK( * )
*     ..
*
*  Purpose
*  =======
*
*  DDRVBD checks the singular value decomposition (SVD) drivers
*  DGESVD and SGESDD.
*  Both DGESVD and SGESDD factor A = U diag(S) VT, where U and VT are
*  orthogonal and diag(S) is diagonal with the entries of the array S
*  on its diagonal. The entries of S are the singular values,
*  nonnegative and stored in decreasing order.  U and VT can be
*  optionally not computed, overwritten on A, or computed partially.
*
*  A is M by N. Let MNMIN = min( M, N ). S has dimension MNMIN.
*  U can be M by M or M by MNMIN. VT can be N by N or MNMIN by N.
*
*  When DDRVBD is called, a number of matrix "sizes" (M's and N's)
*  and a number of matrix "types" are specified.  For each size (M,N)
*  and each type of matrix, and for the minimal workspace as well as
*  workspace adequate to permit blocking, an  M x N  matrix "A" will be
*  generated and used to test the SVD routines.  For each matrix, A will
*  be factored as A = U diag(S) VT and the following 12 tests computed:
*
*  Test for DGESVD:
*
*  (1)    | A - U diag(S) VT | / ( |A| max(M,N) ulp )
*
*  (2)    | I - U'U | / ( M ulp )
*
*  (3)    | I - VT VT' | / ( N ulp )
*
*  (4)    S contains MNMIN nonnegative values in decreasing order.
*         (Return 0 if true, 1/ULP if false.)
*
*  (5)    | U - Upartial | / ( M ulp ) where Upartial is a partially
*         computed U.
*
*  (6)    | VT - VTpartial | / ( N ulp ) where VTpartial is a partially
*         computed VT.
*
*  (7)    | S - Spartial | / ( MNMIN ulp |S| ) where Spartial is the
*         vector of singular values from the partial SVD
*
*  Test for DGESDD:
*
*  (8)    | A - U diag(S) VT | / ( |A| max(M,N) ulp )
*
*  (9)    | I - U'U | / ( M ulp )
*
*  (10)   | I - VT VT' | / ( N ulp )
*
*  (11)   S contains MNMIN nonnegative values in decreasing order.
*         (Return 0 if true, 1/ULP if false.)
*
*  (12)   | U - Upartial | / ( M ulp ) where Upartial is a partially
*         computed U.
*
*  (13)   | VT - VTpartial | / ( N ulp ) where VTpartial is a partially
*         computed VT.
*
*  (14)   | S - Spartial | / ( MNMIN ulp |S| ) where Spartial is the
*         vector of singular values from the partial SVD
*
*  The "sizes" are specified by the arrays MM(1:NSIZES) and
*  NN(1:NSIZES); the value of each element pair (MM(j),NN(j))
*  specifies one size.  The "types" are specified by a logical array
*  DOTYPE( 1:NTYPES ); if DOTYPE(j) is .TRUE., then matrix type "j"
*  will be generated.
*  Currently, the list of possible types is:
*
*  (1)  The zero matrix.
*  (2)  The identity matrix.
*  (3)  A matrix of the form  U D V, where U and V are orthogonal and
*       D has evenly spaced entries 1, ..., ULP with random signs
*       on the diagonal.
*  (4)  Same as (3), but multiplied by the underflow-threshold / ULP.
*  (5)  Same as (3), but multiplied by the overflow-threshold * ULP.
*
*  Arguments
*  ==========
*
*  NSIZES  (input) INTEGER
*          The number of matrix sizes (M,N) contained in the vectors
*          MM and NN.
*
*  MM      (input) INTEGER array, dimension (NSIZES)
*          The values of the matrix row dimension M.
*
*  NN      (input) INTEGER array, dimension (NSIZES)
*          The values of the matrix column dimension N.
*
*  NTYPES  (input) INTEGER
*          The number of elements in DOTYPE.   If it is zero, DDRVBD
*          does nothing.  It must be at least zero.  If it is MAXTYP+1
*          and NSIZES is 1, then an additional type, MAXTYP+1 is
*          defined, which is to use whatever matrices are in A and B.
*          This is only useful if DOTYPE(1:MAXTYP) is .FALSE. and
*          DOTYPE(MAXTYP+1) is .TRUE. .
*
*  DOTYPE  (input) LOGICAL array, dimension (NTYPES)
*          If DOTYPE(j) is .TRUE., then for each size (m,n), a matrix
*          of type j will be generated.  If NTYPES is smaller than the
*          maximum number of types defined (PARAMETER MAXTYP), then
*          types NTYPES+1 through MAXTYP will not be generated.  If
*          NTYPES is larger than MAXTYP, DOTYPE(MAXTYP+1) through
*          DOTYPE(NTYPES) will be ignored.
*
*  ISEED   (input/output) INTEGER array, dimension (4)
*          On entry, the seed of the random number generator.  The array
*          elements should be between 0 and 4095; if not they will be
*          reduced mod 4096.  Also, ISEED(4) must be odd.
*          On exit, ISEED is changed and can be used in the next call to
*          DDRVBD to continue the same random number sequence.
*
*  THRESH  (input) DOUBLE PRECISION
*          The threshold value for the test ratios.  A result is
*          included in the output file if RESULT >= THRESH.  The test
*          ratios are scaled to be O(1), so THRESH should be a small
*          multiple of 1, e.g., 10 or 100.  To have every test ratio
*          printed, use THRESH = 0.
*
*  A       (workspace) DOUBLE PRECISION array, dimension (LDA,NMAX)
*          where NMAX is the maximum value of N in NN.
*
*  LDA     (input) INTEGER
*          The leading dimension of the array A.  LDA >= max(1,MMAX),
*          where MMAX is the maximum value of M in MM.
*
*  U       (workspace) DOUBLE PRECISION array, dimension (LDU,MMAX)
*
*  LDU     (input) INTEGER
*          The leading dimension of the array U.  LDU >= max(1,MMAX).
*
*  VT      (workspace) DOUBLE PRECISION array, dimension (LDVT,NMAX)
*
*  LDVT    (input) INTEGER
*          The leading dimension of the array VT.  LDVT >= max(1,NMAX).
*
*  ASAV    (workspace) DOUBLE PRECISION array, dimension (LDA,NMAX)
*
*  USAV    (workspace) DOUBLE PRECISION array, dimension (LDU,MMAX)
*
*  VTSAV   (workspace) DOUBLE PRECISION array, dimension (LDVT,NMAX)
*
*  S       (workspace) DOUBLE PRECISION array, dimension
*                      (max(min(MM,NN)))
*
*  SSAV    (workspace) DOUBLE PRECISION array, dimension
*                      (max(min(MM,NN)))
*
*  E       (workspace) DOUBLE PRECISION array, dimension
*                      (max(min(MM,NN)))
*
*  WORK    (workspace) DOUBLE PRECISION array, dimension (LWORK)
*
*  LWORK   (input) INTEGER
*          The number of entries in WORK.  This must be at least
*          max(3*MN+MX,5*MN-4)+2*MN**2 for all pairs
*          pairs  (MN,MX)=( min(MM(j),NN(j), max(MM(j),NN(j)) )
*
*  IWORK   (workspace) INTEGER array, dimension at least 8*min(M,N)
*
*  NOUT    (input) INTEGER
*          The FORTRAN unit number for printing out error messages
*          (e.g., if a routine returns IINFO not equal to 0.)
*
*  INFO    (output) INTEGER
*          If 0, then everything ran OK.
*           -1: NSIZES < 0
*           -2: Some MM(j) < 0
*           -3: Some NN(j) < 0
*           -4: NTYPES < 0
*           -7: THRESH < 0
*          -10: LDA < 1 or LDA < MMAX, where MMAX is max( MM(j) ).
*          -12: LDU < 1 or LDU < MMAX.
*          -14: LDVT < 1 or LDVT < NMAX, where NMAX is max( NN(j) ).
*          -21: LWORK too small.
*          If  DLATMS, or DGESVD returns an error code, the
*              absolute value of it is returned.
*
*  =====================================================================
*
*     .. Parameters ..
      DOUBLE PRECISION   ZERO, ONE
      PARAMETER          ( ZERO = 0.0D0, ONE = 1.0D0 )
      INTEGER            MAXTYP
      PARAMETER          ( MAXTYP = 5 )
*     ..
*     .. Local Scalars ..
      LOGICAL            BADMM, BADNN
      CHARACTER          JOBQ, JOBU, JOBVT
      CHARACTER*3        PATH
      INTEGER            I, IINFO, IJQ, IJU, IJVT, IWS, IWTMP, J, JSIZE,
     $                   JTYPE, LSWORK, M, MINWRK, MMAX, MNMAX, MNMIN,
     $                   MTYPES, N, NFAIL, NMAX, NTEST
      DOUBLE PRECISION   ANORM, DIF, DIV, OVFL, ULP, ULPINV, UNFL
*     ..
*     .. Local Arrays ..
      CHARACTER          CJOB( 4 )
      INTEGER            IOLDSD( 4 )
      DOUBLE PRECISION   RESULT( 14 )
*     ..
*     .. External Functions ..
      DOUBLE PRECISION   DLAMCH
      EXTERNAL           DLAMCH
*     ..
*     .. External Subroutines ..
      EXTERNAL           ALASVM, DBDT01, DGESDD, DGESVD, DLABAD, DLACPY,
     $                   DLASET, DLATMS, DORT01, DORT03, XERBLA
*     ..
*     .. Intrinsic Functions ..
      INTRINSIC          ABS, DBLE, MAX, MIN
*     ..
*     .. Scalars in Common ..
      LOGICAL            LERR, OK
      CHARACTER*6        SRNAMT
      INTEGER            INFOT, NUNIT
*     ..
*     .. Common blocks ..
      COMMON             / INFOC / INFOT, NUNIT, OK, LERR
      COMMON             / SRNAMC / SRNAMT
*     ..
*     .. Data statements ..
      DATA               CJOB / 'N', 'O', 'S', 'A' /
*     ..
*     .. Executable Statements ..
*
*     Check for errors
*
      INFO = 0
      BADMM = .FALSE.
      BADNN = .FALSE.
      MMAX = 1
      NMAX = 1
      MNMAX = 1
      MINWRK = 1
      DO 10 J = 1, NSIZES
         MMAX = MAX( MMAX, MM( J ) )
         IF( MM( J ).LT.0 )
     $      BADMM = .TRUE.
         NMAX = MAX( NMAX, NN( J ) )
         IF( NN( J ).LT.0 )
     $      BADNN = .TRUE.
         MNMAX = MAX( MNMAX, MIN( MM( J ), NN( J ) ) )
         MINWRK = MAX( MINWRK, MAX( 3*MIN( MM( J ),
     $            NN( J ) )+MAX( MM( J ), NN( J ) ), 5*MIN( MM( J ),
     $            NN( J )-4 ) )+2*MIN( MM( J ), NN( J ) )**2 )
   10 CONTINUE
*
*     Check for errors
*
      IF( NSIZES.LT.0 ) THEN
         INFO = -1
      ELSE IF( BADMM ) THEN
         INFO = -2
      ELSE IF( BADNN ) THEN
         INFO = -3
      ELSE IF( NTYPES.LT.0 ) THEN
         INFO = -4
      ELSE IF( LDA.LT.MAX( 1, MMAX ) ) THEN
         INFO = -10
      ELSE IF( LDU.LT.MAX( 1, MMAX ) ) THEN
         INFO = -12
      ELSE IF( LDVT.LT.MAX( 1, NMAX ) ) THEN
         INFO = -14
      ELSE IF( MINWRK.GT.LWORK ) THEN
         INFO = -21
      END IF
*
      IF( INFO.NE.0 ) THEN
         CALL XERBLA( 'DDRVBD', -INFO )
         RETURN
      END IF
*
*     Initialize constants
*
      PATH( 1: 1 ) = 'Double precision'
      PATH( 2: 3 ) = 'BD'
      NFAIL = 0
      NTEST = 0
      UNFL = DLAMCH( 'Safe minimum' )
      OVFL = ONE / UNFL
      CALL DLABAD( UNFL, OVFL )
      ULP = DLAMCH( 'Precision' )
      ULPINV = ONE / ULP
      INFOT = 0
*
*     Loop over sizes, types
*
      DO 150 JSIZE = 1, NSIZES
         M = MM( JSIZE )
         N = NN( JSIZE )
         MNMIN = MIN( M, N )
*
         IF( NSIZES.NE.1 ) THEN
            MTYPES = MIN( MAXTYP, NTYPES )
         ELSE
            MTYPES = MIN( MAXTYP+1, NTYPES )
         END IF
*
         DO 140 JTYPE = 1, MTYPES
            IF( .NOT.DOTYPE( JTYPE ) )
     $         GO TO 140
*
            DO 20 J = 1, 4
               IOLDSD( J ) = ISEED( J )
   20       CONTINUE
*
*           Compute "A"
*
            IF( MTYPES.GT.MAXTYP )
     $         GO TO 30
*
            IF( JTYPE.EQ.1 ) THEN
*
*              Zero matrix
*
               CALL DLASET( 'Full', M, N, ZERO, ZERO, A, LDA )
*
            ELSE IF( JTYPE.EQ.2 ) THEN
*
*              Identity matrix
*
               CALL DLASET( 'Full', M, N, ZERO, ONE, A, LDA )
*
            ELSE
*
*              (Scaled) random matrix
*
               IF( JTYPE.EQ.3 )
     $            ANORM = ONE
               IF( JTYPE.EQ.4 )
     $            ANORM = UNFL / ULP
               IF( JTYPE.EQ.5 )
     $            ANORM = OVFL*ULP
               CALL DLATMS( M, N, 'U', ISEED, 'N', S, 4, DBLE( MNMIN ),
     $                      ANORM, M-1, N-1, 'N', A, LDA, WORK, IINFO )
               IF( IINFO.NE.0 ) THEN
                  WRITE( NOUT, FMT = 9996 )'Generator', IINFO, M, N,
     $               JTYPE, IOLDSD
                  INFO = ABS( IINFO )
                  RETURN
               END IF
            END IF
*
   30       CONTINUE
            CALL DLACPY( 'F', M, N, A, LDA, ASAV, LDA )
*
*           Do for minimal and adequate (for blocking) workspace
*
            DO 130 IWS = 1, 4
*
               DO 40 J = 1, 14
                  RESULT( J ) = -ONE
   40          CONTINUE
*
*              Test DGESVD: Factorize A
*
               IWTMP = MAX( 3*MIN( M, N )+MAX( M, N ), 5*MIN( M, N ) )
               LSWORK = IWTMP + ( IWS-1 )*( LWORK-IWTMP ) / 3
               LSWORK = MIN( LSWORK, LWORK )
               LSWORK = MAX( LSWORK, 1 )
               IF( IWS.EQ.4 )
     $            LSWORK = LWORK
*
               IF( IWS.GT.1 )
     $            CALL DLACPY( 'F', M, N, ASAV, LDA, A, LDA )
               SRNAMT = 'DGESVD'
               CALL DGESVD( 'A', 'A', M, N, A, LDA, SSAV, USAV, LDU,
     $                      VTSAV, LDVT, WORK, LSWORK, IINFO )
               IF( IINFO.NE.0 ) THEN
                  WRITE( NOUT, FMT = 9995 )'GESVD', IINFO, M, N, JTYPE,
     $               LSWORK, IOLDSD
                  INFO = ABS( IINFO )
                  RETURN
               END IF
*
*              Do tests 1--4
*
               CALL DBDT01( M, N, 0, ASAV, LDA, USAV, LDU, SSAV, E,
     $                      VTSAV, LDVT, WORK, RESULT( 1 ) )
               IF( M.NE.0 .AND. N.NE.0 ) THEN
                  CALL DORT01( 'Columns', M, M, USAV, LDU, WORK, LWORK,
     $                         RESULT( 2 ) )
                  CALL DORT01( 'Rows', N, N, VTSAV, LDVT, WORK, LWORK,
     $                         RESULT( 3 ) )
               END IF
               RESULT( 4 ) = ZERO
               DO 50 I = 1, MNMIN - 1
                  IF( SSAV( I ).LT.SSAV( I+1 ) )
     $               RESULT( 4 ) = ULPINV
                  IF( SSAV( I ).LT.ZERO )
     $               RESULT( 4 ) = ULPINV
   50          CONTINUE
               IF( MNMIN.GE.1 ) THEN
                  IF( SSAV( MNMIN ).LT.ZERO )
     $               RESULT( 4 ) = ULPINV
               END IF
*
*              Do partial SVDs, comparing to SSAV, USAV, and VTSAV
*
               RESULT( 5 ) = ZERO
               RESULT( 6 ) = ZERO
               RESULT( 7 ) = ZERO
               DO 80 IJU = 0, 3
                  DO 70 IJVT = 0, 3
                     IF( ( IJU.EQ.3 .AND. IJVT.EQ.3 ) .OR.
     $                   ( IJU.EQ.1 .AND. IJVT.EQ.1 ) )GO TO 70
                     JOBU = CJOB( IJU+1 )
                     JOBVT = CJOB( IJVT+1 )
                     CALL DLACPY( 'F', M, N, ASAV, LDA, A, LDA )
                     SRNAMT = 'DGESVD'
                     CALL DGESVD( JOBU, JOBVT, M, N, A, LDA, S, U, LDU,
     $                            VT, LDVT, WORK, LSWORK, IINFO )
*
*                    Compare U
*
                     DIF = ZERO
                     IF( M.GT.0 .AND. N.GT.0 ) THEN
                        IF( IJU.EQ.1 ) THEN
                           CALL DORT03( 'C', M, MNMIN, M, MNMIN, USAV,
     $                                  LDU, A, LDA, WORK, LWORK, DIF,
     $                                  IINFO )
                        ELSE IF( IJU.EQ.2 ) THEN
                           CALL DORT03( 'C', M, MNMIN, M, MNMIN, USAV,
     $                                  LDU, U, LDU, WORK, LWORK, DIF,
     $                                  IINFO )
                        ELSE IF( IJU.EQ.3 ) THEN
                           CALL DORT03( 'C', M, M, M, MNMIN, USAV, LDU,
     $                                  U, LDU, WORK, LWORK, DIF,
     $                                  IINFO )
                        END IF
                     END IF
                     RESULT( 5 ) = MAX( RESULT( 5 ), DIF )
*
*                    Compare VT
*
                     DIF = ZERO
                     IF( M.GT.0 .AND. N.GT.0 ) THEN
                        IF( IJVT.EQ.1 ) THEN
                           CALL DORT03( 'R', N, MNMIN, N, MNMIN, VTSAV,
     $                                  LDVT, A, LDA, WORK, LWORK, DIF,
     $                                  IINFO )
                        ELSE IF( IJVT.EQ.2 ) THEN
                           CALL DORT03( 'R', N, MNMIN, N, MNMIN, VTSAV,
     $                                  LDVT, VT, LDVT, WORK, LWORK,
     $                                  DIF, IINFO )
                        ELSE IF( IJVT.EQ.3 ) THEN
                           CALL DORT03( 'R', N, N, N, MNMIN, VTSAV,
     $                                  LDVT, VT, LDVT, WORK, LWORK,
     $                                  DIF, IINFO )
                        END IF
                     END IF
                     RESULT( 6 ) = MAX( RESULT( 6 ), DIF )
*
*                    Compare S
*
                     DIF = ZERO
                     DIV = MAX( DBLE( MNMIN )*ULP*S( 1 ), UNFL )
                     DO 60 I = 1, MNMIN - 1
                        IF( SSAV( I ).LT.SSAV( I+1 ) )
     $                     DIF = ULPINV
                        IF( SSAV( I ).LT.ZERO )
     $                     DIF = ULPINV
                        DIF = MAX( DIF, ABS( SSAV( I )-S( I ) ) / DIV )
   60                CONTINUE
                     RESULT( 7 ) = MAX( RESULT( 7 ), DIF )
   70             CONTINUE
   80          CONTINUE
*
*              Test DGESDD: Factorize A
*
               IWTMP = 5*MNMIN*MNMIN + 9*MNMIN + MAX( M, N )
               LSWORK = IWTMP + ( IWS-1 )*( LWORK-IWTMP ) / 3
               LSWORK = MIN( LSWORK, LWORK )
               LSWORK = MAX( LSWORK, 1 )
               IF( IWS.EQ.4 )
     $            LSWORK = LWORK
*
               CALL DLACPY( 'F', M, N, ASAV, LDA, A, LDA )
               SRNAMT = 'DGESDD'
               CALL DGESDD( 'A', M, N, A, LDA, SSAV, USAV, LDU, VTSAV,
     $                      LDVT, WORK, LSWORK, IWORK, IINFO )
               IF( IINFO.NE.0 ) THEN
                  WRITE( NOUT, FMT = 9995 )'GESDD', IINFO, M, N, JTYPE,
     $               LSWORK, IOLDSD
                  INFO = ABS( IINFO )
                  RETURN
               END IF
*
*              Do tests 8--11
*
               CALL DBDT01( M, N, 0, ASAV, LDA, USAV, LDU, SSAV, E,
     $                      VTSAV, LDVT, WORK, RESULT( 8 ) )
               IF( M.NE.0 .AND. N.NE.0 ) THEN
                  CALL DORT01( 'Columns', M, M, USAV, LDU, WORK, LWORK,
     $                         RESULT( 9 ) )
                  CALL DORT01( 'Rows', N, N, VTSAV, LDVT, WORK, LWORK,
     $                         RESULT( 10 ) )
               END IF
               RESULT( 11 ) = ZERO
               DO 90 I = 1, MNMIN - 1
                  IF( SSAV( I ).LT.SSAV( I+1 ) )
     $               RESULT( 11 ) = ULPINV
                  IF( SSAV( I ).LT.ZERO )
     $               RESULT( 11 ) = ULPINV
   90          CONTINUE
               IF( MNMIN.GE.1 ) THEN
                  IF( SSAV( MNMIN ).LT.ZERO )
     $               RESULT( 11 ) = ULPINV
               END IF
*
*              Do partial SVDs, comparing to SSAV, USAV, and VTSAV
*
               RESULT( 12 ) = ZERO
               RESULT( 13 ) = ZERO
               RESULT( 14 ) = ZERO
               DO 110 IJQ = 0, 2
                  JOBQ = CJOB( IJQ+1 )
                  CALL DLACPY( 'F', M, N, ASAV, LDA, A, LDA )
                  SRNAMT = 'DGESDD'
                  CALL DGESDD( JOBQ, M, N, A, LDA, S, U, LDU, VT, LDVT,
     $                         WORK, LSWORK, IWORK, IINFO )
*
*                 Compare U
*
                  DIF = ZERO
                  IF( M.GT.0 .AND. N.GT.0 ) THEN
                     IF( IJQ.EQ.1 ) THEN
                        IF( M.GE.N ) THEN
                           CALL DORT03( 'C', M, MNMIN, M, MNMIN, USAV,
     $                                  LDU, A, LDA, WORK, LWORK, DIF,
     $                                  INFO )
                        ELSE
                           CALL DORT03( 'C', M, MNMIN, M, MNMIN, USAV,
     $                                  LDU, U, LDU, WORK, LWORK, DIF,
     $                                  INFO )
                        END IF
                     ELSE IF( IJQ.EQ.2 ) THEN
                        CALL DORT03( 'C', M, MNMIN, M, MNMIN, USAV, LDU,
     $                               U, LDU, WORK, LWORK, DIF, INFO )
                     END IF
                  END IF
                  RESULT( 12 ) = MAX( RESULT( 12 ), DIF )
*
*                 Compare VT
*
                  DIF = ZERO
                  IF( M.GT.0 .AND. N.GT.0 ) THEN
                     IF( IJQ.EQ.1 ) THEN
                        IF( M.GE.N ) THEN
                           CALL DORT03( 'R', N, MNMIN, N, MNMIN, VTSAV,
     $                                  LDVT, VT, LDVT, WORK, LWORK,
     $                                  DIF, INFO )
                        ELSE
                           CALL DORT03( 'R', N, MNMIN, N, MNMIN, VTSAV,
     $                                  LDVT, A, LDA, WORK, LWORK, DIF,
     $                                  INFO )
                        END IF
                     ELSE IF( IJQ.EQ.2 ) THEN
                        CALL DORT03( 'R', N, MNMIN, N, MNMIN, VTSAV,
     $                               LDVT, VT, LDVT, WORK, LWORK, DIF,
     $                               INFO )
                     END IF
                  END IF
                  RESULT( 13 ) = MAX( RESULT( 13 ), DIF )
*
*                 Compare S
*
                  DIF = ZERO
                  DIV = MAX( DBLE( MNMIN )*ULP*S( 1 ), UNFL )
                  DO 100 I = 1, MNMIN - 1
                     IF( SSAV( I ).LT.SSAV( I+1 ) )
     $                  DIF = ULPINV
                     IF( SSAV( I ).LT.ZERO )
     $                  DIF = ULPINV
                     DIF = MAX( DIF, ABS( SSAV( I )-S( I ) ) / DIV )
  100             CONTINUE
                  RESULT( 14 ) = MAX( RESULT( 14 ), DIF )
  110          CONTINUE
*
*              End of Loop -- Check for RESULT(j) > THRESH
*
               DO 120 J = 1, 14
                  IF( RESULT( J ).GE.THRESH ) THEN
                     IF( NFAIL.EQ.0 ) THEN
                        WRITE( NOUT, FMT = 9999 )
                        WRITE( NOUT, FMT = 9998 )
                     END IF
                     WRITE( NOUT, FMT = 9997 )M, N, JTYPE, IWS, IOLDSD,
     $                  J, RESULT( J )
                     NFAIL = NFAIL + 1
                  END IF
  120          CONTINUE
               NTEST = NTEST + 14
*
  130       CONTINUE
  140    CONTINUE
  150 CONTINUE
*
*     Summary
*
      CALL ALASVM( PATH, NOUT, NFAIL, NTEST, 0 )
*
 9999 FORMAT( ' SVD -- Real Singular Value Decomposition Driver ',
     $      / ' Matrix types (see DDRVBD for details):',
     $      / / ' 1 = Zero matrix', / ' 2 = Identity matrix',
     $      / ' 3 = Evenly spaced singular values near 1',
     $      / ' 4 = Evenly spaced singular values near underflow',
     $      / ' 5 = Evenly spaced singular values near overflow', / /
     $      ' Tests performed: ( A is dense, U and V are orthogonal,',
     $      / 19X, ' S is an array, and Upartial, VTpartial, and',
     $      / 19X, ' Spartial are partially computed U, VT and S),', / )
 9998 FORMAT( ' 1 = | A - U diag(S) VT | / ( |A| max(M,N) ulp ) ',
     $      / ' 2 = | I - U**T U | / ( M ulp ) ',
     $      / ' 3 = | I - VT VT**T | / ( N ulp ) ',
     $      / ' 4 = 0 if S contains min(M,N) nonnegative values in',
     $      ' decreasing order, else 1/ulp',
     $      / ' 5 = | U - Upartial | / ( M ulp )',
     $      / ' 6 = | VT - VTpartial | / ( N ulp )',
     $      / ' 7 = | S - Spartial | / ( min(M,N) ulp |S| )',
     $      / ' 8 = | A - U diag(S) VT | / ( |A| max(M,N) ulp ) ',
     $      / ' 9 = | I - U**T U | / ( M ulp ) ',
     $      / '10 = | I - VT VT**T | / ( N ulp ) ',
     $      / '11 = 0 if S contains min(M,N) nonnegative values in',
     $      ' decreasing order, else 1/ulp',
     $      / '12 = | U - Upartial | / ( M ulp )',
     $      / '13 = | VT - VTpartial | / ( N ulp )',
     $      / '14 = | S - Spartial | / ( min(M,N) ulp |S| )', / / )
 9997 FORMAT( ' M=', I5, ', N=', I5, ', type ', I1, ', IWS=', I1,
     $      ', seed=', 4( I4, ',' ), ' test(', I2, ')=', G11.4 )
 9996 FORMAT( ' DDRVBD: ', A, ' returned INFO=', I6, '.', / 9X, 'M=',
     $      I6, ', N=', I6, ', JTYPE=', I6, ', ISEED=(', 3( I5, ',' ),
     $      I5, ')' )
 9995 FORMAT( ' DDRVBD: ', A, ' returned INFO=', I6, '.', / 9X, 'M=',
     $      I6, ', N=', I6, ', JTYPE=', I6, ', LSWORK=', I6, / 9X,
     $      'ISEED=(', 3( I5, ',' ), I5, ')' )
*
      RETURN
*
*     End of DDRVBD
*
      END
