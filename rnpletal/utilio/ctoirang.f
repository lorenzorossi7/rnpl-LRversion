C-------------------------------------------------------------------------------
C
C     Checks a group (ST-FIN/INC) for semantic validity, performing minor
C     adjustments if necessary.
C
C-------------------------------------------------------------------------------
C
      INTEGER FUNCTION CHKGRP(ST,FN,INC,RMIN,RMAX)
C
         INTEGER       INC, FN, RMAX, RMIN, ST
C
         CHKGRP = 0
         IF( ST .EQ. FN  .AND.  INC .NE. 1 ) THEN
            INC = 1
            CHKGRP = -1
         END IF
         IF( INC .GT. ABS(FN-ST) ) THEN
            FN = ST
            CHKGRP = -1
         END IF
         IF( ST .GT. FN  .AND.  INC .GT. 0 ) THEN
            INC = -INC
         END IF
         IF( ST .LT. RMIN  .OR.  ST .GT. RMAX  .OR.
     *       FN .LT. RMIN  .OR.  FN .GT. RMAX  .OR.
     *       INC .EQ. 0 ) THEN
            CHKGRP = -2
         END IF
C
         RETURN
C
      END
C-------------------------------------------------------------------------------
C
C     "Loads" character variable with single character.
C
C-------------------------------------------------------------------------------
C
      SUBROUTINE CLOAD(CVAR,LCHAR)
C
         CHARACTER*(*)   CVAR
         CHARACTER*1     LCHAR
         INTEGER         IC
C
         DO 10 IC = 1 , LEN(CVAR)
            CVAR(IC:IC) = LCHAR
 10      CONTINUE
C
         RETURN
C
      END
C-------------------------------------------------------------------------------
C
C     Converts character representation of signed integer in CREP to integer.
C     Representation assumed left-justified, blanks ignored, but no error
C     checking.
C
C-------------------------------------------------------------------------------
C
      INTEGER FUNCTION CTOI(CREP)
C
         CHARACTER*(*)    CREP
C
         INTEGER          INDEX, LEN
C
         CHARACTER*10     NUMS 
         CHARACTER*1      BLANK, MISIGN, PLSIGN, CHAR
         INTEGER          CURS, CURSST, PLMIN
         
         DATA             NUMS / '0123456789' /
     *                    BLANK / ' ' /, MISIGN / '-' /,
     *                    PLSIGN / '+' /
C
         CTOI = 0
         CURSST = 1
         PLMIN = 1
         IF( CREP(1:1) .EQ. PLSIGN ) THEN
            CURSST = 2
         ELSE
            IF( CREP(1:1) .EQ. MISIGN ) THEN
               CURSST = 2
               PLMIN = -1
            END IF
         END IF
         DO 10 CURS = CURSST , LEN(CREP)
            CHAR = CREP(CURS:CURS)
            IF( CHAR .NE. BLANK ) THEN
               CTOI = 10 * CTOI + INDEX(NUMS,CHAR) - 1
            END IF
 10      CONTINUE
         CTOI = PLMIN * CTOI
C
         RETURN
C
      END
C-------------------------------------------------------------------------------
C
C     Returns token.
C
C-------------------------------------------------------------------------------
C
      SUBROUTINE GETTOK(S,CURS,TOK,TOKLEN,CATCOD)
C
         INTEGER         INDEX, LEN, SKBLNK
C
         CHARACTER*(*)   TOK, S
         INTEGER         CATCOD, CURS, TOKLEN
C
         CHARACTER*14    CATCH, TOKSPN
         INTEGER         NCAT, CATST(5), CATFN(5) 
         CHARACTER*1     CHAR
         INTEGER         ICAT

         DATA            CATCH / '0123456789,-/*' /,
     *                   NCAT / 5 /,
     *                   CATST /  1, 11, 12, 13, 14 /,
     *                   CATFN / 10, 11, 12, 13, 14 /
C
         CURS = SKBLNK(S,CURS)
         IF( CURS .LE. LEN(S) ) THEN
C
C           Determine category type.
C
            DO 10 ICAT = 1 , NCAT
               IF( INDEX(CATCH(CATST(ICAT):CATFN(ICAT)),
     *                   S(CURS:CURS)) .NE. 0 ) GO TO 20
 10         CONTINUE
C
C           Invalid character encountered.
C
            CATCOD = -1
            RETURN
C
C           Accumulate token.
C
 20         CONTINUE
            CATCOD = ICAT
            TOKSPN = CATCH(CATST(CATCOD):CATFN(CATCOD))
            TOKLEN = 0
 30         CONTINUE
               CHAR = S(CURS:CURS)
               IF( INDEX(TOKSPN,CHAR) .EQ. 0 ) GO TO 40
                  TOKLEN = TOKLEN + 1
                  TOK(TOKLEN:TOKLEN) = CHAR
                  CURS = CURS + 1
                  CURS = SKBLNK(S,CURS)
            IF( CURS .LE. LEN(S) ) GO TO 30
 40         CONTINUE
C
         ELSE
C
            CATCOD = 0
            RETURN
C
         END IF
C
      END
C-------------------------------------------------------------------------------
C
C     Looks for integer group of form ST-FIN/INC starting at CURSth position
C     of S.
C
C-------------------------------------------------------------------------------
C
      SUBROUTINE GGROUP(S,CURS,ST,FIN,INC,FOUND)
C
         INTEGER         CTOI
C
         CHARACTER*(*)   S
         INTEGER         CURS, INC, ST, FIN
         LOGICAL         FOUND
C
         CHARACTER*80    TOK
         INTEGER         CATCOD, OCURS, TOKLEN
         LOGICAL         FRANGE
C
         FOUND = .TRUE.
         OCURS = CURS
         CALL GRANGE(S,CURS,ST,FIN,FRANGE)
         IF( FRANGE ) THEN
            OCURS = CURS
            CALL GETTOK(S,CURS,TOK,TOKLEN,CATCOD)
            IF( CATCOD .EQ. 0 ) THEN
               INC = 1
            ELSE IF( CATCOD .EQ. 2 ) THEN
               CURS = OCURS
               INC = 1
            ELSE IF( CATCOD .EQ. 4 ) THEN
               OCURS = CURS
               CALL GETTOK(S,CURS,TOK,TOKLEN,CATCOD)
               IF( CATCOD .NE. 1 ) THEN
                  FOUND = .FALSE.
               ELSE
                  INC = CTOI(TOK(1:TOKLEN))
               END IF
            ELSE
               FOUND = .FALSE.
            END IF
         ELSE
            FOUND = .FALSE.
         END IF
         IF( .NOT. FOUND ) THEN
            CURS = OCURS
         END IF
C
         RETURN
C
      END
C-------------------------------------------------------------------------------
C
C     Looks for integer range of form ST-FIN starting at CURSth position
C     of S.
C
C-------------------------------------------------------------------------------
C
      SUBROUTINE GRANGE(S,CURS,ST,FIN,FOUND)
C
         INTEGER         CTOI
C
         CHARACTER*(*)   S
         INTEGER         CURS, ST, FIN
         LOGICAL         FOUND
C
         CHARACTER*80    TOK
         INTEGER         CATCOD, OCURS, TOKLEN
C
         FOUND = .TRUE.
         OCURS = CURS
         CALL GETTOK(S,CURS,TOK,TOKLEN,CATCOD)
         IF( CATCOD .NE. 1  .AND.  CATCOD .NE. 5 ) THEN
            FOUND = .FALSE.
         ELSE
            IF( CATCOD .EQ. 1 ) THEN
               ST = CTOI(TOK(1:TOKLEN))
            ELSE
               ST = -999 999
            END IF
            OCURS = CURS
            CALL GETTOK(S,CURS,TOK,TOKLEN,CATCOD)
            IF( CATCOD .EQ. 0 ) THEN
               FIN = ST
            ELSE IF ( CATCOD .EQ. 3 ) THEN
               OCURS = CURS
               CALL GETTOK(S,CURS,TOK,TOKLEN,CATCOD)
               IF( CATCOD .EQ. 1 ) THEN
                  FIN = CTOI(TOK(1:TOKLEN))
               ELSE IF ( CATCOD .EQ. 5 ) THEN
                  FIN = -999 999
               ELSE
                  FOUND = .FALSE.
               END IF
            ELSE IF ( CATCOD .EQ. 2 ) THEN
               FIN = ST
               CURS = OCURS
            ELSE
               FOUND = .FALSE.
            END IF
         END IF
         IF( .NOT. FOUND ) THEN
            CURS = OCURS
         END IF
C
         RETURN
C
      END
C
C-----------------------------------------------------------------------
C
C     Standard routine for getting user to input group specifications.
C     Modification 060489: if PRUNIT < 0, "read" string from PRSTR,
C     no way of error checking right now.
C
C-----------------------------------------------------------------------
C
      SUBROUTINE INPTIV(IV,LIV,MXLIV,SUP,PRSTR,PRUNIT)
C
         CHARACTER*80    ITOC
         INTEGER         IXSPAN, MAKIND
C
         CHARACTER*(*)   PRSTR
         INTEGER         IV(1)
         INTEGER         LIV, MXLIV, PRUNIT, SUP
C
         INTEGER         RDUNIT
C
         CHARACTER*80    CLINE, CSUP
         INTEGER         INC(100), FN(100), ST(100),
     *                   CURS, NG, IDUMMY
         LOGICAL         FOUND
C
         CSUP = ITOC(SUP)
C
         IF( PRUNIT .GT. 0 ) THEN
            IF( PRUNIT .EQ. 6  .OR.  PRUNIT .EQ. 0 ) THEN
               RDUNIT = 5
            ELSE
               RDUNIT = PRUNIT
            END IF
 100        CONTINUE
               WRITE(PRUNIT,1020) PRSTR, CSUP(1:IXSPAN(CSUP,' '))
1020           FORMAT(' Select',A,'from 1 to ',A,': ')
C
C              Aug 4 1988, Had to change FORMAT statement from
C              simply A, was generating end-of-record on FILE
C              FT06F001 ...
C
C              Mar 18 1989, Still a problem, trying A40.
C
C              Modified: Sep 5 1990: eof == ' '
C
               READ(RDUNIT,1040,END=110) CLINE
1040           FORMAT(A80)
               GO TO 120
 110           CONTINUE
                  CLINE = ' '
 120           CONTINUE
               IF( CLINE .EQ. ' ') THEN
                  CALL LIVRMP(IV,1,1,SUP)
                  LIV = SUP
               ELSE
                  CURS = 1
                  CALL PSEXPR(CLINE,CURS,ST,FN,INC,NG,FOUND)
                  IF( .NOT. FOUND ) GO TO 100
                  IF( MAKIND(IV,LIV,MXLIV,ST,FN,INC,NG,1,SUP)
     *                .LT. -1 ) GO TO 100
               END IF
            ELSE
               CLINE = PRSTR
               IF( CLINE .EQ. ' ' ) THEN
                  CALL LIVRMP(IV,1,1,SUP)
                  LIV = SUP
               ELSE
                  CURS = 1
                  CALL PSEXPR(CLINE,CURS,ST,FN,INC,NG,FOUND)
                  IF( .NOT. FOUND ) THEN
                     WRITE(*,*) '<<< INPTIV: Could not parse',
     *                  ' index vector rep. passed in PRSTR. >>>'
                     RETURN
                  END IF
                  IDUMMY = MAKIND(IV,LIV,MXLIV,ST,FN,INC,NG,1,SUP)
               END IF
            END IF
C
         RETURN

900      CONTINUE 
         STOP
C
      END
C-------------------------------------------------------------------------------
C
C     Converts integer to character representation.
C
C-------------------------------------------------------------------------------
C
      CHARACTER*(*) FUNCTION ITOC(IREP)
C
         CHARACTER*10   NUMS 
C
         INTEGER        CURS, DIG, IREP, K, MAG, LCIREP, LOWER,
     *                  UPPER

         DATA           NUMS / '0123456789' /
C
         ITOC = ' '
         LCIREP = IREP
         IF( LCIREP .EQ. 0 ) THEN
            ITOC(1:1) = '0'
            RETURN
         ELSE IF( LCIREP .LT. 0 ) THEN
            ITOC(1:1) = '-'
            LCIREP = -LCIREP
            CURS = 2
         ELSE
            CURS = 1
         END IF
         LOWER = 1
         DO 10 MAG = 1 , 10
            UPPER = 10 * LOWER
            IF( LCIREP .GE. LOWER  .AND.
     *          LCIREP .LT. UPPER ) GO TO 20
            LOWER = UPPER
 10      CONTINUE
 20      CONTINUE
         DO 30 K = MAG , 1 , -1
            DIG = LCIREP / (10 ** (K - 1))
            ITOC(CURS:CURS) = NUMS(DIG + 1:DIG + 1)
            LCIREP = LCIREP - DIG * (10 ** (K - 1))
            CURS = CURS + 1
 30      CONTINUE
C
         RETURN
C
      END
C-------------------------------------------------------------------------------
C
C     Returns index of character to the left of first occurence of CHAR
C     in S.
C
C-------------------------------------------------------------------------------
C
      INTEGER FUNCTION IXSPAN(S,CHAR)
C
         CHARACTER*(*)   S
         CHARACTER*1     CHAR
C
         DO 10 IXSPAN = 0 , LEN(S) - 1
            IF( S(IXSPAN+1:IXSPAN+1) .EQ. CHAR ) RETURN
 10      CONTINUE
         IXSPAN = LEN(S)
C
         RETURN
C
      END
C-------------------------------------------------------------------------------
C
C     Produces index vector from list of groups provided no serious semantic
C     errors exist and INDVEC is sufficiently roomy. MINCOD is the minimum
C     severity level returned by CHKGRD which will allow processing to
C     continue.
C
C-------------------------------------------------------------------------------
C
      INTEGER FUNCTION MAKIND(INDVEC,LVEC,MAXVEC,ST,FN,INC,NG,RMIN,RMAX)
C
         INTEGER       CHKGRP
C
         INTEGER       LVEC, MAXVEC, RMAX, RMIN, NG
         INTEGER       INDVEC(MAXVEC), INC(NG), FN(NG), ST(NG)
         INTEGER       I, IG, MINCOD 

         DATA          MINCOD / -1 /
C
         MAKIND = 0
         DO 10 IG = 1 , NG
            IF( ST(IG) .EQ. -999 999 ) THEN
               ST(IG) = RMIN
            END IF
            IF( FN(IG) .EQ. -999 999 ) THEN
               FN(IG) = RMAX
            END IF
            MAKIND = MIN(MAKIND,CHKGRP(ST(IG),FN(IG),INC(IG),RMIN,RMAX))
 10      CONTINUE
         IF( MAKIND .GE. MINCOD ) THEN
            LVEC = 0
            DO 30 IG = 1 , NG
               DO 20 I = ST(IG) , FN(IG) , INC(IG)
                  LVEC = LVEC + 1
                  IF( LVEC .GT. MAXVEC ) THEN
                     MAKIND = -5
                     RETURN
                  END IF
                  INDVEC(LVEC) = I
 20            CONTINUE
 30         CONTINUE
         END IF
C
         RETURN
C
      END
C-------------------------------------------------------------------------------
C
C     Parses expression consisting of one or more groups, returning STart,
C     FINish and INCrement arrays.
C
C-------------------------------------------------------------------------------
C
      SUBROUTINE PSEXPR(S,CURS,ST,FIN,INC,NGROUP,SUCESS)
C
         CHARACTER*(*)   S
         INTEGER         INC(1), FIN(1), ST(1), CURS, NGROUP
         LOGICAL         SUCESS
C
         CHARACTER*80    TOK
         INTEGER         CATCOD, OCURS, TOKLEN
         LOGICAL         GFOUND
C
         NGROUP = 0
 10      OCURS = CURS
         CALL GGROUP(S,CURS,ST(NGROUP+1),FIN(NGROUP+1),
     *               INC(NGROUP+1),GFOUND)
         IF( GFOUND ) THEN
            NGROUP = NGROUP + 1
            OCURS = CURS
            CALL GETTOK(S,CURS,TOK,TOKLEN,CATCOD)
            IF( CATCOD .EQ. 0 ) THEN
               SUCESS = .TRUE.
            ELSE IF( CATCOD .EQ. 2 ) THEN
               GO TO 10
            ELSE
               SUCESS = .FALSE.
            END IF
         ELSE
            SUCESS = .FALSE.
         END IF
         IF( .NOT. SUCESS ) THEN
            CURS = OCURS
         END IF
C
         RETURN
C
      END

C-------------------------------------------------------------------------------
C
C     Same as INPTIV except command line is to be read (non-interactively)
C     from RDUNIT.
C
C     Modified so that null line == blank line.
C
C-------------------------------------------------------------------------------
C
      INTEGER FUNCTION READIV(IV,LIV,MXLIV,INF,SUP,RDUNIT)
C
         INTEGER         MAKIND
C
         INTEGER         IV(1)
         INTEGER         INF, LIV, MXLIV, RDUNIT, SUP
C
         CHARACTER*80    CLINE
         INTEGER         INC(100), FN(100), ST(100),
     *                   CURS, NG
         LOGICAL         FOUND
C
         READ(RDUNIT,1040,END=100,ERR=100) CLINE
1040     FORMAT(/A)
         IF( CLINE .EQ. ' ') THEN
            CALL LIVRMP(IV,INF,1,SUP-INF+1)
            LIV = SUP-INF+1
            READIV = 0
         ELSE
            CURS = 1
            CALL PSEXPR(CLINE,CURS,ST,FN,INC,NG,FOUND)
            IF( FOUND ) THEN
               READIV =
     *         MAKIND(IV,LIV,MXLIV,ST,FN,INC,NG,INF,SUP)
            ELSE
               READIV = -2
            END IF
         END IF
C
         RETURN
C
 100     CONTINUE
            READIV = -3
         RETURN
C
      END
C

C-------------------------------------------------------------------------------
C
C     History: READIV.                                                  y)
C
C     Blank line returns null index vector in this version.
C
C-------------------------------------------------------------------------------
C
      INTEGER FUNCTION RDIVNL(IV,LIV,MXLIV,INF,SUP,RDUNIT)
C
         INTEGER         MAKIND
C
         INTEGER         IV(1)
         INTEGER         INF, LIV, MXLIV, RDUNIT, SUP
C
         CHARACTER*80    CLINE
         INTEGER         INC(100), FN(100), ST(100),
     *                   CURS, NG
         LOGICAL         FOUND
C
         READ(RDUNIT,1040,END=100,ERR=100) CLINE
1040     FORMAT(/A)
         IF( CLINE .EQ. ' ') THEN
            LIV = 0
            RDIVNL = 0
         ELSE
            CURS = 1
            CALL PSEXPR(CLINE,CURS,ST,FN,INC,NG,FOUND)
            IF( FOUND ) THEN
               RDIVNL =
     *         MAKIND(IV,LIV,MXLIV,ST,FN,INC,NG,INF,SUP)
            ELSE
               RDIVNL = -2
            END IF
         END IF
C
         RETURN
C
 100     CONTINUE
            RDIVNL = -3
         RETURN
C
      END

C-------------------------------------------------------------------------------
C
C     History: READIV, RDIVNL.
C
C     Blank line returns null index vector in this version.
C
C     Character string passed directly instead of reading via RDUNIT.
C
C-------------------------------------------------------------------------------
C
      INTEGER FUNCTION IRDIVN(IV,LIV,MXLIV,INF,SUP,CLINE)
C
         INTEGER         MAKIND
C
         INTEGER         IV(1)
         INTEGER         INF, LIV, MXLIV, SUP
C
         CHARACTER*(*)   CLINE
         INTEGER         INC(100), FN(100), ST(100),
     *                   CURS, NG
         LOGICAL         FOUND
C
         IF( CLINE .EQ. ' ') THEN
            LIV = 0
            IRDIVN = 0
         ELSE
            CURS = 1
            CALL PSEXPR(CLINE,CURS,ST,FN,INC,NG,FOUND)
            IF( FOUND ) THEN
               IRDIVN =
     *         MAKIND(IV,LIV,MXLIV,ST,FN,INC,NG,INF,SUP)
            ELSE
               IRDIVN = -2
            END IF
         END IF
C
         RETURN
C
 100     CONTINUE
            IRDIVN = -3
         RETURN
C
      END

C-------------------------------------------------------------------------------
C
C     History: IRDIVN.
C
C     Blank line returns full index vector in this version.
C
C     Character string passed directly instead of reading via RDUNIT.
C
C-------------------------------------------------------------------------------
C
      INTEGER FUNCTION IRDIV(IV,LIV,MXLIV,INF,SUP,CLINE)
C
         INTEGER         MAKIND
C
         INTEGER         IV(1)
         INTEGER         INF, LIV, MXLIV, SUP
C
         CHARACTER*(*)   CLINE
         INTEGER         INC(100), FN(100), ST(100),
     *                   CURS, NG
         LOGICAL         FOUND
C
         IF( CLINE .EQ. ' ') THEN
            LIV = SUP - INF + 1
            CALL LOC_IVRAMP(IV,SUP,1,LIV)
            IRDIV = 0
         ELSE
            CURS = 1
            CALL PSEXPR(CLINE,CURS,ST,FN,INC,NG,FOUND)
            IF( FOUND ) THEN
               IRDIV =
     *         MAKIND(IV,LIV,MXLIV,ST,FN,INC,NG,INF,SUP)
            ELSE
               IRDIV = -2
            END IF
         END IF
C
         RETURN
C
 100     CONTINUE
            IRDIV = -3
         RETURN
C
      END


C-------------------------------------------------------------------------------
C
C     Returns position of first non-blank character starting from (and
C     including) CURSORth position.
C
C-------------------------------------------------------------------------------
C
      INTEGER FUNCTION SKBLNK(S,CURSOR)
C
         CHARACTER*(*)   S
         INTEGER         CURSOR
C
         CHARACTER*1     BLANK 
         INTEGER         I

         DATA            BLANK / ' ' /
C
         IF( CURSOR .LE. LEN(S) ) THEN
            DO 10 I = CURSOR , LEN(S)
               IF( S(I:I) .NE. BLANK ) GO TO 20
 10         CONTINUE
            SKBLNK = LEN(S) + 1
            RETURN
C
 20         SKBLNK = I
            RETURN
        ELSE
            SKBLNK = CURSOR
            RETURN
        END IF
C
      END
C
C-----------------------------------------------------------------------
C
C     Blank-delimited token isolator.
C
C-----------------------------------------------------------------------
C
      CHARACTER*(*) FUNCTION GTOK(S,CURS)
C
         INTEGER        INDEX, SKBLNK
         CHARACTER*(*)  S
         INTEGER        CURS, NCURS
C
         GTOK(1:LEN(GTOK)) = ' '
         CURS = SKBLNK(S,CURS)
C*       WRITE(*,*) '***1 CURS = ',CURS
         IF( CURS .LE. LEN(S) ) THEN
            NCURS = INDEX(S(CURS:LEN(S)),' ') + CURS - 1
            IF( NCURS .LT. CURS ) NCURS = LEN(S) + 1
            GTOK = S(CURS:NCURS-1)
            CURS = SKBLNK(S,NCURS)
C*          WRITE(*,*) '**2 CURS =',CURS,' NCURS = ',NCURS
         END IF
C
         RETURN
C
      END
C
C-----------------------------------------------------------------------
C
C     Concatenates S1, S2; squeezing out blanks in process.
C
C-----------------------------------------------------------------------
C
      CHARACTER*(*) FUNCTION CATSQZ(S1,S2)
C
         CHARACTER*(*)   S1, S2
         INTEGER         IXSPAN, SKBLNK
         INTEGER         IFIN, IST, N1, N2
C
         CATSQZ = ' '
         IST = SKBLNK(S1,1)
         IF( IST .LE. LEN(S1) ) THEN
            IFIN = IST + IXSPAN(S1(IST:LEN(S1)),' ') - 1
            N1 = IFIN - IST + 1
            CATSQZ(1:N1) = S1(IST:IFIN)
         ELSE
            N1 = 0
         END IF
         IST = SKBLNK(S2,1)
         IF( IST .LE. LEN(S2) ) THEN
            IFIN = IST + IXSPAN(S2(IST:LEN(S2)),' ') - 1
            N2 = IFIN - IST + 1
            CATSQZ(N1+1:N1+1+N2) = S2(IST:IFIN)
         ELSE
            N2 = 0
         END IF
C
         RETURN
C
      END
C
C-----------------------------------------------------------------------
C
C     Returns index of last non-blank character in S.
C
C-----------------------------------------------------------------------
C
      INTEGER FUNCTION INDLNB(S)
C
         CHARACTER*(*)    S
         INTEGER          I
C
         DO 10 INDLNB = LEN(S) , 1 , -1
            IF( S(INDLNB:INDLNB) .NE. ' ' ) RETURN
 10      CONTINUE
         INDLNB = 0
C
         RETURN
C
      END

C-----------------------------------------------------------------------
C     Returns index of first non-blank character in S.
C
C     March 2012: Had to hack on this due to what I would claim is a 
C     bug in the 64-bit version of the Intel compiler?  After how 
C     many years?  (20+ !)
C-----------------------------------------------------------------------
 
      integer function indfnb(s)
 
         character*(*)    s
         integer          i
 
         do i = 1 , len(s)
            if( s(i:i) .ne. ' ' ) then
               indfnb = i
               return
            end if
         end do
         indfnb = 0
 
         return
 
      end

C---------------------------------------------------------------------
C
C     Initializes V to ramp function - origin V0, increment VINC.
C
C---------------------------------------------------------------------
C
      SUBROUTINE LIVRMP(V,V0,VINC,N)
C
         INTEGER     V(1)
         INTEGER     V0, VINC
         INTEGER     I, N
C
         V(1) = V0
         DO 10 I = 2 , N
            V(I) = V(I-1) + VINC
 10      CONTINUE
C
         RETURN
C
      END
C
C---------------------------------------------------------------------
C
C     LOCAL VERSION of IVECLIB routine ...
C
C     Initializes V to ramp function - origin V0, increment VINC.
C
C---------------------------------------------------------------------
C
      SUBROUTINE LOC_IVRAMP(V,V0,VINC,N)
C
         INTEGER     V(1)
         INTEGER     V0, VINC
         INTEGER     I, N
C
         V(1) = V0
         DO 10 I = 2 , N
            V(I) = V(I-1) + VINC
 10      CONTINUE
C
         RETURN
C
      END
