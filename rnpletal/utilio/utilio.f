C-------------------------------------------------------------------------
C
C     IRIS Version of MYOPEN.
C
C     Opens file from within FORTRAN and attaches to LU. OPCODE
C     determines type of file as follows.
C
C     0  --->   formatted sequential
C     1  --->   unformatted sequential
C     2  --->   unformatted direct access
C
C     Modified after 3.2 upgrade, RECL now (a la ANSI standard) specified
C     in words.                  
C
C     Modified to use C2UFNL which returns length of file name after
C     CMS ---> Unix conversion.
C
C--------------------------------------------------------------------------

      SUBROUTINE MYOPEN(LU,FDESC,LSTAT,OPCODE,RECL,XTENT,RC)

         IMPLICIT       NONE

         INTEGER        INDLNB

         CHARACTER*(*)  FDESC,       LSTAT
         CHARACTER*1024 LFDESC,      LLSTAT
         INTEGER        LU,          OPCODE,        RC,
     *                  RECL,        XTENT
         INTEGER        LLFD

         logical        ltrace
         parameter    ( ltrace = .false. )

         CALL SLOAD(LFDESC,' ')
         CALL C2UFNL(FDESC,LFDESC,LLFD)
         if( ltrace ) then
            write(0,50) fdesc, lfdesc(1:indlnb(lfdesc))
 50         format('myopen: <',a,'> -> <',a,'>')
         end if
         CALL SLOAD(LLSTAT,' ')
         LLSTAT = LSTAT(1:INDLNB(LSTAT))
         IF( LLSTAT(1:1) .EQ. ' ' ) THEN
            LLSTAT = 'UNKNOWN'
         END IF

         IF(      OPCODE .EQ. 0 ) THEN

            OPEN( UNIT = LU, IOSTAT = RC,
     *            FILE = LFDESC(1:INDLNB(LFDESC)),
     *            ACCESS = 'SEQUENTIAL', FORM = 'FORMATTED',
     *            STATUS = LLSTAT(1:INDLNB(LLSTAT)) )

         ELSE IF( OPCODE .EQ. 1 ) THEN

            OPEN( UNIT = LU, IOSTAT = RC,
     *            FILE = LFDESC(1:INDLNB(LFDESC)),
     *            ACCESS = 'SEQUENTIAL', FORM = 'UNFORMATTED',
     *            STATUS = LLSTAT )

         ELSE IF( OPCODE .EQ. 2 ) THEN

c->         OPEN( UNIT = LU, IOSTAT = RC,
c->  *            FILE = LFDESC(1:INDLNB(LFDESC)),
c->  *            ACCESS = 'DIRECT',  RECL = RECL,
c->  *            FORM = 'UNFORMATTED',
c->  *            STATUS = LLSTAT )
            OPEN( UNIT = LU, IOSTAT = RC,
     *            FILE = LFDESC(1:INDLNB(LFDESC)),
     *            ACCESS = 'DIRECT',  RECL = RECL / 4,
     *            FORM = 'UNFORMATTED',
     *            STATUS = LLSTAT )

         ELSE

            WRITE(*,*) '<<< MYOPEN:: Invalid OPCODE ',OPCODE,
     *                 '. >>>'

         END IF

         RETURN

      END

C-----------------------------------------------------------------------------
C
C     Open with error exit on non--zero RC.
C
C-----------------------------------------------------------------------------

      SUBROUTINE MYOPENC(LU,FDESC,LSTAT,OPCODE,RECL,XTENT,PGM)

         IMPLICIT       NONE

         INTEGER        INDLNB

         CHARACTER*(*)  FDESC,       LSTAT,         PGM
         INTEGER        LU,          OPCODE,        
     *                  RECL,        XTENT

         INTEGER        RC
         CHARACTER*256  BUFFER

         logical        ltrace
         parameter    ( ltrace = .true. )

         CALL MYOPEN(LU,FDESC,LSTAT,OPCODE,RECL,XTENT,RC)
         IF( RC .NE. 0 ) THEN
            WRITE(BUFFER,1000) FDESC(1:INDLNB(FDESC)),       
     *                         LSTAT(1:INDLNB(LSTAT))
1000        FORMAT('Error opening ',a,'  Status: ',a)
            CALL ERR1(PGM,BUFFER)
         END IF
         RETURN

      END

C-------------------------------------------------------------------------
C
C     Case-sensitive version of MYOPEN ...
C
C--------------------------------------------------------------------------

      SUBROUTINE MYOPENCS(LU,FDESC,LSTAT,OPCODE,RECL,XTENT,RC)

         IMPLICIT       NONE

         INTEGER        INDLNB

         CHARACTER*(*)  FDESC,       LSTAT
         CHARACTER*1024 LFDESC,      LLSTAT
         INTEGER        LU,          OPCODE,        RC,
     *                  RECL,        XTENT
         INTEGER        LLFD

         logical        ltrace
         parameter    ( ltrace = .false. )

         LFDESC = FDESC
         if( ltrace ) then
            write(0,50) fdesc, lfdesc(1:indlnb(lfdesc))
 50         format('myopen: <',a,'> -> <',a,'>')
         end if
         CALL SLOAD(LLSTAT,' ')
         LLSTAT = LSTAT(1:INDLNB(LSTAT))
         IF( LLSTAT(1:1) .EQ. ' ' ) THEN
            LLSTAT = 'UNKNOWN'
         END IF

         IF(      OPCODE .EQ. 0 ) THEN

            OPEN( UNIT = LU, IOSTAT = RC,
     *            FILE = LFDESC(1:INDLNB(LFDESC)),
     *            ACCESS = 'SEQUENTIAL', FORM = 'FORMATTED',
     *            STATUS = LLSTAT(1:INDLNB(LLSTAT)) )

         ELSE IF( OPCODE .EQ. 1 ) THEN

            OPEN( UNIT = LU, IOSTAT = RC,
     *            FILE = LFDESC(1:INDLNB(LFDESC)),
     *            ACCESS = 'SEQUENTIAL', FORM = 'UNFORMATTED',
     *            STATUS = LLSTAT )

         ELSE IF( OPCODE .EQ. 2 ) THEN

c->         OPEN( UNIT = LU, IOSTAT = RC,
c->  *            FILE = LFDESC(1:INDLNB(LFDESC)),
c->  *            ACCESS = 'DIRECT',  RECL = RECL,
c->  *            FORM = 'UNFORMATTED',
c->  *            STATUS = LLSTAT )
            OPEN( UNIT = LU, IOSTAT = RC,
     *            FILE = LFDESC(1:INDLNB(LFDESC)),
     *            ACCESS = 'DIRECT',  RECL = RECL / 4,
     *            FORM = 'UNFORMATTED',
     *            STATUS = LLSTAT )

         ELSE

            WRITE(*,*) '<<< MYOPENCS:: Invalid OPCODE ',OPCODE,
     *                 '. >>>'

         END IF

         RETURN

      END

C-----------------------------------------------------------------------------
C
C     Opens FILE FT<unit>F001 formatted on D.
C
C-----------------------------------------------------------------------------

      SUBROUTINE DOPEN(UNIT)

         IMPLICIT     LOGICAL(A-Z)

         CHARACTER*2  ITOC

         INTEGER      RC,         UNIT

         CHARACTER*24 FNAME
         CHARACTER*1  FIRST

         IF( UNIT .LE. 0  .OR. UNIT .GE.  100  .OR.
     *       UNIT .EQ. 5  .OR. UNIT .EQ. 6 ) THEN
            WRITE(*,100) UNIT
 100        FORMAT(' <<< DOPEN:: Abend. Can not open unit ',I4,
     *             ' as FILE FT... >>>')
         ELSE
            IF( UNIT .LT. 10 ) THEN
               FIRST = ITOC(UNIT)
               FNAME = 'FILE FT0'//FIRST//'F001 FILE D'
            ELSE
               FNAME = 'FILE FT'//ITOC(UNIT)//'F001 FILE D'
            END IF
            CALL MYOPEN(UNIT,FNAME,' ',0,0,0,RC)
            IF( RC .NE. 0 ) THEN
               WRITE(*,200) FNAME
 200           FORMAT(' <<< DOPEN:: Abend. Failure opening ',
     *                A,'. >>>')
            END IF
         END IF

         RETURN

       END
C
C-----------------------------------------------------------------------
C
C     For use with MYOPEN. Prints a message, then stops.
C
C-----------------------------------------------------------------------
C
      SUBROUTINE MYOPAB(MESS)
C
         IMPLICIT       LOGICAL*1 (A-Z)
C
         CHARACTER*(*)  MESS
C
         WRITE(*,100) MESS
100      FORMAT(' <<< ',A,' >>>')
C
         STOP
C
      END
C
C-----------------------------------------------------------------------
C
C     Given S of form <FNAME FTYPE FMODE>, returns the 3 tokens.
C
C-----------------------------------------------------------------------
C
      SUBROUTINE FNPARS(S,FNAME,FTYPE,FMODE)
C
         CHARACTER*8  GTOK
         CHARACTER*80 S
         CHARACTER*8  FMODE, FNAME, FTYPE
         INTEGER      CURS
C
         CURS = 1
         FNAME = GTOK(S,CURS)
C*       WRITE(*,*) S, CURS, FNAME
         FTYPE = GTOK(S,CURS)
C*       WRITE(*,*) S, CURS, FTYPE
         FMODE = GTOK(S,CURS)
C*       WRITE(*,*) S, CURS, FMODE
C
         RETURN
C
      END

C-----------------------------------------------------------------------
C
C     Nth invocation returns the symbol {SYM}{N-1}.
C
C-----------------------------------------------------------------------
C
      CHARACTER*(*) FUNCTION GENSYM()
C
         CHARACTER*8  ITOC
         CHARACTER*3  SYM 
         INTEGER      SYMNO

         DATA         SYM / 'SYM' /,
     *                SYMNO / 0 /
C
         GENSYM = SYM // ITOC(SYMNO)
         SYMNO = SYMNO + 1
C
         RETURN
C
      END
C
C-----------------------------------------------------------------------
C
C     Yet another "general purpose" vector listing routine.
C
C-----------------------------------------------------------------------
C
      SUBROUTINE DVDMP2(V,N,INC,LGRP,LABEL,LABLSW,FMTSPC,UNIT)
C
         INTEGER       N
         REAL*8        V(N)
         CHARACTER*(*) FMTSPC, LABEL
         INTEGER       INC, LGRP, UNIT
         LOGICAL       LABLSW
C
         CHARACTER*8   ITOC
         CHARACTER*64  FMT
         INTEGER       FIN, IGRP, J, NGRP, ST
C
         IF( LABLSW ) THEN
            WRITE(UNIT,100) LABEL
100         FORMAT(/' <<< ',A,' >>>'/)
         ELSE
            WRITE(UNIT,200)
200         FORMAT(' ')
         END IF
         FMT = '(1H ,' // ITOC(LGRP) // '(' // FMTSPC // '))'
         NGRP = 1 + ((N - 1) / INC) / LGRP
         ST = 1
         DO 10 IGRP = 1 , NGRP
           FIN = MIN0(N,ST+(LGRP - 1) * INC)
           WRITE(UNIT,FMT) ( V(J) , J = ST , FIN , INC )
           ST = FIN + INC
 10      CONTINUE
C
         RETURN
C
      END
C
C-----------------------------------------------------------------------
C
C     Performs standard file opening / header record-writing operations
C     for PDEDAT/RECL files on CMS.
C
C-----------------------------------------------------------------------
C
      SUBROUTINE OPNOPD(CODNAM,FUNNAM,NF,NLDREC,NR,NT,DR,DT,URECL,
     *                  UDATA,RC)
C
         IMPLICIT         LOGICAL*1 (A-Z)
C
         INTEGER          INDLNB
C
         CHARACTER*(*)    CODNAM
         INTEGER          NF
         CHARACTER*16     FUNNAM(NF)
         REAL*8           DR, DT
         INTEGER          NLDREC, NR, NT, UDATA, URECL, RC
C
         INTEGER          IF, LRECL, XTENT
         CHARACTER*1024   LCDNAM
         CHARACTER*1      DEFMOD 

         LOGICAL          LTRACE 
         PARAMETER      ( LTRACE = .TRUE. )

         DATA             DEFMOD / 'D' /

C
         CALL SLOAD(LCDNAM,' ')
         LCDNAM(1:INDLNB(CODNAM)) = CODNAM(1:INDLNB(CODNAM))
         LRECL = MAX(8 * NR , 64)
         XTENT = NLDREC + NF * NT
         CALL MYOPEN(URECL,LCDNAM(1:indlnb(lcdnam))//'.recl',
     *               ' ',1,0,0,RC)
         IF( RC .NE. 0 ) THEN
            IF( LTRACE ) THEN
               WRITE(0,*) 'opnopd: Error opening <'//
     *                    LCDNAM(1:indlnb(lcdnam))//'.recl>'
            END IF
            RC = -1
            RETURN
         END IF
         CALL MYOPEN(UDATA,LCDNAM(1:indlnb(lcdnam))//'.pdedat',
     *               'NEW',2,LRECL,XTENT,RC)
         IF( RC .NE. 0 ) THEN
            IF( LTRACE ) THEN
               WRITE(0,*) 'opnopd: Error opening <'//
     *                    LCDNAM(1:indlnb(lcdnam))//'.pdedat>'
            END IF
            RC = -1
            RC = -2
            RETURN
         END IF
         WRITE(URECL) LRECL, XTENT
         WRITE(UDATA,REC=1) NLDREC, NF, NR, NT, DR, DT
         DO 100 IF = 1 , NF
            WRITE(UDATA,REC=NLDREC-NF+IF) FUNNAM(IF)
 100     CONTINUE
C
         RC = 0
C
         RETURN
C
      END
C
C-----------------------------------------------------------------------
C
C     Routine to keep number of record groups ("time steps stored")
C     properly maintained.
C
C-----------------------------------------------------------------------
C
      SUBROUTINE WRNT(UDATA)
C
         IMPLICIT    LOGICAL*1 (A-Z)
C
         INTEGER     NT, UDATA
C
         REAL*8      DR, DT
         INTEGER     LNT, NF, NLDREC, NR

         DATA        NT / 0 /
         SAVE        NT
C
         NT = NT + 1
         READ(UDATA,REC=1)  NLDREC, NF, NR, LNT, DR, DT
         WRITE(UDATA,REC=1) NLDREC, NF, NR,  NT, DR, DT
C
         RETURN
C
      END

C-----------------------------------------------------------------------
C
C     Routine to keep number of record groups ("time steps stored")
C     properly maintained.
C
C-----------------------------------------------------------------------
C
      SUBROUTINE WRNT1(UDATA)
C
         IMPLICIT    LOGICAL*1 (A-Z)
C
         INTEGER     NT
         COMMON    / COMNT /   NT
         INTEGER     UDATA
C
         REAL*8      DR, DT
         INTEGER     LNT, NF, NLDREC, NR
C
         NT = NT + 1
         READ(UDATA,REC=1)  NLDREC, NF, NR, LNT, DR, DT
         WRITE(UDATA,REC=1) NLDREC, NF, NR,  NT, DR, DT
C
         RETURN
C
      END
C
      SUBROUTINE RESETNT 
C
         IMPLICIT    LOGICAL*1 (A-Z)
C
         INTEGER     NT
         COMMON    / COMNT /   NT

         NT = 0

         RETURN

      END
C
C-----------------------------------------------------------------------
C
C     Same as WRNT but maintains information in vector so that
C     multiple files can be active.
C
C-----------------------------------------------------------------------
C
      SUBROUTINE VWRNT(UDATA)
C
         IMPLICIT    LOGICAL*1 (A-Z)
C
         INTEGER     MXUNIT
         PARAMETER   ( MXUNIT = 100 )
         INTEGER     NT(MXUNIT) 
C
         INTEGER     UDATA
C
         REAL*8      DR, DT
         INTEGER     IUD, LNT, NF, NLDREC, NR

         DATA        NT / MXUNIT * 0 /
C
         IUD = IABS(UDATA)
         IF( 0 .LT. IUD  .AND. IUD .LE. MXUNIT ) THEN
            NT(IUD) = NT(IUD) + 1
            READ(UDATA,REC=1)  NLDREC, NF, NR, LNT,      DR, DT
            WRITE(UDATA,REC=1) NLDREC, NF, NR,  NT(IUD), DR, DT
         END IF
C
         RETURN
C
      END
C
C-----------------------------------------------------------------------
C
C     Same as VWRNT but gets rid of vector.
C
C-----------------------------------------------------------------------
C
      SUBROUTINE VWRNT1(UDATA)
C
         IMPLICIT    LOGICAL*1 (A-Z)
C
C
         INTEGER     UDATA
C
         REAL*8      DR, DT
         INTEGER     NF, NT, NLDREC, NR
C
         READ(UDATA,REC=1)  NLDREC, NF, NR,  NT,      DR, DT
         WRITE(UDATA,REC=1) NLDREC, NF, NR,  NT+1,    DR, DT
C
         RETURN
C
      END
C
C-----------------------------------------------------------------------
C
C     Performs output selection/pointer initialization procedure.
C
C-----------------------------------------------------------------------
C
      SUBROUTINE OUTSEL(OUTPTR,OFNAME,ORSFLG,OFTOT,OFPAIR,FNAME,RSFLG,
     *                  OUTFLG,NFPAIR,NFSING,NSIZE,BPTR)
C
         IMPLICIT         LOGICAL*1 (A-Z)
C
         CHARACTER*(*)    FNAME(1), OFNAME(1)
         INTEGER          OUTPTR(1)
         LOGICAL*1        ORSFLG(1), OUTFLG(1), RSFLG(1)
         INTEGER          BPTR, NFPAIR, NFSING, NSIZE, OFPAIR, OFTOT
         INTEGER          IF, LBPTR
C
         LBPTR = BPTR
         OFTOT = 0
         OFPAIR = 0
         IF( NFPAIR .GE. 0 ) THEN
            DO 100 IF = 1 , NFPAIR
               IF( OUTFLG(IF) ) THEN
                  OFTOT = OFTOT + 1
                  OFPAIR = OFPAIR + 1
                  OUTPTR(OFTOT) = LBPTR + 2 * NSIZE * (IF - 1)
                  OFNAME(OFTOT) = FNAME(IF)
                  ORSFLG(OFTOT) = RSFLG(IF)
               END IF
 100        CONTINUE
         END IF
         IF( NFSING .GE. 0 ) THEN
            LBPTR = BPTR - NSIZE + 2 * NSIZE * NFPAIR
            DO 200 IF = 1 , NFSING
               IF( OUTFLG(NFPAIR+IF) ) THEN
                  OFTOT = OFTOT + 1
                  OUTPTR(OFTOT) = LBPTR + NSIZE * (IF - 1)
                  OFNAME(OFTOT) = FNAME(NFPAIR+IF)
                  ORSFLG(OFTOT) = RSFLG(NFPAIR+IF)
               END IF
 200        CONTINUE
         END IF
C
         RETURN
C
      END
C
C-------------------------------------------------------------------------------
C
C     Outputs grid functions.
C
C-------------------------------------------------------------------------------
C
      SUBROUTINE OUTF(Q,RIV,LRIV,ST,SKIP,NF,RECST,UNIT)
C
         REAL*8    Q(1)
         INTEGER   RIV(1)
         INTEGER   I, IF, LRIV, NF, RECST, SKIP, ST, UNIT
C
         DO 10 IF = 0 , NF-1
            WRITE(UNIT,REC=RECST+IF)
     *           ( Q(ST + IF*SKIP + RIV(I) - 1) , I = 1 , LRIV )
 10      CONTINUE
C
         RETURN
C
      END
C
C-------------------------------------------------------------------------------
C
C     Swaps pointers to effect time step advance.
C
C-------------------------------------------------------------------------------
C
      SUBROUTINE SWPPTR(P,N)
C
         INTEGER       P(1)
         INTEGER       I, K, N, T
C
         DO 10 I = 1 , N
           K = 2 * I - 1
           T = P(K)
           P(K) = P(K+1)
           P(K+1) = T
 10      CONTINUE
C
         RETURN
C
      END
C
C-----------------------------------------------------------------------
C
C     Opens PDEDAT file, attaches to LU and returns basic "header"
C     parameters.
C
C     Modified to abend if either open unsuccessful.
C
C-----------------------------------------------------------------------
C
      SUBROUTINE PDOHDR(LU,CODE,NLDREC,NF,NR,NT,DR,DT)
C
         INTEGER           INDLNB
C
         CHARACTER*(*)     CODE
         REAL*8            DR, DT
         INTEGER           LU, NF, NLDREC, NR, NT
C
         CHARACTER*1024    LCODE
         CHARACTER*1       DEFMOD 
         INTEGER           DRECL, RC, XTENT

         DATA              DEFMOD / '*' /
C
         LCODE = CODE
         CALL MYOPEN(LU,LCODE(1:INDLNB(LCODE))//'.recl',
     *              'OLD',1,0,0,RC)
         IF( RC .NE. 0 ) THEN
            WRITE(*,*) '<<< PDOHDR:: Abend: Failure opening ',
     *                 LCODE(1:INDLNB(LCODE))//'.recl. >>>'
            STOP
         END IF
         READ(LU) DRECL, XTENT
         CLOSE(LU)
         CALL MYOPEN(LU,LCODE(1:INDLNB(LCODE))//'.pdedat',
     *               'OLD',2,DRECL,
     *               XTENT,RC)
         IF( RC .NE. 0 ) THEN
            WRITE(*,*) '<<< PDOHDR:: Abend: Failure opening ',
     *                 LCODE(1:INDLNB(LCODE))//'.pdedat. >>>'
            STOP
         END IF
         READ(LU,REC=1) NLDREC, NF, NR, NT, DR, DT
C
         RETURN
C
      END
C
C-----------------------------------------------------------------------
C
C     Returns index of function with label NAME in standard PDEDAT file
C     attached to LU.
C
C-----------------------------------------------------------------------
C
      INTEGER FUNCTION PDFIND(NAME,LU,NLDREC,NF)
C
         CHARACTER*(*)     NAME
         INTEGER           LU, NF, NLDREC
C
         CHARACTER*16      FNAME
         INTEGER           IF
C
         PDFIND = 0
         DO 10 IF = 1 , NF
            READ(LU,REC=NLDREC-NF+IF) FNAME
            IF( INDEX(FNAME,NAME) .NE. 0 ) THEN
               PDFIND = IF
               RETURN
            END IF
 10      CONTINUE
C
         RETURN
C
      END
C
C-----------------------------------------------------------------------
C
C     Advances to end-of-file on unit LU for FTYPE:0 formatted,
C                                            FTYPE:1 unformatted.
C
C-----------------------------------------------------------------------
C
      SUBROUTINE TOEOF(LU,FTYPE)
C
         IMPLICIT      NONE
C
         INTEGER       FTYPE,   LU
         CHARACTER*1   CHAR
C
         IF( FTYPE .EQ. 0 ) THEN
 100        CONTINUE
               READ(LU,110,END=150) CHAR
 110           FORMAT(A1)
            GO TO 100
 150        CONTINUE
            BACKSPACE LU
         ELSE IF( FTYPE .EQ. 1 ) THEN
 200        CONTINUE
               READ(LU,    END=250)
            GO TO 200
 250        CONTINUE
            BACKSPACE LU
         ELSE
            WRITE(*,*) '<<< TOEOF: Invalid file type code: ',
     *                 FTYPE,'. >>>'
         END IF
C
         RETURN
C
      END
C
C-----------------------------------------------------------------------
C
C     Same as TOEOF except returns number of records in file.
C
C-----------------------------------------------------------------------
C
      INTEGER FUNCTION ITOEOF(LU,FTYPE)
C
         IMPLICIT      LOGICAL*1 (A-Z)
C
         INTEGER       FTYPE,   LU
C
         ITOEOF = 0
         IF( FTYPE .EQ. 0 ) THEN
 100        CONTINUE
               READ(LU,110,END=150)
 110           FORMAT(/)
               ITOEOF = ITOEOF + 1
            GO TO 100
 150        CONTINUE
            BACKSPACE LU
         ELSE IF( FTYPE .EQ. 1 ) THEN
 200        CONTINUE
               READ(LU,    END=250)
               ITOEOF = ITOEOF + 1
            GO TO 200
 250        CONTINUE
            BACKSPACE LU
         ELSE
            WRITE(*,*) '<<< TOEOF: Invalid file type code: ',
     *                 FTYPE,'. >>>'
         END IF
C
         RETURN
C
      END
C
C-----------------------------------------------------------------------
C
C     Routines for input/output of double precision vector V of
C     length N on direct access unit UDA having logical record
C     length of LRECL * 8 bytes starting at record number CURREC.
C     On return, NXTREC is next available record in file.
C
C-----------------------------------------------------------------------
C
      SUBROUTINE ODVDA(V,N,UDA,LRECL8,CURREC,NXTREC)
C
         IMPLICIT       LOGICAL*1 (A-Z)
C
         INTEGER        N
         REAL*8         V(N)
C
         INTEGER        CURREC,   LRECL8,   NXTREC,   UDA
C
         CALL IODVDA(V,N,UDA,LRECL8,CURREC,NXTREC,0)
C
         RETURN
C
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE IDVDA(V,N,UDA,LRECL8,CURREC,NXTREC)
C
         IMPLICIT       LOGICAL*1 (A-Z)
C
         INTEGER        N
         REAL*8         V(N)
C
         INTEGER        CURREC,   LRECL8,   NXTREC,   UDA
C
         CALL IODVDA(V,N,UDA,LRECL8,CURREC,NXTREC,1)
C
         RETURN
C
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE IODVDA(V,N,UDA,LRECL8,CURREC,NXTREC,IOSW)
C
         IMPLICIT       LOGICAL*1 (A-Z)
C
         INTEGER        N
         REAL*8         V(N)
C
         INTEGER        CURREC,   IOSW,    LRECL8,   NXTREC,   UDA
C
         INTEGER        IREC,     J,        NREC
C
         NREC = N / LRECL8
         IF( NREC * LRECL8 .NE. N ) THEN
            NREC = NREC + 1
         END IF
         DO 10 IREC = 0 , NREC - 1
            IF( IOSW .EQ. 0 ) THEN
               WRITE(UDA,REC=CURREC+IREC)
     *              ( V(J) , J = IREC * LRECL8 + 1 ,
     *                           MIN((IREC + 1) * LRECL8,N) )
            ELSE
               READ (UDA,REC=CURREC+IREC)
     *              ( V(J) , J = IREC * LRECL8 + 1 ,
     *                           MIN((IREC + 1) * LRECL8,N) )
            END IF
 10      CONTINUE
C
         NXTREC = CURREC + NREC
C
         RETURN
C
      END
C
C-----------------------------------------------------------------------
C
C     Opens and reads SNAPDAT file, assumes header records are not
C     "important".
C
C-----------------------------------------------------------------------

      SUBROUTINE RDSNAP(USNAP,SNPNM,X,Y,N,RC)

         IMPLICIT        LOGICAL*1 (A-Z)

         INTEGER         USNAP,     N,      RC
         REAL            X(1),      Y(1)
         CHARACTER*(*)   SNPNM

         CHARACTER*7     LSNPNM

         LSNPNM = SNPNM(1:7)
         CALL MYOPEN(USNAP,LSNPNM//' SNAPDAT D',' ',0,0,0,RC)
         IF( RC .NE. 0 ) RETURN

         READ(USNAP,*)
         READ(USNAP,*)
         READ(USNAP,*)
         N = 0
 100     CONTINUE
            READ(USNAP,*,END=200) X(N+1), Y(N+1)
            N = N + 1
         GO TO 100

 200     CONTINUE
         CLOSE(USNAP)

         RETURN

      END
C
C------------------------------------------------------------------------------
C
C     Produces <fname> SNAPDAT D file for F vs X.
C
C------------------------------------------------------------------------------
      SUBROUTINE DVPL1(F,X,NX,FNAME,XNAME)

         IMPLICIT       LOGICAL*1 (A-Z)

         INTEGER        INDLNB

         INTEGER        NX
         REAL*8         F(NX),         X(NX)

         CHARACTER*(*)  FNAME,         XNAME

         CHARACTER*8    FNAME8
         INTEGER        MYORC,         J

         INTEGER        SNAPDAT
         PARAMETER    ( SNAPDAT = 80 )

         IF( NX .LE. 0 ) THEN
            WRITE(*,*) '<<< DVPL1:: Non--positive NX. >>>'
            RETURN
         END IF

         IF ( LEN(FNAME) .GT. 8 ) THEN
            FNAME8 = FNAME(1:8)
         ELSE
            FNAME8 = FNAME
         END IF

         CALL MYOPEN(SNAPDAT,FNAME8(1:INDLNB(FNAME8))//
     *                       ' SNAPDAT D',' ',0,0,0,MYORC)
         IF( MYORC .NE. 0 ) THEN
            WRITE(*,*) '<<< DVPL1:: Error opening SNAPDAT file. >>>'
            RETURN
         END IF

         WRITE(SNAPDAT,*) XNAME
         WRITE(SNAPDAT,*) FNAME
         WRITE(SNAPDAT,*)

         DO 10 J = 1 , NX
            WRITE(SNAPDAT,*) SNGL(X(J)), SNGL(F(J))
 10      CONTINUE

         CLOSE(SNAPDAT)

         RETURN

      END
C
C--------------------------------------------------------------------------
C
C     Dumps array unformatted on file <fn>.
C
C--------------------------------------------------------------------------

      SUBROUTINE DMPUTU(FN,F,NX,NY)

         IMPLICIT       LOGICAL (A-Z)

         INTEGER        DUNIT
         PARAMETER    ( DUNIT = 40 )

         CHARACTER*(*)  FN
         INTEGER        NX,           NY
         REAL*8         F(NX,NY)

         INTEGER        IX,           IY,            RC

         CALL MYOPEN(DUNIT,FN,' ',1,0,0,RC)
         IF( RC .NE. 0 ) THEN
            WRITE(*,*) '<<< DMPUTU:: Failure opening ',FN,'. >>>'
            RETURN
         END IF
         IF( NX * NY .GT. 0 ) THEN
             WRITE(DUNIT) NX,     NY
             WRITE(DUNIT) ( ( F(IX,IY) , IX = 1 , NX ) ,
     *                                   IY = 1 , NY )
         END IF
         CLOSE(DUNIT)

         RETURN

      END

C--------------------------------------------------------------------------
C
C     Reads array unformatted from file <fn>.
C
C--------------------------------------------------------------------------

      SUBROUTINE DMGETU(FN,F,NX,NY)

         IMPLICIT       LOGICAL (A-Z)

         INTEGER        DUNIT
         PARAMETER    ( DUNIT = 40 )

         CHARACTER*(*)  FN
         INTEGER        NX,           NY
         REAL*8         F(NX,NY)

         INTEGER        ONX,          ONY,
     *                  IX,           IY,            RC

         CALL MYOPEN(DUNIT,FN,'OLD',1,0,0,RC)
         IF( RC .NE. 0 ) THEN
            WRITE(*,*) '<<< DMGETU:: Failure opening ',FN,'. >>>'
            RETURN
         END IF
         IF( NX * NY .GT. 0 ) THEN
             READ(DUNIT) ONX,     ONY
             IF( ONX .EQ. NX  .AND.  ONY .EQ. NY ) THEN
                READ(DUNIT) ( ( F(IX,IY) , IX = 1 , NX ) ,
     *                                     IY = 1 , NY )
             ELSE
                WRITE(*,*) '<<< DMGETU:: Incompatible array',
     *                     ' dimensions. >>>'
                WRITE(*,1000) NX, NY, ONX, ONY
1000            FORMAT(' <<< DMGETU:: Requested (',I4,' x ',I4,'). ',
     *                 '  Stored (',I4,' x ',I4,'). >>>')
                RETURN
             END IF
         END IF
         CLOSE(DUNIT)

         RETURN

      END
C
C--------------------------------------------------------------------------
C
C     Dumps 3--array unformatted on file <fn>.
C
C--------------------------------------------------------------------------

      SUBROUTINE D3PUTU(FN,A,D1,D2,D3)

         IMPLICIT       LOGICAL (A-Z)

         INTEGER        DUNIT
         PARAMETER    ( DUNIT = 40 )

         CHARACTER*(*)  FN
         INTEGER        D1,           D2,            D3
         REAL*8         A(D1,D2,D3)

         INTEGER        I,            J,             K,
     *                  RC

         CALL MYOPEN(DUNIT,FN,' ',1,0,0,RC)
         IF( RC .NE. 0 ) THEN
            WRITE(*,*) '<<< D3PUTU:: Failure opening ',FN,'. >>>'
            RETURN
         END IF
         IF( D1 * D2 * D3 .GT. 0 ) THEN
             WRITE(DUNIT) D1,     D2,     D3
             WRITE(DUNIT) ( ( ( A(I,J,K) , I = 1 , D1 ) ,
     *                                     J = 1 , D2 ) ,
     *                                     K = 1 , D3 )
         END IF
         CLOSE(DUNIT)

         RETURN

      END

C--------------------------------------------------------------------------
C
C     Reads array unformatted from file <fn>.
C
C--------------------------------------------------------------------------

      SUBROUTINE D3GETU(FN,A,D1,D2,D3)

         IMPLICIT       LOGICAL (A-Z)

         INTEGER        DUNIT
         PARAMETER    ( DUNIT = 40 )

         CHARACTER*(*)  FN
         INTEGER        D1,           D2,            D3
         REAL*8         A(D1,D2,D3)

         INTEGER        OD1,          OD2,           OD3,
     *                  I,            J,             K,
     *                  RC

         CALL MYOPEN(DUNIT,FN,'OLD',1,0,0,RC)
         IF( RC .NE. 0 ) THEN
            WRITE(*,*) '<<< D3GETU:: Failure opening ',FN,'. >>>'
            RETURN
         END IF
         IF( D1 * D2 * D3  .GT. 0 ) THEN
             READ(DUNIT)  OD1,     OD2,      OD3
             IF( OD1 .EQ. D1  .AND.  OD2 .EQ. D2  .AND.
     *           OD3 .EQ. D3                     ) THEN
                READ(DUNIT) ( ( ( A(I,J,K) , I = 1 , D1 ) ,
     *                                       J = 1 , D2 ) ,
     *                                       K = 1 , D3 )
             ELSE
                WRITE(*,*) '<<< DMGETU:: Incompatible array',
     *                     ' dimensions. >>>'
                WRITE(*,1000) D1, D2, D3, OD1, OD2, OD3
1000            FORMAT(' <<< D3GETU:: Requested (',
     *                I4,' x ',I4,' x ',I4,'). ',
     *                 '  Stored (',
     *                I4,' x ',I4,' x ',I4,'). >>>')
                RETURN
             END IF
         END IF
         CLOSE(DUNIT)

         RETURN

      END

C-----------------------------------------------------------------------
C
C     Opens and writes SNAPDAT file.
C
C-----------------------------------------------------------------------

      SUBROUTINE WRSNAP(USNAP,SNPNM,XNAME,YNAME,TGNAME,X,Y,N,RC)

         IMPLICIT        LOGICAL   (A-Z)

         INTEGER         INDLNB

         INTEGER         USNAP,     N,      RC
         REAL*8          X(1),      Y(1)
         CHARACTER*(*)   SNPNM,     XNAME,  YNAME,  TGNAME

         CHARACTER*20    FNAME
         INTEGER         J

         FNAME = SNPNM(1:MIN(7,INDLNB(SNPNM)))//' SNAPDAT D'
         CALL MYOPEN(USNAP,FNAME,' ',0,0,0,RC)
         IF( RC .NE. 0 ) THEN
            WRITE(*,*) '<<< WRSNAP: Error opening ',FNAME,'. >>>'
            RETURN
         ELSE
            WRITE(*,*) '<<< WRSNAP: ',FNAME,' opened. >>>'
         END IF

         WRITE(USNAP,*) XNAME
         WRITE(USNAP,*) YNAME
         WRITE(USNAP,900) TGNAME
900      FORMAT(A80)
         DO 10 J = 1 , N
            WRITE(USNAP,*) SNGL(X(J)), SNGL(Y(J))
 10      CONTINUE
         WRITE(*,1000) N, X(1), X(N)
1000     FORMAT(' <<< WRSNAP: ',I5,' points, (',1PE8.2,'...',
     *          1PE8.2,'), dumped. >>>')

         CLOSE(USNAP)

         RETURN

      END

C-----------------------------------------------------------------------
C
C     Skips SKIP records on UNIT.
C
C-----------------------------------------------------------------------
C
      SUBROUTINE USKIP(UNIT,SKIP)
C
         INTEGER     SKIP,   UNIT
C
         INTEGER     ISKIP,  USER 

         DATA        USER / 6 /
C
         IF( SKIP .GT. 0 ) THEN
            DO 100 ISKIP = 1 , SKIP
               READ(UNIT,END=9000)
 100        CONTINUE
         END IF
C
         RETURN
C
9000     CONTINUE
            WRITE(USER,*) '<<< USKIP: unexpected end-of-file. >>>'
         RETURN
C
      END

c-----------------------------------------------------------------------
c
c     Read integer vector from file (one per line) ...
c
c-----------------------------------------------------------------------


      subroutine ivfget(filename,v,n,maxn)

         implicit          none

         integer           getu

         character*(*)     filename

         integer           n,      maxn
         integer           v(maxn)

         integer           vn
         integer           u
            
         logical           ltrace
         parameter       ( ltrace = .true. )

         u = getu()
         open(u,file=filename,status='old',form='formatted',err=900)
         n = 0
100      continue
            read(u,*,err=910,end=200) vn
            if( n .eq. maxn ) go to 920
            n = n + 1
            v(n) = vn
         go to 100
200      continue

         close(u)
         return

900      continue
            if( ltrace ) then
               write(0,9000) filename
9000           format(' ivfget: Error opening ',a)
            end if
         return

910      continue
            if( ltrace ) then
               write(0,9100) filename
9100           format(' ivfget: Error reading ',a)
            end if
            close(u)
         return

920      continue
            if( ltrace ) then
               write(0,9200) filename,     maxn
9200           format(' ivfget: too many integers in file ',a/
     *                ' ivfget: Can only read ',i6)
            end if
            close(u)
         return

      end

c-----------------------------------------------------------------------
c
c     Read double precision vector from file (one per line) ...
c
c-----------------------------------------------------------------------


      subroutine dvfget(filename,v,n,maxn)

         implicit          none

         integer           getu

         character*(*)     filename

         integer           n,      maxn
         real*8            v(maxn)

         real*8            vn
         integer           u
            
         logical           ltrace
         parameter       ( ltrace = .true. )

         n = 0
         u = getu()
         open(u,file=filename,status='old',form='formatted',err=900)
100      continue
            read(u,*,err=910,end=200) vn
            if( n .eq. maxn ) go to 920
            n = n + 1
            v(n) = vn
         go to 100
200      continue

         close(u)
         return

900      continue
            if( ltrace ) then
               write(0,9000) filename
9000           format(' dvfget: Error opening ',a)
            end if
         return

910      continue
            if( ltrace ) then
               write(0,9100) filename
9100           format(' dvfget: Error reading ',a)
            end if
            close(u)
         return

920      continue
            if( ltrace ) then
               write(0,9200) filename,     maxn
9200           format(' dvfget: Too many reals in file ',a/
     *                ' dvfget: Can only read ',i6)
            end if
            close(u)
         return

      end

c-----------------------------------------------------------------------
c
c     Read up to <nreq> values into arary <v> from a line in <fname> 
c     which begins with the character string <key>.  Number read 
c     returned in <nread> which should equal <nreq> unless error 
c     occured.  
c
c-----------------------------------------------------------------------

      subroutine dvfgetkey(filename,key,v,nreq,nread)

         implicit          none

         integer           getu,               indlnb
         real*8            s2r8
         character*1024    strip

         character*(*)     filename,           key

         integer           nreq,               nread
         real*8            v(nreq)

         character*1024    line_buffer,        tag,
     *                     val_buffer
         integer           i,                  posfb,
     *                     posend

         integer           u
            
         logical           ltrace
         parameter       ( ltrace = .true. )

         nread = 0

         u = getu()
         open(u,file=filename,status='old',form='formatted',err=900)

100      continue
            read(u,fmt='(a)',end=200,err=910) line_buffer 
            line_buffer = strip(line_buffer,' ')
            posfb = index(line_buffer,' ')
            if( posfb .gt. 0 ) then
               call sload(tag,' ')
               read(line_buffer(1:posfb-1),fmt='(a)',err=910) tag
               if( key .eq. tag(1:indlnb(tag)) ) then
                  do i = 1 , nreq
                     call sload(val_buffer,' ')
                     val_buffer = strip(line_buffer(
     *                               posfb+1:len(line_buffer)),' ')
                     if( indlnb(val_buffer) .gt. 0 ) then
                        posend = index(val_buffer,' ') - 1
                        if( posend .lt. 1 ) then
                           posend = len(val_buffer)
                        end if
                        v(i) = s2r8(val_buffer(1:posend))
                        nread = nread + 1
                        line_buffer = val_buffer
                        posfb = index(line_buffer,' ')
                     else 
         go to 500
                     end if
                  end do
         go to 500
150               continue
                  if( ltrace ) then 
                     write(0,*) 'dvfgetkey: Error reading values.'
                  end if
                  nread = -1
         go to 500
               end if
            end if
         go to 100

200      continue
         if( ltrace ) then 
            write(0,*) 'dvfgetkey: Could not locate key: ', key
         end if

500      continue
         close(u)
         return

900      continue
            if( ltrace ) then
               write(0,9000) filename
9000           format(' dvfgetkey: Error opening ',a)
            end if
         return

910      continue
            if( ltrace ) then
               write(0,9100) filename
9100           format(' dvfgetkey: Error reading ',a)
            end if
            close(u)
         return

      end

c-----------------------------------------------------------------------
c
c     Read tdvv double precision vectors from file (two per line) ...
c
c-----------------------------------------------------------------------


      subroutine dvvfget(fn,v1,v2,n,maxn)

         implicit          none

         integer           getu,        indlnb

         character*(*)     fn

         integer           n,           maxn
         real*8            v1(maxn),    v2(maxn)

         real*8            v1n,         v2n
         integer           u
            
         logical           ltrace
         parameter       ( ltrace = .true. )

         n = 0
         u = getu()
         open(u,file=fn(1:indlnb(fn)),
     *        status='old',form='formatted',err=900)
100      continue
            read(u,*,err=910,end=200) v1n, v2n
            if( n .eq. maxn ) go to 920
            n = n + 1
            v1(n) = v1n
            v2(n) = v2n
         go to 100
200      continue

         close(u)
         return

900      continue
            if( ltrace ) then
               write(0,9000) fn(1:indlnb(fn))
9000           format(' dvvfget: Error opening ',a)
            end if
         return

910      continue
            if( ltrace ) then
               write(0,9100) fn(1:indlnb(fn))
9100           format(' dvvfget: Error reading ',a)
            end if
            close(u)
         return

920      continue
            if( ltrace ) then
               write(0,9200) fn(1:indlnb(fn)),     maxn
9200           format(' dvvfget: Too many reals in file ',a/
     *                ' dvvfget: Can only read ',i6)
            end if
            close(u)
         return

      end
c-----------------------------------------------------------
c     Returns a double precision vector (one-dimensional 
c     array) read from file 'fname'.  If 'fname' is the 
c     string '-', the vector is read from standard input.
c
c     The file should contain one number per line; invalid 
c     input is ignored.
c
c     This routine illustrates  a general technique for 
c     reading data from a FORMATTED (ASCII) file.  In
c     Fortran, one associates a "logical unit number"
c     (an integer) with a file via the OPEN statement.
c     The unit number can then be used as the first 
c     "argument" of the READ and WRITE statements to 
c     perform input and output on the file. 
c     
c     Fortran reserves the following unit numbers:
c
c     5      terminal input (stdin) 
c     6      terminal output (stdout) 
c     0      error output on Unix systems (stderr)  
c-----------------------------------------------------------

      subroutine dvfrom(fname,v,n,maxn)
c-----------------------------------------------------------
c     Arguments:
c
c        fname:  (I)    File name
c        v:      (O)    Return vector
c        n:      (O)    Length of v (# read)
c        maxn:   (I)    Maximum number to read
c-----------------------------------------------------------
         implicit         none
c-----------------------------------------------------------
c        The integer functions 'indlnb' and 'getu' are 
c        defined in the 'p329f' library.
c-----------------------------------------------------------
         integer          indlnb,      getu
c-----------------------------------------------------------
c        Declaration of routine arguments: note 
c        "adjustable dimensioning" of v; any array which 
c        is declared with adjustable dimesions must be 
c        a subroutine argument; any adjustable dimensions
c        must also be subroutine arguments.
c-----------------------------------------------------------
         character*(*)    fname
         integer          n,           maxn
         real*8           v(maxn)

c-----------------------------------------------------------
c        Programming style: Use parameter (ustdin) rather 
c        than constant value (5) for stdin logical unit #
c-----------------------------------------------------------
         integer          ustdin
         parameter      ( ustdin = 5 )

c-----------------------------------------------------------
c        Local variables:
c
c        vn:     Current number read from input
c        ufrom:  Logical unit number for READ 
c        rc:     For storing return status from READ
c-----------------------------------------------------------
         real*8           vn
         integer          ufrom,       rc

c-----------------------------------------------------------
c        Intialize 
c-----------------------------------------------------------
         n = 0

c-----------------------------------------------------------
c        Read from stdin?
c-----------------------------------------------------------
         if( fname .eq. '-' ) then
c-----------------------------------------------------------
c           Set unit number to stdin default
c-----------------------------------------------------------
            ufrom = ustdin
         else
c-----------------------------------------------------------
c           Get an available unit number
c-----------------------------------------------------------
            ufrom = getu()
c-----------------------------------------------------------
c           Open the file for formatted I/O 
c-----------------------------------------------------------
            open(ufrom,file=fname(1:indlnb(fname)),
     &           form='formatted',status='old',iostat=rc)
            if( rc .ne. 0 ) then
c-----------------------------------------------------------
c              Couldn't open the file, print error message
c              and return.
c-----------------------------------------------------------
               write(0,*) 'dvfrom: Error opening ',
     &                    fname(1:indlnb(fname))
               return
            end if
         end if

c-----------------------------------------------------------
c        Input numbers into vector (one per line) until
c        EOF or maximum allowable number read
c-----------------------------------------------------------
 100     continue
            read(ufrom,*,iostat=rc,end=200)  vn
            if( rc .eq. 0 ) then
               n = n + 1
               if( n .gt. maxn ) then
                  write(0,*)  'dvfrom: Read maximum of ',
     &                         maxn, ' from ', 
     &                         fname(1:indlnb(fname))
                     n = maxn 
                     return
               end if
               v(n) = vn
            end if
         go to 100
 200     continue

c-----------------------------------------------------------
c        If we are reading from a file, close the file.
c        This releases the unit number for subsequent use.
c-----------------------------------------------------------
         if( ufrom .ne. ustdin ) then
            close(ufrom)
         end if

         return

      end
c-----------------------------------------------------------
c     Writes a double precision vector to file 'fname'.
c     If fname is the string '-' then the vector is written
c     to standard output.
c-----------------------------------------------------------

      subroutine dvto(fname,v,n)
c-----------------------------------------------------------
c     Arguments:
c
c        fname:  (I)    File name
c        v:      (I)    Vector to be written
c        n:      (I)    Length of vector
c-----------------------------------------------------------
         implicit         none

         integer          getu,       indlnb

         character*(*)    fname
         integer          n
         real*8           v(n)

         integer          ustdout
         parameter      ( ustdout = 6 )

         integer          i,      uto,       rc

         if( fname .eq. '-' ) then
            uto = ustdout
         else
            uto = getu()
            open(uto,file=fname(1:indlnb(fname)),
     &           form='formatted',iostat=rc)
            if( rc .ne. 0 ) then
               write(0,*) 'dvto: Error opening ',
     &                    fname(1:indlnb(fname))
               return
            end if
         end if

         do i = 1 , n
            write(uto,*) v(i)
         end do

         if( uto .ne. ustdout ) then
            close(uto)
         end if

         return

      end
