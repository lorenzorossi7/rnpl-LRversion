C-----------------------------------------------------------------------
C
C     Reads selected function section from UNIT.
C
C-----------------------------------------------------------------------
C
      SUBROUTINE DRDSEC(DSC,DSCTMP,LSC,IVEC,LIVEC,NLDREC,IF,NF,ISC,
     *           UNIT,RC)
C
         INTEGER      LIVEC, LSC
         REAL*8       DSC(LIVEC), DSCTMP(LSC)
         INTEGER      IVEC(LIVEC)
         INTEGER      I, IF, ISC, J, NF, NLDREC, RC, UNIT
C
         DO 10 I = 1 , LIVEC
            READ(UNIT,REC=NLDREC+(IVEC(I)-1)*NF+IF,ERR=20)
     *          ( DSCTMP(J) , J = 1 , ISC )
            DSC(I) = DSCTMP(ISC)
 10      CONTINUE
         RC = 0
C
         RETURN
C
 20      CONTINUE
         RC = 1
C
         RETURN
C
      END
C
C-----------------------------------------------------------------------
C
C     "Single precision" version of previous routine.
C
C-----------------------------------------------------------------------
C
      SUBROUTINE SRDSEC(SSC,DSCTMP,LSC,IVEC,LIVEC,NLDREC,IF,NF,ISC,
     *           UNIT,RC)
C
         INTEGER      LIVEC, LSC
         REAL*8       DSCTMP(LSC)
         REAL         SSC(LIVEC)
         INTEGER      IVEC(LIVEC)
         INTEGER      I, IF, ISC, J, NF, NLDREC, RC, UNIT
C
         DO 10 I = 1 , LIVEC
            READ(UNIT,REC=NLDREC+(IVEC(I)-1)*NF+IF,ERR=20)
     *          ( DSCTMP(J) , J = 1 , ISC )
            SSC(I) = DSCTMP(ISC)
 10      CONTINUE
         RC = 0
C
         RETURN
C
 20      CONTINUE
         RC = 1
C
         RETURN
C
      END
C
C-----------------------------------------------------------------------
C
C     Reads selected function slice from UNIT.
C
C-----------------------------------------------------------------------
C
      SUBROUTINE DRDSLI(DSL,DSLTMP,LSL,IVEC,LIVEC,NLDREC,IF,NF,ISL,
     *                  UNIT,RC)
C
         INTEGER      LIVEC, LSL
         REAL*8       DSL(LIVEC), DSLTMP(LSL)
         INTEGER      IVEC(LIVEC)
         INTEGER      I, IF, ISL, NF, NLDREC, RC, UNIT
C
         READ(UNIT,REC=NLDREC+(ISL-1)*NF+IF,ERR=20)
     *       ( DSLTMP(I) , I = 1 , IVEC(LIVEC) )
         DO 10 I = 1 , LIVEC
            DSL(I) = DSLTMP(IVEC(I))
 10      CONTINUE
         RC = 0
C
         RETURN
C
 20      CONTINUE
         RC = 1
C
         RETURN
C
      END
C
C-----------------------------------------------------------------------
C
C     Single precision version of previous routine.
C
C-----------------------------------------------------------------------
C
      SUBROUTINE SRDSLI(SSL,DSLTMP,LSL,IVEC,LIVEC,NLDREC,IF,NF,ISL,
     *                  UNIT,RC)
C
         INTEGER      LIVEC, LSL
         REAL*8       DSLTMP(LSL)
         REAL         SSL(LIVEC)
         INTEGER      IVEC(LIVEC)
         INTEGER      I, IF, ISL, NF, NLDREC, RC, UNIT
C
         READ(UNIT,REC=NLDREC+(ISL-1)*NF+IF,ERR=20)
     *       ( DSLTMP(I) , I = 1 , IVEC(LIVEC) )
         DO 10 I = 1 , LIVEC
            SSL(I) = DSLTMP(IVEC(I))
 10      CONTINUE
         RC = 0
C
         RETURN
C
 20      CONTINUE
         RC = 1
C
         RETURN
C
      END
C
C-----------------------------------------------------------------------
C
C     Reads selected function "matrix" of values from UNIT.
C
C-----------------------------------------------------------------------
C
      SUBROUTINE DRDMAT(DMAT,DSLTMP,LSL,ISLVEC,LSLVEC,ISCVEC,LSCVEC,
     *                  NLDREC,IF,NF,UNIT,RC)
C
         INTEGER      LSCVEC, LSL, LSLVEC
         REAL*8       DMAT(LSLVEC,LSCVEC), DSLTMP(LSL)
         INTEGER      ISLVEC(LSLVEC), ISCVEC(LSCVEC)
         INTEGER      I, IF, NF, NLDREC, RC, UNIT
C
         DO 10 I = 1 , LSCVEC
            CALL DRDSLI(DMAT(1,I),DSLTMP,LSL,ISLVEC,LSLVEC,NLDREC,
     *                  IF,NF,ISCVEC(I),UNIT,RC)
            IF( RC .NE. 0 ) GO TO 20
 10      CONTINUE
C
 20      RETURN
C
      END
C
C-----------------------------------------------------------------------
C
C     Single precision version of previous routine.
C
C-----------------------------------------------------------------------
C
      SUBROUTINE SRDMAT(SMAT,DSLTMP,LSL,ISLVEC,LSLVEC,ISCVEC,LSCVEC,
     *                  NLDREC,IF,NF,UNIT,RC)
C
         INTEGER      LSCVEC, LSL, LSLVEC
         REAL         SMAT(LSLVEC,LSCVEC)
         REAL*8       DSLTMP(LSL)
         INTEGER      ISLVEC(LSLVEC), ISCVEC(LSCVEC)
         INTEGER      I, IF, NF, NLDREC, RC, UNIT
C
         DO 10 I = 1 , LSCVEC
            CALL SRDSLI(SMAT(1,I),DSLTMP,LSL,ISLVEC,LSLVEC,NLDREC,
     *                  IF,NF,ISCVEC(I),UNIT,RC)
            IF( RC .NE. 0 ) GO TO 20
 10      CONTINUE
C
 20      RETURN
C
      END
