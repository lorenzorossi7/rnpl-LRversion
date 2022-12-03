      DOUBLE PRECISION FUNCTION D1MACH(I)
c-----------------------------------------------------------------------------
c     Hardcoded for PC with Intel processors (or other IEEE in 
c     principle).
c-----------------------------------------------------------------------------
      DOUBLE PRECISION DMACH(5)

      DATA   DMACH /
     & 2.225073858507202d-308,
     & 1.797693134862315d+308,
     & 1.110223024625157d-016,
     & 2.220446049250313d-016,
     & 0.301029995663981d0
     &             /
      SAVE
      IF (I .LT. 1 .OR. I .GT. 5)
     $    CALL ERRMSG( 'D1MACH--ARGUMENT OUT OF BOUNDS',.TRUE.)
      D1MACH = DMACH(I)

      RETURN
      END

c************************* ERROR MESSAGES *********************************

        SUBROUTINE  ERRMSG( MESSAG, FATAL )
        IMPLICIT DOUBLE PRECISION ( A-H, O-Z )
C
C        PRINT OUT A WARNING OR ERROR MESSAGE;  ABORT IF ERROR
C
        LOGICAL       FATAL, ONCE
        CHARACTER*(*) MESSAG
        INTEGER       MAXMSG, NUMMSG
        SAVE          MAXMSG, NUMMSG, ONCE
        DATA NUMMSG / 0 /,  MAXMSG / 100 /,  ONCE / .FALSE. /
C
C
        IF ( FATAL )  THEN
           WRITE ( *, '(2A)' )  ' ERROR >>> ', MESSAG
           STOP
        END IF
C
        NUMMSG = NUMMSG + 1
        IF ( NUMMSG.GT.MAXMSG )  THEN
           IF ( .NOT.ONCE )  WRITE ( *,99 )
           ONCE = .TRUE.
        ELSE
           WRITE ( *, '(2A)' )  ' WARNING >>> ', MESSAG
        ENDIF
C
        RETURN
C
   99 FORMAT( ///,' >>> TOO MANY WARNING MESSAGES --  ',
     $   'THEY WILL NO LONGER BE PRINTED  <<<', /// )
        END


