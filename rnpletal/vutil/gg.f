C
C---------------------------------------------------------------------
C
C     Returns index of first occurence of KEY in first N elements of
C     V, or 0 if not found.
C
C---------------------------------------------------------------------
C
      INTEGER FUNCTION IVINDX(V,N,KEY)
C
         INTEGER     V(1)
         INTEGER     I, KEY, N
C
         IVINDX = 0
         IF( N .GT. 0 ) THEN
            DO 10 I = 1 , N
               IF( V(I) .EQ. KEY ) GO TO 20
 10         CONTINUE
         END IF
C
         RETURN
C
 20      IVINDX = I
C
         RETURN
C
      END
