PROGRAM named_loop
IMPLICIT NONE
INTEGER :: i

i = 0
loopname: DO 
   I = I + 1
   IF ( I == 3 ) CYCLE loopname
   IF ( i > 5 ) EXIT loopname
   WRITE (*,*) i
END DO loopname
END PROGRAM named_loop
