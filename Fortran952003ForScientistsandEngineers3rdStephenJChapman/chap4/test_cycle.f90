PROGRAM test_cycle
INTEGER :: i
DO i = 1, 5
   IF ( i == 3 ) CYCLE
   WRITE (*,*) i
END DO
WRITE (*,*) 'End of loop!'
END PROGRAM test_cycle
