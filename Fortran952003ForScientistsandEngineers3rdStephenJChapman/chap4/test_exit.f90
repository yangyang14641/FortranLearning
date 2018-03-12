PROGRAM test_exit
INTEGER :: i
DO i = 1, 5
   IF ( i == 3 ) EXIT
   WRITE (*,*) i
END DO
WRITE (*,*) 'End of loop!'
END PROGRAM test_exit
