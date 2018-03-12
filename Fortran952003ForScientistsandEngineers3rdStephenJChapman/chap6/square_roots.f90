PROGRAM square_roots
 
IMPLICIT NONE
 
INTEGER :: i
REAL, DIMENSION(10) :: value = (/ (i, i=1,10) /) 
REAL, DIMENSION(10) :: square_root

! Calculate the square roots of the numbers.
DO i = 1, 10
   square_root(i) = SQRT(value(i))
END DO

! Write out each number and its square root.
DO i = 1, 10
   WRITE (*,100) value(i), square_root(i)
   100 FORMAT (1X,'Value = ',F5.1,'  Square Root = ',F10.4)
END DO

END PROGRAM square_roots
