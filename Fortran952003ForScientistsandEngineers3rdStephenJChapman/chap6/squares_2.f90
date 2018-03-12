PROGRAM squares_2

IMPLICIT NONE

INTEGER :: i
INTEGER, DIMENSION(-5:5) :: number, square
 
! Initialize number and calculate square.
DO i = -5, 5
   number(i) = i                 ! Initialize number
   square(i) = number(i)**2      ! Calculate square 
END DO
 
! Write out each number and its square.
DO i = -5, 5
   WRITE (*,100) number(i), square(i)
   100 FORMAT (1X,'Number = ',I6,'  Square = ',I6)
END DO
 
END PROGRAM squares_2

