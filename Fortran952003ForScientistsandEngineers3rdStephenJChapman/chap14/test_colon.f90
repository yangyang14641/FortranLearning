PROGRAM test_colon
IMPLICIT NONE
REAL, DIMENSION(8) :: x
INTEGER :: i
x = (/ 1.1, 2.2, 3.3, 4.4, 5.5, 6.6, 7.7, 8.8 /)

WRITE (*,100) (i, x(i), i = 1, 8)
100 FORMAT (/,1X,'The output values are: '/, &
          3(5X,'X(',I2,') = ',F10.4)) 

WRITE (*,200) (i, x(i), i = 1, 8)
200 FORMAT (/,1X,'The output values are: '/,
          3(:,5X,'X(',I2,') = ',F10.4)) 

END PROGRAM test_colon
