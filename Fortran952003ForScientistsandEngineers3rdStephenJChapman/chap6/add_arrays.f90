PROGRAM add_arrays

IMPLICIT NONE

INTEGER :: i
REAL, DIMENSION(4) :: a = (/ 1., 2., 3., 4./) 
REAL, DIMENSION(4) :: b = (/ 5., 6., 7., 8./) 
REAL, DIMENSION(4) :: c, d 

! Element by element addition
DO i = 1, 4
   c(i) = a(i) + b(i)
END DO

! Whole array addition
d = a + b

! Write out results
WRITE (*,100) 'c', c
WRITE (*,100) 'd', d
100 FORMAT (' ',A,' = ',5(F6.1,1X))

END PROGRAM
