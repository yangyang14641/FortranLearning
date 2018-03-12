PROGRAM test_all_array
IMPLICIT NONE

INTEGER,DIMENSION(3) :: i = (/ 1, 3, 2 /)
REAL,DIMENSION(4) :: a = (/1., 2., 3., 4. /)
REAL,DIMENSION(:),ALLOCATABLE :: aaa

! Print the value of x
WRITE(*,*) 'i = ', i
WRITE (*,*) a(i)

! 
aaa = a
WRITE (*,*) aaa(i)

END PROGRAM test_all_array