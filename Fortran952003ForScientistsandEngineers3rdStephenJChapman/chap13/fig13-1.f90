MODULE module_example
IMPLICIT NONE
REAL :: x = 100.
REAL :: y = 200.
END MODULE

PROGRAM scoping_test
USE module_example
IMPLICIT NONE
INTEGER :: i = 1, j = 2
WRITE (*,'(A25,2I7,2F7.1)') ' Beginning:', i, j, x, y
CALL sub1 ( i, j )
WRITE (*,'(A25,2I7,2F7.1)') ' After sub1:', i, j, x, y
CALL sub2 
WRITE (*,'(A25,2I7,2F7.1)') ' After sub2:', i, j, x, y
CONTAINS
   SUBROUTINE sub2
   REAL :: x
   x = 1000.
   y = 2000.
   WRITE (*,'(A25,2F7.1)') ' In sub2:', x, y
   END SUBROUTINE sub2
END PROGRAM scoping_test

SUBROUTINE sub1 (i,j)
IMPLICIT NONE
INTEGER, INTENT(INOUT) :: i, j
INTEGER, DIMENSION(5) :: array
WRITE (*,'(A25,2I7)') ' In sub1 before sub2:', i, j
CALL sub2
WRITE (*,'(A25,2I7)') ' In sub1 after sub2:', i, j
array = (/ (1000*i, i=1,5) /)
WRITE (*,'(A25,7I7)') ' After array def in sub2:', i, j, array
CONTAINS
   SUBROUTINE sub2
   INTEGER :: i
   i = 1000
   j = 2000
   WRITE (*,'(A25,2I7)') 'In sub1 in sub2:', i, j
   END SUBROUTINE sub2
END SUBROUTINE sub1

