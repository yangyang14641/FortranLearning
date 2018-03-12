PROGRAM main 
IMPLICIT NONE
REAL :: a, b
REAL, DIMENSION(5) :: c
INTEGER :: i 
COMMON / common1 / a, b, c, i 
...
CALL sub11
END PROGRAM main

SUBROUTINE sub11 
REAL :: x
REAL,DIMENSION(5) :: y
INTEGER :: i, j
COMMON / common1 / x, y, i, j 
...
END SUBROUTINE sub11
