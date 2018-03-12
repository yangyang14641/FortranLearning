MODULE mymod
IMPLICIT NONE

TYPE :: t
   REAL :: x
   REAL :: y
CONTAINS
   PROCEDURE :: sum
END TYPE

REAL FUNCTION sum(val)
TYPE(t) :: val
sum = val%x + val%y
END FUNCTION sum

END MODULE mymod

PROGRAM test
USE mymod
IMPLICIT NONE
TYPE(t) :: v
v%x = 1
v%y = 2
WRITE (*,*) t%sum

END PROGRAM test
