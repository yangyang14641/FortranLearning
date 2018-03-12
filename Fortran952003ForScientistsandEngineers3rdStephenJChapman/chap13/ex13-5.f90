PROGRAM junk
IMPLICIT NONE
REAL :: a = 3, b = 4, output
INTEGER :: i = 0
call sub1(a, i, output)
WRITE (*,*) 'The output is ', output

CONTAINS
   SUBROUTINE sub1(x, j, junk)
   REAL, INTENT(IN) :: x
   INTEGER, INTENT(IN) :: j
   REAL, INTENT(OUT) :: junk
   junk = (x - j) / b
   END SUBROUTINE sub1
END PROGRAM junk
