PROGRAM exercise13_4
IMPLICIT NONE
REAL :: x = 12., y = -3., result
INTEGER :: i = 6, j = 4
WRITE (*,100) ' Before call: x, y, i, j = ', x, y, i, j
100 FORMAT (A,2F6.1,2I6)
result = exec(y,i)
WRITE (*,*) 'The result is ', result
WRITE (*,100) ' After call:  x, y, i, j = ', x, y, i, j

CONTAINS
   REAL FUNCTION exec(x,i)
   REAL, INTENT(IN) :: x
   INTEGER, INTENT(IN) :: i
   WRITE (*,100) ' In exec:     x, y, i, j = ', x, y, i, j
   100 FORMAT (A,2F6.1,2I6)
   exec = ( x + y ) / REAL ( i + j )
   j = i
   END FUNCTION exec
END PROGRAM exercise13_4
