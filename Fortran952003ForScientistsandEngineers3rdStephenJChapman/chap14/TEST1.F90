PROGRAM test
USE big_integers
IMPLICIT NONE

TYPE (BIG_INTEGER) :: a, b, c
INTEGER :: i = 8, j = 4, k

a = i
b = big_int(j)
c = a + b

k = 1

CALL print(a)
CALL print(b)
CALL print(c)

READ (*,*) k

END PROGRAM
