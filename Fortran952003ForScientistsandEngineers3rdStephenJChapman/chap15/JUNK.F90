PROGRAM ex11_17
REAL :: exp1 = 0., x = 1., exp2 = 0.
INTEGER :: i
DO i = 0, 11
   exp1 = exp1 + x**i / factorial(i)
END DO

DO i = 11, 0, -1
   exp2 = exp2 + x**i / factorial(i)
END DO

WRITE (*,*) exp1
WRITE (*,*) exp2
WRITE (*,*) exp(1)


END PROGRAM
