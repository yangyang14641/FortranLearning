PROGRAM polyfn 
!
! This program evaluates a third order polynomial 
! of the form:
!    RES = A + B*X + C*X**2 + D*X**3
! using a statement function.

IMPLICIT NONE

! Declare local variables.
REAL :: a, b, c, d, x, y 
INTEGER :: i

! Declare dummy arguments of the statement function.
REAL :: a1, b1, c1, d1, x1, res

! Declare statement function res.
res(a1,b1,c1,d1,x1) = a1 + b1**x1 + c1*x1**2 + d1*x1**3 

! Set up coefficients of polynomial res.
a = 1.
b = 2.
c = 1.
d = 2.

! Evaluate polynomial for x values of 1 through 10.
C
DO i = 1, 10
   x = REAL(i)
   y = res(a,b,c,d,x)
   WRITE (*,*) 'y(',i,') = ', y
END DO

END PROGRAM polyfn
