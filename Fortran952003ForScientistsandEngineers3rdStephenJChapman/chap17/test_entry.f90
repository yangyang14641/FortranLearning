PROGRAM test_entry
REAL :: a = 1., b = 2., c = 1., d = 2.
CALL initl ( a, b, c, d )
DO I = 1, 10
   CALL eval3 ( REAL(i), result )
   WRITE (*,*) 'EVAL3(', i, ') = ', result
END DO
END PROGRAM

SUBROUTINE eval3 ( x, result )
!
!     Evaluates a third order polynomial of the form:
!        RESULT = A + B*X + C*X**2 + D*X**3
!
!     Declare calling arguments
IMPLICIT NONE
REAL :: a1, b1, c1, d1, x, result

! Declare local variables
REAL, SAVE :: a, b, c, d

! Calculate result
result = a + b**x + c*x**2 + d*x**3 

RETURN

! Entry INITL specifies the values of a, b, c, and d
! to be used when evaluating the polynomial.

ENTRY initl(a1, b1, c1, d1)
a = a1
b = b1
c = c1
d = d1

RETURN
END SUBROUTINE
