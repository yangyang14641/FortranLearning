MODULE evaluate
IMPLICIT NONE
PRIVATE
PUBLIC eval3, initl

! Declare shared data.
REAL, SAVE :: a, b, c, d

! Declare procedures
CONTAINS
   SUBROUTINE eval3 ( x, result )
   !
   !     Evaluates a third order polynomial of the form:
   !        RESULT = A + B*X + C*X**2 + D*X**3
   !
   ! Declare calling arguments
   REAL, INTENT(IN) :: x
   REAL, INTENT(OUT) :: result
   
   ! Calculate result
   result = a + b**x + c*x**2 + d*x**3 
   
   END SUBROUTINE eval3
   
   SUBROUTINE initl (a1, b1, c1, d1 )
   !
   ! Subroutine INITL specifies the values of a, b, c, and d
   ! to be used when evaluating the polynomial.
   !
   REAL, INTENT(IN) :: a1, b1, c1, d1
   a = a1
   b = b1
   c = c1
   d = d1
   END SUBROUTINE initl
END MODULE evaluate

PROGRAM test
USE evaluate
REAL :: a = 1., b = 2., c = 1., d = 2.
CALL initl ( a, b, c, d )
DO i = 1, 10
   CALL eval3 ( REAL(i), result )
   WRITE (*,*) 'EVAL3(', i, ') = ', result
END DO
END PROGRAM test_noentry
