PROGRAM test_quadf
!
!  Purpose:     
!    Program to test function quadf.
!
IMPLICIT NONE
 
! Data dictionary: declare variable types & definitions
REAL :: quadf                   ! Declare function
REAL :: a, b, c, x              ! Declare local variables

! Get input data.
WRITE (*,*) 'Enter quadratic coefficients a, b, and c: '
READ  (*,*) a, b, c
WRITE (*,*) 'Enter location at which to evaluate equation: '
READ  (*,*) x
 
! Write out result.
WRITE (*,100) ' quadf(', x, ') = ', quadf(x,a,b,c)
100 FORMAT (A,F10.4,A,F12.4)

END PROGRAM test_quadf
