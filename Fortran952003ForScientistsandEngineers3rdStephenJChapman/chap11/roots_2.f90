PROGRAM roots_2
!
!  Purpose:
!    To find the roots of a quadratic equation 
!       A * X**2 + B * X + C = 0.
!    using complex numbers to eliminate the need to branch 
!    based on the value of the discriminant. 
!
!  Record of revisions:
!       Date       Programmer          Description of change
!       ====       ==========          =====================
!     12/01/06    S. J. Chapman        Original code
!
IMPLICIT NONE

! Data dictionary: declare variable types & definitions
REAL :: a               ! The coefficient of X**2
REAL :: b               ! The coefficient of X
REAL :: c               ! The constant coefficient
REAL :: discriminant    ! The discriminant of the quadratic eqn
COMPLEX :: x1           ! First solution to the equation
COMPLEX :: x2           ! Second solution to the equation
 
! Get the coefficients.
WRITE (*,1000) 
1000 FORMAT (' Program to solve for the roots of a quadratic', &
           /,' equation of the form A * X**2 + B * X + C = 0. ' )
WRITE (*,1010) 
1010 FORMAT (' Enter the coefficients A, B, and C: ')
READ (*,*) a, b, c
 
! Calculate the discriminant 
discriminant = b**2 - 4. * a * c
 
! Calculate the roots of the equation
x1 = ( -b + SQRT( CMPLX(discriminant,0.) ) ) / (2. * a)
x2 = ( -b - SQRT( CMPLX(discriminant,0.) ) ) / (2. * a)
 
! Tell user.
WRITE (*,*) 'The roots are: '
WRITE (*,1020) '   x1 = ', REAL(x1), ' + i ', AIMAG(x1)
WRITE (*,1020) '   x2 = ', REAL(x2), ' + i ', AIMAG(x2)
1020 FORMAT (A,F10.4,A,F10.4)
 
END PROGRAM roots_2
