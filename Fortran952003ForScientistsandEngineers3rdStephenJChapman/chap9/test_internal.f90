PROGRAM test_internal
!
!  Purpose: 
!    To illustrate the use of an internal procedure.
! 
!  Record of revisions:
!       Date       Programmer          Description of change
!       ====       ==========          =====================
!     07/03/02    S. J. Chapman        Original code
!
IMPLICIT NONE

! Data dictionary: declare constants
REAL, PARAMETER :: PI = 3.141592       ! PI

! Data dictionary: declare variable types & definitions
REAL :: theta                          ! Angle in degrees

! Get desired angle
WRITE (*,*) 'Enter desired angle in degrees: ' 
READ (*,*) theta

! Calculate and display the result.
WRITE (*,'(A,F10.4)') ' The secant is ', secant(theta)

! Note that the WRITE above was the last executable statement.
! Now, declare internal procedure secant:
CONTAINS
   REAL FUNCTION secant(angle_in_degrees)
   !
   !  Purpose:
   !    To calculate the secant of an angle in degrees.
   !
   REAL :: angle_in_degrees

   ! Calculate secant
   secant = 1. / cos( angle_in_degrees * pi / 180. )

   END FUNCTION secant

END PROGRAM test_internal
