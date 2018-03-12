PROGRAM test_plot
!
!  Purpose:
!    Program to test subroutine "plot".  This program generates
!    a data set based on the function:
!       y(t) = 10. * EXP (-t/5) * SIN(t)
!    starting at t = 0 for 12 seconds, with a step size dt = 1/3.
!
IMPLICIT NONE

! Declare local variables:
LOGICAL :: default = .TRUE.       ! Default plot boundaries
INTEGER :: i, unit = 6
REAL :: minplt = 0.,  maxplt = 0.
INTEGER :: npts = 37
REAL, DIMENSION(0:36) :: y

! Generate function.
DO i = 0, 36
   y(i) = 10. * EXP ( -REAL(i)/15. ) * SIN ( REAL(i)/3. )
END DO
 
! Plot data
CALL plot ( y, npts, minplt, maxplt, default, unit )
 
END PROGRAM test_plot
