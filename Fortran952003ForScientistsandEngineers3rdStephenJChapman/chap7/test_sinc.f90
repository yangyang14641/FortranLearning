PROGRAM test_sinc
!
!  Purpose:
!    To test the sinc function sinc(x)
!
IMPLICIT NONE

! Data dictionary: declare function types
REAL :: sinc             ! sinc function

! Data dictionary: declare variable types & definitions
REAL :: x                ! Input value to evaluate

! Get value to evaluate
WRITE (*,*) 'Enter x: '
READ (*,*) x
 
! Write answer.
WRITE (*,'(1X,A,F8.5)') 'sinc(x) = ', sinc(x)
 
END PROGRAM test_sinc
