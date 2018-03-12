PROGRAM test_hypotenuse
!
!  Purpose:
!    Program to test the operation of subroutine calc_hypotenuse.
!
!  Record of revisions:
!      Date       Programmer          Description of change
!      ====       ==========          =====================
!    11/18/06    S. J. Chapman        Original code
!
IMPLICIT NONE

! Data dictionary: declare variable types & definitions
REAL :: s1             ! Length of side 1
REAL :: s2             ! Length of side 2
REAL :: hypot          ! Hypotenuse

! Get the lengths of the two sides.
WRITE (*,*) 'Program to test subroutine calc_hypotenuse: ' 
WRITE (*,*) 'Enter the length of side 1: '
READ (*,*) s1
WRITE (*,*) 'Enter the length of side 2: '
READ (*,*) s2
 
! Call calc_hypotenuse.
CALL calc_hypotenuse ( s1, s2, hypot )

! Write out hypotenuse.
WRITE (*,1000) hypot
1000 FORMAT (1X,'The length of the hypotenuse is: ', F10.4 )
       
END PROGRAM test_hypotenuse
