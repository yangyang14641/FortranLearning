PROGRAM test_ave_value2
!
!  Purpose:
!    To test function ave_value by calling it with the intrinsic 
!    function sin.  
!
!  Record of revisions:
!      Date       Programmer          Description of change
!      ====       ==========          =====================
!    12/31/06    S. J. Chapman        Original code
!
IMPLICIT NONE

! Declare functions:
REAL :: ave_value              ! Average value of function
INTRINSIC sin                  ! Function to evaluate

! Declare parameters:
REAL, PARAMETER :: TWOPI = 6.283185 ! 2 * Pi

! Declare local variables:
REAL :: ave                    ! Average of my_function

! Call function with func=sin.
ave = ave_value ( sin, 0., TWOPI, 101 )
WRITE (*,1000) 'SIN', ave
1000 FORMAT (1X,'The average value of ',A,' between 0. and twopi is ', &
             F16.6,'.')
 
END PROGRAM test_ave_value2
