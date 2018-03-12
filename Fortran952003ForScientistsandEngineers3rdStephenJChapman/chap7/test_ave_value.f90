PROGRAM test_ave_value
!
!  Purpose:
!    To test function ave_value by calling it with a user-defined 
!    function my_func.  
!
!  Record of revisions:
!      Date       Programmer          Description of change
!      ====       ==========          =====================
!    11/24/06    S. J. Chapman        Original code
!
IMPLICIT NONE

! Data dictionary: declare function types
REAL :: ave_value              ! Average value of function
REAL, EXTERNAL :: my_function  ! Function to evaluate

! Data dictionary: declare local variable types & definitions
REAL :: ave                    ! Average of my_function

! Call function with func=my_function.
ave = ave_value ( my_function, 0., 1., 101 )
WRITE (*,1000) 'my_function', ave
1000 FORMAT (1X,'The average value of ',A,' between 0. and 1. is ', &
             F16.6,'.')
 
END PROGRAM test_ave_value


REAL FUNCTION my_function( x )
IMPLICIT NONE
REAL, INTENT(IN) :: x
my_function = 3. * x
END FUNCTION my_function
