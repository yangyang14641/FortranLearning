SUBROUTINE calc_hypotenuse ( side_1, side_2, hypotenuse )
!
!  Purpose:
!    To calculate the hypotenuse of a right triangle from the two 
!    other sides.
!
!  Record of revisions:
!      Date       Programmer          Description of change
!      ====       ==========          =====================
!    11/18/06    S. J. Chapman        Original code
!
IMPLICIT NONE

! Data dictionary: declare calling parameter types & definitions
REAL, INTENT(IN) :: side_1        ! Length of side 1 
REAL, INTENT(IN) :: side_2        ! Length of side 2 
REAL, INTENT(OUT) :: hypotenuse   ! Length of hypotenuse

! Data dictionary: declare local variable types & definitions
REAL :: temp                      ! Temporary variable

! Calculate hypotenuse
temp = side_1**2 + side_2**2
hypotenuse = SQRT ( temp )

END SUBROUTINE calc_hypotenuse
