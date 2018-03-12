FUNCTION sinc ( x )
!
!  Purpose:
!    To calculate the sinc function
!       sinc(x) = sin(x) / x
!
!  Record of revisions:
!      Date       Programmer          Description of change
!      ====       ==========          =====================
!    11/23/06    S. J. Chapman        Original code
!
IMPLICIT NONE

! Data dictionary: declare calling parameter types & definitions
REAL, INTENT(IN) :: x        ! Value for which to evaluate sinc
REAL :: sinc                 ! Output value sinc(x)

! Data dictionary: declare local constants
REAL, PARAMETER :: EPSILON = 1.0E-30  ! the smallest value for which 
                                      ! to calculate SIN(x)/x

! Check to see of ABS(x) > EPSILON.
IF ( ABS(x) > EPSILON ) THEN
   sinc = SIN(x) / x
ELSE
   sinc = 1.
END IF

END FUNCTION sinc
