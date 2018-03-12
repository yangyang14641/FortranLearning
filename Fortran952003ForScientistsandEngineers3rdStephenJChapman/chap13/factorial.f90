RECURSIVE SUBROUTINE factorial ( n, result )
!
!  Purpose:
!    To calculate the factorial function
!             | n(n-1)!    n >= 1 
!       n ! = |
!             | 1          n = 0
!
!  Record of revisions:
!      Date       Programmer          Description of change
!      ====       ==========          =====================
!    12/07/06    S. J. Chapman        Original code
!
IMPLICIT NONE

! Data dictionary: declare calling parameter types & definitions
INTEGER, INTENT(IN) :: n         ! Value to calculate
INTEGER, INTENT(OUT) :: result   ! Result

! Data dictionary: declare local variable types & definitions
INTEGER :: temp                  ! Temporary variable

IF ( n >= 1 ) THEN
   CALL factorial ( n-1, temp )
   result = n * temp
ELSE
   result = 1
END IF

END SUBROUTINE factorial
