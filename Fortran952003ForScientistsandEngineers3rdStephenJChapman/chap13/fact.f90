RECURSIVE FUNCTION fact(n) RESULT(answer)
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
INTEGER :: answer                ! Result variable

IF ( n >= 1 ) THEN
   answer = n * fact(n-1)
ELSE
   answer = 1
END IF

END FUNCTION fact
