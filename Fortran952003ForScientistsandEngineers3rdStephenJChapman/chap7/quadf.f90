REAL FUNCTION quadf ( x, a, b, c )
!
!  Purpose:
!    To evaluate a quadratic polynomial of the form
!       quadf = a * x**2 + b * x + c
!
!  Record of revisions:
!      Date       Programmer          Description of change
!      ====       ==========          =====================
!    11/22/06    S. J. Chapman        Original code
!
IMPLICIT NONE

! Data dictionary: declare calling parameter types & definitions
REAL, INTENT(IN) :: x       ! Value to evaluate expression for
REAL, INTENT(IN) :: a       ! Coefficient of X**2 term
REAL, INTENT(IN) :: b       ! Coefficient of X term
REAL, INTENT(IN) :: c       ! Coefficient of constant term

! Evaluate expression.
quadf = a * x**2 + b * x + c

END FUNCTION quadf
