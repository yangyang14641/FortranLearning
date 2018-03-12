MODULE ran001
!
!  Purpose:
!    To declare data shared between subs random0 and seed.
!
!  Record of revisions:
!      Date       Programmer          Description of change
!      ====       ==========          =====================
!    11/22/06    S. J. Chapman        Original code
!
IMPLICIT NONE
SAVE
INTEGER :: n = 9876
END MODULE ran001

!*****************************************************************
!*****************************************************************

SUBROUTINE random0 ( ran )
!
!  Purpose:
!    Subroutine to generate a pseudorandom number with a uniform 
!    distribution in the range 0. <= ran < 1.0.
!
!  Record of revisions:
!      Date       Programmer          Description of change
!      ====       ==========          =====================
!    11/22/06    S. J. Chapman        Original code
!
USE ran001                     ! Shared seed
IMPLICIT NONE

! Data dictionary: declare calling parameter types & definitions
REAL, INTENT(OUT) :: ran       ! Random number

! Calculate next number
n = MOD (8121 * n + 28411, 134456 )  

! Generate random value from this number
ran = REAL(n) / 134456.              
 
END SUBROUTINE random0

!*****************************************************************
!*****************************************************************

SUBROUTINE seed ( iseed )
!
!  Purpose:
!    To set the seed for random number generator random0.
!
!  Record of revisions:
!      Date       Programmer          Description of change
!      ====       ==========          =====================
!    11/22/06    S. J. Chapman        Original code
!
USE ran001                     ! Shared seed
IMPLICIT NONE

! Data dictionary: declare calling parameter types & definitions
INTEGER, INTENT(IN) :: iseed   ! Value to initialize sequence

! Set seed
n = ABS ( iseed )
 
END SUBROUTINE seed
