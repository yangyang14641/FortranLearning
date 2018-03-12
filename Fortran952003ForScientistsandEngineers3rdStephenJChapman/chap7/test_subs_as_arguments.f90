SUBROUTINE subs_as_arguments(x, y, sub, result )
!
!  Purpose:
!    To test passing subroutine names as arguments.
!
IMPLICIT NONE

! Data dictionary: declare calling parameter types & definitions
EXTERNAL :: sub                   ! Dummy subroutine name
REAL, INTENT(IN) :: x             ! First value 
REAL, INTENT(IN) :: y             ! Last value 
REAL, INTENT(OUT) :: result       ! Result

CALL sub(x, y, result)

END SUBROUTINE subs_as_arguments

!*****************************************************************
!*****************************************************************

PROGRAM test_subs_as_arguments
!
!  Purpose:
!    To test passing subroutine names as arguments.
!
IMPLICIT NONE

! Data dictionary: declare calling parameter types & definitions
EXTERNAL :: sum, prod             ! Name of subroutines to call
REAL :: x                         ! First value 
REAL :: y                         ! Last value 
REAL :: result                    ! Result


! Get the x and y values
WRITE (*,*) 'Enter x:'
READ (*,*) x
WRITE (*,*) 'Enter y:'
READ (*,*) y

! Calculate product 
CALL subs_as_arguments(x, y, prod, result)
WRITE (*,*) 'The product is ', result

! Calculate product and sum
CALL subs_as_arguments(x, y, sum, result)
WRITE (*,*) 'The sum is ', result

END PROGRAM test_subs_as_arguments

!*****************************************************************
!*****************************************************************

SUBROUTINE prod ( x, y, result )
!
!  Purpose:
!    To calculate product of two real numbers.
!
IMPLICIT NONE

! Data dictionary: declare calling parameter types & definitions
REAL, INTENT(IN) :: x             ! First value 
REAL, INTENT(IN) :: y             ! Last value 
REAL, INTENT(OUT) :: result       ! Result

! Calculate value.
result = x * y
 
END SUBROUTINE prod

!*****************************************************************
!*****************************************************************

SUBROUTINE sum ( x, y, result )
!
!  Purpose:
!    To calculate sum of two real numbers.
!
IMPLICIT NONE

! Data dictionary: declare calling parameter types & definitions
REAL, INTENT(IN) :: x             ! First value 
REAL, INTENT(IN) :: y             ! Last value 
REAL, INTENT(OUT) :: result       ! Result

! Calculate value.
result = x + y
 
END SUBROUTINE sum

