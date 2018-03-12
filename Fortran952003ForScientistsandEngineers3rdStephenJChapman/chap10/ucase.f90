SUBROUTINE ucase ( string )
! 
!  Purpose: 
!    To shift a character string to upper case on any processor,
!    regardless of collating sequence.
! 
!  Record of revisions:
!      Date       Programmer          Description of change
!      ====       ==========          =====================
!    11/25/06    S. J. Chapman        Original code
!
IMPLICIT NONE

! Declare calling parameters:
CHARACTER(len=*), INTENT(INOUT) :: string  

! Declare local variables:
INTEGER :: i                 ! Loop index
INTEGER :: length            ! Length of input string
 
! Get length of string
length = LEN ( string )
 
! Now shift lower case letters to upper case.
DO i = 1, length
   IF ( LGE(string(i:i),'a') .AND. LLE(string(i:i),'z') ) THEN
      string(i:i) = ACHAR ( IACHAR ( string(i:i) ) - 32 )
   END IF
END DO

END SUBROUTINE ucase
