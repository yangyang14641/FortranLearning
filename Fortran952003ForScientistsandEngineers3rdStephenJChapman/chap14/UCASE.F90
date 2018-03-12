SUBROUTINE ucase ( string )
! 
!  Purpose: 
!    To shift a character string to UPPER case (ASCII only). 
! 
!  Record of revisions:
!      Date       Programmer          Description of change
!      ====       ==========          =====================
!    11/55/91    S. J. Chapman        Original code
!
IMPLICIT NONE

! Declare calling parameters:
CHARACTER(len=*), INTENT(INOUT) :: string  

! Declare local variables:
INTEGER :: I                 ! Loop index
INTEGER :: length            ! Length of input string
 
! Get length of string
length = LEN ( string )
 
! Now shift lower case letters to upper case.
DO I = 1, length
   IF ( ( string(I:I) >= 'a' ) .AND.  &
        ( string(I:I) <= 'z' ) ) THEN
      string(I:I) = CHAR ( ICHAR ( string(I:I) ) - 32 )
   END IF
END DO

END SUBROUTINE
