PROGRAM test_real_to_char
!
!  Purpose:
!    To test function real_to_char.  
!
!  Record of revisions:
!      Date       Programmer          Description of change
!      ====       ==========          =====================
!    11/25/06    S. J. Chapman        Original code
!
!  External routines:
!     real_to_char -- Convert real to character string
!     ucase        -- Shift string to upper case 
!
IMPLICIT NONE

! Declare external functions:
CHARACTER(len=12), EXTERNAL :: real_to_char

! Data dictionary: declare variable types & definitions
CHARACTER :: ch             ! Character to hold Y/N response.
CHARACTER(len=12) :: result ! Character output
REAL :: value               ! Value to be converted

while_loop: DO      
 
   ! Prompt for input value.
   WRITE (*,'(1X,A)') 'Enter value to convert:'
   READ (*,*) value
 
   ! Write converted value, and see if we want another.
   result = real_to_char(value)
   WRITE (*,'(1X,A,A,A)') 'The result is ', result, &
                          ': Convert another one? (Y/N) [N]'
   ! Get answer.
   READ (*,'(A)') ch
 
   ! Convert answer to upper case to make match.
   CALL ucase ( ch )
 
   ! Do another?
     IF ( ch /= 'Y' ) EXIT

END DO while_loop

END PROGRAM test_real_to_char
