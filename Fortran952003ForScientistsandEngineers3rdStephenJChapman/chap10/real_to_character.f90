FUNCTION real_to_char ( value )
!
!  Purpose:
!    To convert a real value into a 12-character string, with the 
!    number printed in as readable a format as possible considering 
!    its range.  This routine prints out the number according to the
!    following rules:
!       1.  value > 9999999.                ES12.5
!       2.  value < -999999.                ES12.5
!       3.  0.    <  ABS(value) < 0.01      ES12.5
!       4.  value = 0.0                     F12.4
!       5.  Otherwise                       F12.4
!
!  Record of revisions:
!      Date       Programmer          Description of change
!      ====       ==========          =====================
!    11/25/06    S. J. Chapman        Original code
!
IMPLICIT NONE

! Data dictionary: declare calling parameter types & definitions
REAL, INTENT(IN) :: value           ! value to convert to char form
CHARACTER (len=12) :: real_to_char  ! Output character string

! Data dictionary: declare local variable types & definitions
CHARACTER(len=9) :: fmt             ! Format descriptor
CHARACTER(len=12) :: string         ! Output string
 
! Clear string before use
string = ' '

! Select proper format
IF ( value > 9999999. ) THEN
   fmt = '(ES12.5)'
ELSE IF ( value < -999999. ) THEN
   fmt = '(ES12.5)'
ELSE IF ( value == 0. ) THEN
   fmt = '(F12.4)'
ELSE IF ( ABS(value) < 0.01 ) THEN
   fmt = '(ES12.5)'
ELSE
   fmt = '(F12.4)'
END IF
 
! Convert to character form.
WRITE (string,fmt) value
real_to_char = string
 
END FUNCTION real_to_char
