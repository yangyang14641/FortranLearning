PROGRAM compare 
!
!  Purpose:
!    To compare two strings to see if they are equivalent,
!    ignoring case.
!
!  Record of revisions:
!      Date       Programmer          Description of change
!      ====       ==========          =====================
!    11/14/06    S. J. Chapman        Original code
!
IMPLICIT NONE

! Data dictionary: declare variable types, definitions, & units  
INTEGER :: i                 ! Loop index
CHARACTER(len=20) :: str1    ! First string to compare
CHARACTER(len=20) :: str1a   ! Copy of first string to compare
CHARACTER(len=20) :: str2    ! Second string to compare
CHARACTER(len=20) :: str2a   ! Copy of second string to compare
 
! Prompt for the strings
WRITE (*,*) 'Enter first string to compare:'
READ (*,*) str1 
WRITE (*,*) 'Enter second string to compare:'
READ (*,*) str2
 
! Make copies so that the original strings are not modified
str1a = str1
str2a = str2

! Now shift lower case letters to upper case.
DO i = 1, LEN(str1a)
   IF ( str1a(i:i) >= 'a' .AND. str1a(i:i) <= 'z' ) THEN
      str1a(i:i) = ACHAR ( IACHAR ( str1a(i:i) ) - 32 )
   END IF
END DO
DO i = 1, LEN(str2a)
   IF ( str2a(i:i) >= 'a' .AND. str2a(i:i) <= 'z' ) THEN
      str2a(i:i) = ACHAR ( IACHAR ( str2a(i:i) ) - 32 )
   END IF
END DO

! Compare strings and write result
IF ( str1a == str2a ) THEN
   WRITE (*,*) "'", str1, "' = '", str2, "' ignoring case."
ELSE
   WRITE (*,*) "'", str1, "' /= '", str2, "' ignoring case."
END IF

END PROGRAM compare
