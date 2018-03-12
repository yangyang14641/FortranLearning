PROGRAM test_varying_string
!
!  Purpose:
!    To test the VARYING_STRING data type defined in ISO/IEC 
!    auxiliary standard ISO/IEC 1539-2 : 1994. 
!
!  Record of revisions:
!      Date       Programmer          Description of change
!      ====       ==========          =====================
!    05/11/96    S. J. Chapman        Original code
!
USE iso_varying_string
IMPLICIT NONE

! List of variables:
CHARACTER(len=20) :: string1         ! Character variable
CHARACTER(len=20) :: string2         ! Character variable
CHARACTER(len=40) :: string3         ! Character variable
TYPE (varying_string) :: string4     ! Varying character variable
TYPE (varying_string) :: string5     ! Varying character variable
TYPE (varying_string) :: string6     ! Varying character variable

! Set input strings
string1 = 'ABC'
string2 = 'Test 2'
string3 = string1 // string2
string4 = 'ABC'
string5 = 'Test 2'
string6 = string5 // string6

! Write out variables:
WRITE (*,*) 'string1 = ', string1
WRITE (*,*) 'string2 = ', string2
WRITE (*,*) 'string3 = ', string3
WRITE (*,*) 'string4 = ', CHAR(string4)
WRITE (*,*) 'string5 = ', CHAR(string5)
WRITE (*,*) 'string6 = ', CHAR(string6)

! Now explore properties of these varibles:
WRITE(*,*) 'The length & trimmed length of string1 is: ', &
           LEN(string1), LEN_TRIM(string1)
WRITE(*,*) 'The length & trimmed length of string1 is: ', &
           LEN(string2), LEN_TRIM(string2)
WRITE(*,*) 'The length & trimmed length of string1 is: ', &
           LEN(string3), LEN_TRIM(string3)
WRITE(*,*) 'The length & trimmed length of string1 is: ', &
           LEN(string4), LEN_TRIM(string4)
WRITE(*,*) 'The length & trimmed length of string1 is: ', &
           LEN(string5), LEN_TRIM(string5)
WRITE(*,*) 'The length & trimmed length of string1 is: ', &
           LEN(string6), LEN_TRIM(string6)

END PROGRAM test_varying_string
