PROGRAM test_date
!
!  This program tests the date class.
!
!  Record of revisions:
!      Date       Programmer          Description of change
!      ====       ==========          =====================
!    12/28/06    S. J. Chapman        Original code
!
USE date_class                  ! Import date class
IMPLICIT NONE

! Declare local variables
TYPE(date) :: d1               ! Date 1
TYPE(date) :: d2               ! Date 2
TYPE(date) :: d3               ! Date 3
TYPE(date) :: d4               ! Date 4
CHARACTER(len=10) :: str1      ! Date strings
CHARACTER(len=10) :: str2      ! Date strings
CHARACTER(len=10) :: str3      ! Date strings
CHARACTER(len=10) :: str4      ! Date strings

! Initialize dates d1, d2, and d3 (d4 defaults)
CALL d1%set_date(4,1,1996)
CALL d2%set_date(1,3,1998)
CALL d3%set_date(3,1,1996)

! Write out the dates
str1 = d1%to_string()
str2 = d2%to_string()
str3 = d3%to_string()
str4 = d4%to_string()
WRITE (*,'(A,A)') 'Date 1 = ', str1
WRITE (*,'(A,A)') 'Date 2 = ', str2
WRITE (*,'(A,A)') 'Date 3 = ', str3
WRITE (*,'(A,A)') 'Date 4 = ', str4

! Check for leap years
IF ( d1%is_leap_year() ) THEN
   WRITE (*,'(I4,A)') d1%get_year(), ' is a leap year.'
ELSE
   WRITE (*,'(I4,A)') d1%get_year(), ' is a not leap year.'
END IF

IF ( d2%is_leap_year() ) THEN
   WRITE (*,'(I4,A)') d2%get_year(), ' is a leap year.'
ELSE
   WRITE (*,'(I4,A)') d2%get_year(), ' is a not leap year.'
END IF

! Check for equality
IF ( d1%is_equal(d3) ) THEN
   WRITE (*,'(3A)') str3, ' is equal to ', str1
ELSE
   WRITE (*,'(3A)') str3, ' is not equal to ', str1
END IF

! Check is_earlier
IF ( d1%is_earlier_than(d3) ) THEN
   WRITE (*,'(3A)') str3, ' is earlier than ', str1
ELSE
   WRITE (*,'(3A)') str3, ' is not earlier than ', str1
END IF

! Check is_later
IF ( d1%is_later_than(d3) ) THEN
   WRITE (*,'(3A)') str3, ' is later than ', str1
ELSE
   WRITE (*,'(3A)') str3, ' is not later than ', str1
END IF
   
END PROGRAM test_date
