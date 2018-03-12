MODULE date_class
!
!  This module implements a date class, which stores
!   and manipulates dates on the Gregorian calendar.  
!   It implements set methods, get methods, predicate 
!   methods, and a "to_string" method for displays.
!
!  Record of revisions:
!      Date       Programmer          Description of change
!      ====       ==========          =====================
!    12/28/06    S. J. Chapman        Original code
!
IMPLICIT NONE

! Type definition
TYPE,PUBLIC :: date   ! This will be the name we instantiate

   ! Instance variables.  Note that the default
   ! date is January 1, 1900.
   PRIVATE
   INTEGER :: year = 1900   ! Year (0 - xxxx)
   INTEGER :: month = 1     ! Month (1-12)
   INTEGER :: day = 1       ! Day (1-31)
   
!  CONTAINS

!     ! Bound procedures
!     PUBLIC
!     PROCEDURE :: set_date ==> set_date_sub
!     PROCEDURE :: get_day ==> get_day_fn
!     PROCEDURE :: get_month ==> get_month_fn
!     PROCEDURE :: get_year ==> get_year_fn
!     PROCEDURE :: is_leap_year ==> is_leap_year_fn
!     PROCEDURE :: is_equal ==> is_equal_fn
!     PROCEDURE :: is_earlier ==> is_earlier_fn
!     PROCEDURE :: is_later ==> is_later_fn
!     PROCEDURE :: to_string ==> to_string_fn

END TYPE date

! Restrict access to the actual procedure names
!PRIVATE :: set_date_sub, get_day_fn, get_month_fn, get_year_fn
!PRIVATE :: is_leap_year_fn, is_equal_fn, is_earlier_fn
!PRIVATE :: is_later_fn, to_string_fn

! Now add methods
CONTAINS

   SUBROUTINE set_date_sub(this, day, month, year)
   ! 
   ! Subroutine to set the initial date
   !
   IMPLICIT NONE
   
   ! Declare calling arguments
   TYPE(date) :: this               ! Date object
   INTEGER,INTENT(IN) :: day         ! Day (1-31)
   INTEGER,INTENT(IN) :: month       ! Month (1-12)
   INTEGER,INTENT(IN) :: year        ! Year (0 - xxxx)

   ! Save date
   this%day   = day
   this%month = month
   this%year  = year
                   
   END SUBROUTINE set_date_sub


   INTEGER FUNCTION get_day_fn(this)
   ! 
   ! Function to return the day from this object
   !
   IMPLICIT NONE
   
   ! Declare calling arguments
   TYPE(date) :: this               ! Date object
   
   ! Get day
   get_day_fn = this%day
                   
   END FUNCTION get_day_fn
   
   
   INTEGER FUNCTION get_month_fn(this)
   ! 
   ! Function to return the month from this object
   !
   IMPLICIT NONE
   
   ! Declare calling arguments
   TYPE(date) :: this               ! Date object
   
   ! Get month
   get_month_fn = this%month
                   
   END FUNCTION get_month_fn
   
   
   INTEGER FUNCTION get_year_fn(this)
   ! 
   ! Function to return the year from this object
   !
   IMPLICIT NONE
   
   ! Declare calling arguments
   TYPE(date) :: this               ! Date object
   
   ! Get year
   get_year_fn = this%year
                   
   END FUNCTION get_year_fn
   

   LOGICAL FUNCTION is_leap_year_fn(this)
   ! 
   ! Is this year a leap year?
   !
   IMPLICIT NONE
   
   ! Declare calling arguments
   TYPE(date) :: this               ! Date object
   
   ! Perform calculation
   IF ( MOD(this%year, 400) == 0 ) THEN
      is_leap_year_fn = .TRUE.
   ELSE IF ( MOD(this%year, 100) == 0 ) THEN
      is_leap_year_fn = .FALSE.
   ELSE IF ( MOD(this%year, 4) == 0 ) THEN
      is_leap_year_fn = .TRUE.
   ELSE 
      is_leap_year_fn = .FALSE.
   END IF
                   
   END FUNCTION is_leap_year_fn
   

   LOGICAL FUNCTION is_equal_fn(this,that)
   ! 
   ! Are these two dates equal?
   !
   IMPLICIT NONE
   
   ! Declare calling arguments
   TYPE(date) :: this               ! Date object
   TYPE(date) :: that               ! Another date for comparison
   
   ! Perform calculation
   IF ( (this%year == that%year) .AND. &
        (this%month == that%month) .AND. &
        (this%day == that%day) ) THEN
      is_equal_fn = .TRUE.
   ELSE 
      is_equal_fn = .FALSE.
   END IF
                   
   END FUNCTION is_equal_fn
   

   LOGICAL FUNCTION is_earlier_fn(this,that)
   ! 
   ! Is the date in "that" earlier than the date
   ! stored in the object?
   !
   IMPLICIT NONE
   
   ! Declare calling arguments
   TYPE(date) :: this               ! Date object
   TYPE(date) :: that               ! Another date for comparison
   
   ! Perform calculation
   IF ( that%year > this%year ) THEN
      is_earlier_fn = .FALSE.
   ELSE IF ( that%year < this%year ) THEN
      is_earlier_fn = .TRUE.
   ELSE
      IF ( that%month > this%month) THEN
         is_earlier_fn = .FALSE.
      ELSE IF ( that%month < this%month ) THEN
         is_earlier_fn = .TRUE.
      ELSE
         IF ( that%day >= this%day ) THEN
            is_earlier_fn = .FALSE.
         ELSE 
            is_earlier_fn = .TRUE.
         END IF
      END IF
   END IF
                   
   END FUNCTION is_earlier_fn
   

   LOGICAL FUNCTION is_later_fn(this,that)
   ! 
   ! Is the date in "that" later than the date
   ! stored in the object?
   !
   IMPLICIT NONE
   
   ! Declare calling arguments
   TYPE(date) :: this               ! Date object
   TYPE(date) :: that               ! Another date for comparison
   
   ! Perform calculation
   IF ( that%year > this%year ) THEN
      is_later_fn = .TRUE.
   ELSE IF ( that%year < this%year ) THEN
      is_later_fn = .FALSE.
   ELSE
      IF ( that%month > this%month ) THEN
         is_later_fn = .TRUE.
      ELSE IF ( that%month < this%month ) THEN
         is_later_fn = .FALSE.
      ELSE
         IF ( that%day > this%day ) THEN
            is_later_fn = .TRUE.
         ELSE 
            is_later_fn = .FALSE.
         END IF
      END IF
   END IF
                   
   END FUNCTION is_later_fn
      

   CHARACTER(len=10) FUNCTION to_string_fn(this)
   ! 
   ! Represent the date as a string: MM/DD/YYYY.
   !
   IMPLICIT NONE
   
   ! Declare calling arguments
   TYPE(date) :: this               ! Date object
   
   ! Declare local variables
   CHARACTER(len=2) :: dd            ! Day
   CHARACTER(len=2) :: mm            ! Month
   CHARACTER(len=4) :: yy            ! Year

   ! Get components
   WRITE (dd,'(I2)') this%day
   WRITE (mm,'(I2)') this%month
   WRITE (yy,'(I4)') this%year
   
   ! Return string
   to_string_fn = mm // '/' // dd // '/' // yy
   
   END FUNCTION to_string_fn

END MODULE date_class


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

! Initialize dates d1, d2, and d3 (d4 defaults)
CALL set_date_sub(d1,4,1,1996)
CALL set_date_sub(d2,1,3,1998)
CALL set_date_sub(d3,3,1,1996)

! Write out the dates
WRITE (*,'(A,A)') 'Date 1 = ', to_string_fn(d1)
WRITE (*,'(A,A)') 'Date 2 = ', to_string_fn(d2)
WRITE (*,'(A,A)') 'Date 3 = ', to_string_fn(d3)
WRITE (*,'(A,A)') 'Date 4 = ', to_string_fn(d4)

! Check for leap years
IF ( is_leap_year_fn(d1) ) THEN
   WRITE (*,'(I4,A)') get_year_fn(d1), ' is a leap year.'
ELSE
   WRITE (*,'(I4,A)') get_year_fn(d1), ' is a not leap year.'
END IF

IF ( is_leap_year_fn(d2) ) THEN
   WRITE (*,'(I4,A)') get_year_fn(d2), ' is a leap year.'
ELSE
   WRITE (*,'(I4,A)') get_year_fn(d2), ' is a not leap year.'
END IF

! Check for equality
IF ( is_equal_fn(d1,d3) ) THEN
   WRITE (*,'(3A)') to_string_fn(d3), ' is equal to ', to_string_fn(d1)
ELSE
   WRITE (*,'(3A)') to_string_fn(d3), ' is not equal to ', to_string_fn(d1)
END IF

! Check is_earlier
IF ( is_earlier_fn(d1,d3) ) THEN
   WRITE (*,'(3A)') to_string_fn(d3), ' is earlier than ', to_string_fn(d1)
ELSE
   WRITE (*,'(3A)') to_string_fn(d3), ' is not earlier than ', to_string_fn(d1)
END IF

! Check is_later
IF ( is_later_fn(d1,d3) ) THEN
   WRITE (*,'(3A)') to_string_fn(d3), ' is later than ', to_string_fn(d1)
ELSE
   WRITE (*,'(3A)') to_string_fn(d3), ' is not later than ', to_string_fn(d1)
END IF
   
END PROGRAM test_date
