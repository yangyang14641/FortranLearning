MODULE date_class
!
!   This module implements a date class, which stores
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
   
CONTAINS

   ! Bound procedures
   PROCEDURE,PUBLIC :: set_date => set_date_sub
   PROCEDURE,PUBLIC :: get_day => get_day_fn
   PROCEDURE,PUBLIC :: get_month => get_month_fn
   PROCEDURE,PUBLIC :: get_year => get_year_fn
   PROCEDURE,PUBLIC :: is_leap_year => is_leap_year_fn
   PROCEDURE,PUBLIC :: is_equal => is_equal_fn
   PROCEDURE,PUBLIC :: is_earlier_than => is_earlier_fn
   PROCEDURE,PUBLIC :: is_later_than => is_later_fn
   PROCEDURE,PUBLIC :: to_string => to_string_fn
   
END TYPE date

! Restrict access to the actual procedure names
PRIVATE :: set_date_sub, get_day_fn, get_month_fn, get_year_fn
PRIVATE :: is_leap_year_fn, is_equal_fn, is_earlier_fn
PRIVATE :: is_later_fn, to_string_fn

! Now add methods
CONTAINS

   SUBROUTINE set_date_sub(this, day, month, year)
   ! 
   ! Subroutine to set the initial date
   !
   IMPLICIT NONE
   
   ! Declare calling arguments
   CLASS(date) :: this               ! Date object
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
   CLASS(date) :: this               ! Date object
   
   ! Get day
   get_day_fn = this%day
                   
   END FUNCTION get_day_fn
   
   
   INTEGER FUNCTION get_month_fn(this)
   ! 
   ! Function to return the month from this object
   !
   IMPLICIT NONE
   
   ! Declare calling arguments
   CLASS(date) :: this               ! Date object
   
   ! Get month
   get_month_fn = this%month
                   
   END FUNCTION get_month_fn
   
   
   INTEGER FUNCTION get_year_fn(this)
   ! 
   ! Function to return the year from this object
   !
   IMPLICIT NONE
   
   ! Declare calling arguments
   CLASS(date) :: this               ! Date object
   
   ! Get year
   get_year_fn = this%year
                   
   END FUNCTION get_year_fn
   

   LOGICAL FUNCTION is_leap_year_fn(this)
   ! 
   ! Is this year a leap year?
   !
   IMPLICIT NONE
   
   ! Declare calling arguments
   CLASS(date) :: this               ! Date object
   
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
   CLASS(date) :: this               ! Date object
   CLASS(date) :: that               ! Another date for comparison
   
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
   CLASS(date) :: this               ! Date object
   CLASS(date) :: that               ! Another date for comparison
   
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
   CLASS(date) :: this               ! Date object
   CLASS(date) :: that               ! Another date for comparison
   
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
   CLASS(date) :: this               ! Date object
   
   ! Declare local variables
   CHARACTER(len=2) :: dd            ! Day
   CHARACTER(len=2) :: mm            ! Month
   CHARACTER(len=4) :: yy            ! Year

   ! Get components
   WRITE (dd,'(I2.2)') this%day
   WRITE (mm,'(I2.2)') this%month
   WRITE (yy,'(I4)') this%year
   
   ! Return string
   to_string_fn = mm // '/' // dd // '/' // yy
   
   END FUNCTION to_string_fn

END MODULE date_class
