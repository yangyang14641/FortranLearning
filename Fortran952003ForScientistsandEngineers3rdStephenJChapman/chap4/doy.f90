PROGRAM doy

!  Purpose:
!    This program calculates the day of year corresponding to a
!    specified date.  It illustrates the use of counting loops 
!    and the SELECT CASE construct. 
!
!  Record of revisions:
!      Date       Programmer          Description of change
!      ====       ==========          =====================
!    11/13/06    S. J. Chapman        Original code
!
IMPLICIT NONE

! Data dictionary: declare variable types, definitions, & units  
INTEGER :: day         ! Day (dd)
INTEGER :: day_of_year ! Day of year
INTEGER :: i           ! Index variable
INTEGER :: leap_day    ! Extra day for leap year
INTEGER :: month       ! Month (mm)
INTEGER :: year        ! Year (yyyy)

! Get day, month, and year to convert
WRITE (*,*) 'This program calculates the day of year given the '
WRITE (*,*) 'current date.  Enter current month (1-12), day(1-31),'
WRITE (*,*) 'and year in that order:  '
READ (*,*) month, day, year

! Check for leap year, and add extra day if necessary
IF ( MOD(year,400) == 0 ) THEN
   leap_day = 1           ! Years divisible by 400 are leap years
ELSE IF ( MOD(year,100) == 0 ) THEN
   leap_day = 0           ! Other centuries are not leap years
ELSE IF ( MOD(year,4) == 0 ) THEN
   leap_day = 1           ! Otherwise every 4th year is a leap year
ELSE
   leap_day = 0           ! Other years are not leap years
END IF

! Calculate day of year
day_of_year = day
DO i = 1, month-1

   ! Add days in months from January to last month
   SELECT CASE (i)
   CASE (1,3,5,7,8,10,12)
      day_of_year = day_of_year + 31
   CASE (4,6,9,11)
      day_of_year = day_of_year + 30
   CASE (2)
      day_of_year = day_of_year + 28 + leap_day
   END SELECT

END DO

! Tell user
WRITE (*,*) 'Day         = ', day
WRITE (*,*) 'Month       = ', month
WRITE (*,*) 'Year        = ', year
WRITE (*,*) 'day of year = ', day_of_year

END PROGRAM doy
