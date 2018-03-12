PROGRAM weekday_weekend
!
!  Purpose:
!    This program accepts a character string containing a
!    day of the week, and responds with a message specifying
!    whether the day is a weekday or falls on the weekend.
!
!  Record of revisions:
!      Date       Programmer          Description of change
!      ====       ==========          =====================
!    11/06/06    S. J. Chapman        Original code
!
IMPLICIT NONE

! Declare the variables used in this program.
CHARACTER(len=11) :: c_day  ! Character string containing day
CHARACTER(len=11) :: c_type ! Character string with day type
 
! Prompt the user for the day of the week
WRITE (*,*) 'Enter the name of the day: '
READ  (*,*) c_day
 
! Get the corresponding day of the week.
SELECT CASE (c_day) 
CASE ('Monday','Tuesday','Wednesday','Thursday','Friday')
   c_type = 'Weekday'
CASE ('Saturday','Sunday')
   c_type = 'Weekend'
CASE DEFAULT
   c_type = 'Invalid day'
END SELECT

! Write the resulting day type
WRITE (*,*) 'Day Type = ', c_type
 
END PROGRAM weekday_weekend
