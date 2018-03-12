PROGRAM stats_1
!
!  Purpose:
!    To calculate mean and the standard deviation of an input
!    data set containing an arbitrary number of input values.
!
!  Record of revisions:
!      Date       Programmer          Description of change
!      ====       ==========          =====================
!    11/10/06    S. J. Chapman        Original code
!
IMPLICIT NONE

! Data dictionary: declare variable types, definitions, & units  
INTEGER :: n = 0     ! The number of input samples.
REAL :: std_dev = 0. ! The standard deviation of the input samples.
REAL :: sum_x = 0.   ! The sum of the input values. 
REAL :: sum_x2 = 0.  ! The sum of the squares of the input values. 
REAL :: x = 0.       ! An input data value.
REAL :: x_bar        ! The average of the input samples.

! While Loop to read input values.
DO
   ! Read in next value
   WRITE (*,*) 'Enter number: '
   READ  (*,*) x 
   WRITE (*,*) 'The number is ', x 

   ! Test for loop exit
   IF ( x < 0 ) EXIT
 
   ! Otherwise, accumulate sums.
   n      = n + 1
   sum_x  = sum_x + x
   sum_x2 = sum_x2 + x**2
END DO

! Calculate the mean and standard deviation
x_bar = sum_x / real(n)
std_dev = SQRT( (real(n) * sum_x2 - sum_x**2) / (real(n) * real(n-1)) )

! Tell user.
WRITE (*,*) 'The mean of this data set is:', x_bar
WRITE (*,*) 'The standard deviation is:   ', std_dev
WRITE (*,*) 'The number of data points is:', n

END PROGRAM stats_1
