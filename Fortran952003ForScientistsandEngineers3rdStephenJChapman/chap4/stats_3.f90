PROGRAM stats_3
!
!  Purpose:
!    To calculate mean and the standard deviation of an input
!    data set, where each input value can be positive, negative,
!    or zero.
!
!  Record of revisions:
!      Date       Programmer          Description of change
!      ====       ==========          =====================
!    11/13/06    S. J. Chapman        Original code
!
IMPLICIT NONE

! Data dictionary: declare variable types, definitions, & units  
INTEGER :: i         ! Loop index
INTEGER :: n = 0     ! The number of input samples.
REAL :: std_dev      ! The standard deviation of the input samples.
REAL :: sum_x = 0.   ! The sum of the input values. 
REAL :: sum_x2 = 0.  ! The sum of the squares of the input values. 
REAL :: x = 0.       ! An input data value.
REAL :: x_bar        ! The average of the input samples.

! Get the number of points to input.
WRITE (*,*) 'Enter number of points: '
READ  (*,*) n

! Check to see if we have enough input data.
IF ( n < 2 ) THEN ! Insufficient data

   WRITE (*,*) 'At least 2 values must be entered.'

ELSE ! we will have enough data, so let's get it.
 
   ! Loop to read input values.
   DO i = 1, n

      ! Read values
      WRITE (*,*) 'Enter number: '
      READ  (*,*) x 
      WRITE (*,*) 'The number is ', x 

      ! Accumulate sums.
      sum_x  = sum_x + x
      sum_x2 = sum_x2 + x**2

   END DO

   ! Now calculate statistics.
   x_bar = sum_x / real(n)
   std_dev = SQRT((real(n)*sum_x2 - sum_x**2) / (real(n)*real(n-1)))

   ! Tell user.
   WRITE (*,*) 'The mean of this data set is:', x_bar
   WRITE (*,*) 'The standard deviation is:   ', std_dev
   WRITE (*,*) 'The number of data points is:', n

END IF

END PROGRAM stats_3
