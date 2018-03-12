SUBROUTINE running_average ( x, ave, std_dev, nvals, reset )
!
!  Purpose:
!    To calculate the running average, standard deviation,
!    and number of data points as data values x are received. 
!    If "reset" is .TRUE., clear running sums and exit. 
!
!  Record of revisions:
!      Date       Programmer          Description of change
!      ====       ==========          =====================
!    06/26/02    S. J. Chapman        Original code
!
IMPLICIT NONE

! Data dictionary: declare calling parameter types & definitions
REAL, INTENT(IN) :: x           ! Input data value.
REAL, INTENT(OUT) :: ave        ! Running average.
REAL, INTENT(OUT) :: std_dev    ! Running standard deviation.
INTEGER, INTENT(OUT) :: nvals   ! Current number of points.
LOGICAL, INTENT(IN) :: reset    ! Reset flag: clear sums if true

! Data dictionary: declare local variable types & definitions
INTEGER, SAVE :: n              ! Number of input values.
REAL, SAVE :: sum_x             ! Sum of input values.
REAL, SAVE :: sum_x2            ! Sum of input values squared.

! If the reset flag is set, clear the running sums at this time.
calc_sums: IF ( reset ) THEN

   n       = 0 
   sum_x   = 0.
   sum_x2  = 0.
   ave     = 0.
   std_dev = 0.
   nvals   = 0
   
ELSE
 
   ! Accumulate sums.
   n      = n + 1
   sum_x  = sum_x + x
   sum_x2 = sum_x2 + x**2
 
   ! Calculate average.
   ave = sum_x / REAL(n)

   ! Calculate standard deviation.
   IF ( n >= 2 ) THEN 
      std_dev = SQRT( (REAL(n) * sum_x2 - sum_x**2) &
              / (REAL(n) * REAL(n-1)) )
   ELSE
      std_dev = 0.
   END IF

   ! Number of data points.
   nvals = n

END IF calc_sums
 
END SUBROUTINE running_average
