SUBROUTINE rmax ( a, n, real_max, imax )
!
!  Purpose:
!    To find the maximum value in an array, and the location
!    of that value in the array.
!
IMPLICIT NONE

! Data dictionary: declare calling parameter types & definitions
INTEGER, INTENT(IN) :: n             ! No. of vals in array a.
REAL, INTENT(IN), DIMENSION(n) :: a  ! Input data.
REAL, INTENT(OUT) :: real_max        ! Maximum value in a.
INTEGER, INTENT(OUT) :: imax         ! Location of max value.

! Data dictionary: declare local variable types & definitions
INTEGER :: i                         ! Index variable

! Initialize the maximum value to first value in array.
real_max = a(1)
imax = 1
 
! Find the maximum value.
DO i = 2, n
   IF ( a(i) > real_max ) THEN
      real_max = a(i)
      imax = i
   END IF
END DO

END SUBROUTINE rmax


SUBROUTINE rmin ( a, n, real_min, imin )
!
!  Purpose:
!    To find the minimum value in an array, and the location
!    of that value in the array.
!
IMPLICIT NONE

! Data dictionary: declare calling parameter types & definitions
INTEGER, INTENT(IN) :: n             ! No. of vals in array a.
REAL, INTENT(IN), DIMENSION(n) :: a  ! Input data.
REAL, INTENT(OUT) :: real_min        ! Minimum value in a.
INTEGER, INTENT(OUT) :: imin         ! Location of min value.

! Data dictionary: declare local variable types & definitions
INTEGER :: i                         ! Index variable

! Initialize the minimum value to first value in array.
real_min = a(1)
imin = 1
 
! Find the minimum value.
DO I = 2, n
   IF ( a(i) < real_min ) THEN
      real_min  = a(i)
      imin = i
   END IF
END DO

END SUBROUTINE rmin


SUBROUTINE ave_sd ( a, n, ave, std_dev, error )
!
!  Purpose:
!    To calculate the average and standard deviation of an array.
!
IMPLICIT NONE

! Data dictionary: declare calling parameter types & definitions
INTEGER, INTENT(IN) :: n             ! No. of vals in array a.
REAL, INTENT(IN), DIMENSION(n) :: a  ! Input data.
REAL, INTENT(OUT) :: ave             ! Average of a.
REAL, INTENT(OUT) :: std_dev         ! Standard deviation.
INTEGER, INTENT(OUT) :: error        ! Flag: 0 -- no error
                                     !       1 -- sd invalid
                                     !       2 -- ave & sd invalid

! Data dictionary: declare local variable types & definitions
INTEGER :: i                         ! Loop index
REAL :: sum_x                        ! Sum of input values
REAL :: sum_x2                       ! Sum of input values squared

! Initialize the sums to zero.
sum_x  = 0.
sum_x2 = 0.
 
! Accumulate sums.
DO I = 1, n
   sum_x  = sum_x + a(i)
   sum_x2 = sum_x2 + a(i)**2
END DO
 
! Check to see if we have enough input data.
IF ( n >= 2 ) THEN ! we have enough data
 
   ! Calculate the mean and standard deviation
   ave   = sum_x / REAL(n)
   std_dev = SQRT( (REAL(n) * sum_x2 - sum_x**2) &
           / (REAL(n) * REAL(n-1)) )
   error = 0
 
ELSE IF ( n == 1 ) THEN ! no valid std_dev
 
   ave   = sum_x               
   std_dev = 0.              ! std_dev invalid
   error = 1
 
ELSE
 
   ave = 0.                  ! ave invalid
   std_dev = 0.              ! std_dev invalid
   error = 2

END IF
END SUBROUTINE ave_sd


SUBROUTINE median ( a, n, med )
!
!  Purpose:
!    To calculate the median value of an array.
!
IMPLICIT NONE

! Data dictionary: declare calling parameter types & definitions
INTEGER, INTENT(IN) :: n             ! No. of vals in array a.
REAL, INTENT(IN), DIMENSION(n) :: a  ! Input data.
REAL, INTENT(OUT) :: med             ! Median value of a.

! Sort the data into ascending order.
CALL sort ( a, n )
 
! Get median.
IF ( MOD(n,2) == 0 ) THEN
   med = ( a(n/2) + a(n/2+1) ) / 2.
ELSE
   med = a(n/2+1)
END IF
END SUBROUTINE median
