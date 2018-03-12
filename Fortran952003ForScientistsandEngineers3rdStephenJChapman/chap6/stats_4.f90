PROGRAM stats_4
!
!  Purpose:
!    To calculate mean, median, and standard deviation of an input
!    data set read from a file.
!
!  Record of revisions:
!      Date       Programmer          Description of change
!      ====       ==========          =====================
!    11/17/06    S. J. Chapman        Original code
!
IMPLICIT NONE

! Data dictionary: declare constants
INTEGER, PARAMETER :: MAX_SIZE = 100 ! Max data size

! Data dictionary: declare variable types & definitions
REAL, DIMENSION(MAX_SIZE) :: a   ! Data array to sort
LOGICAL :: exceed = .FALSE.      ! Logical indicating that array 
                                 ! limits are exceeded.
CHARACTER(len=20) :: filename    ! Input data file name
INTEGER :: i                     ! Loop index
INTEGER :: iptr                  ! Pointer to smallest value
INTEGER :: j                     ! Loop index
REAL :: median                   ! The median of the input samples
INTEGER :: nvals = 0             ! Number of data values to sort
INTEGER :: status                ! I/O status: 0 for success
REAL :: std_dev                  ! Standard deviation of input samples
REAL :: sum_x = 0.               ! Sum of input values
REAL :: sum_x2 = 0.              ! Sum of input values squared
REAL :: temp                     ! Temporary variable for swapping
REAL :: x_bar                    ! Average of input values

! Get the name of the file containing the input data.
WRITE (*,1000) 
1000 FORMAT (1X,'Enter the file name with the data to be sorted: ')
READ (*,'(A20)') filename
 
! Open input data file.  Status is OLD because the input data must 
! already exist.
OPEN ( UNIT=9, FILE=filename, STATUS='OLD', ACTION='READ', &
       IOSTAT=status )
 
! Was the OPEN successful? 
fileopen: IF ( status == 0 ) THEN          ! Open successful
 
   ! The file was opened successfully, so read the data to sort 
   ! from it, sort the data, and write out the results.
   ! First read in data.
   DO
      READ (9, *, IOSTAT=status) temp      ! Get value
      IF ( status /= 0 ) EXIT              ! Exit on end of data 
      nvals = nvals + 1                    ! Bump count
      size: IF ( nvals <= MAX_SIZE ) THEN  ! Too many values?
         a(nvals) = temp                   ! No: Save value in array
      ELSE
         exceed = .TRUE.                   ! Yes: Array overflow
      END IF size
   END DO
 
   ! Was the array size exceeded?  If so, tell user and quit.
   toobig: IF ( exceed ) THEN
      WRITE (*,1010) nvals, MAX_SIZE
      1010 FORMAT (' Maximum array size exceeded: ', I6, ' > ', I6 )
   ELSE

      ! Limit not exceeded: sort the data.
      outer: DO i = 1, nvals-1
 
         ! Find the minimum value in a(i) through a(nvals)
         iptr = i
         inner: DO j = i+1, nvals
            minval: IF ( a(j) < a(iptr) ) THEN
               iptr = j
            END IF minval
         END DO inner
 
         ! iptr now points to the minimum value, so swap A(iptr) 
         ! with a(i) if i /= iptr.
         swap: IF ( i /= iptr ) THEN
            temp    = a(i)
            a(i)    = a(iptr)
            a(iptr) = temp
         END IF swap
 
      END DO outer

      ! The data is now sorted.  Accumulate sums to calculate
      ! statistics.
      sums: DO i = 1, nvals
         sum_x  = sum_x + a(i)
         sum_x2 = sum_x2 + a(i)**2
      END DO sums
 
      ! Check to see if we have enough input data.
      enough: IF ( nvals < 2 ) THEN 
 
         ! Insufficient data.
         WRITE (*,*) ' At least 2 values must be entered.'
 
      ELSE 
 
         ! Calculate the mean, median, and standard deviation
         x_bar   = sum_x / real(nvals)
         std_dev = sqrt( (real(nvals) * sum_x2 - sum_x**2) &
                 / (real(nvals) * real(nvals-1)) )
         even: IF ( mod(nvals,2) == 0 ) THEN
            median = ( a(nvals/2) + a(nvals/2+1) ) / 2.
         ELSE
            median = a(nvals/2+1)
         END IF even
 
         ! Tell user.
         WRITE (*,*) 'The mean of this data set is:  ', x_bar
         WRITE (*,*) 'The median of this data set is:', median
         WRITE (*,*) 'The standard deviation is:     ', std_dev
         WRITE (*,*) 'The number of data points is:  ', nvals
 
      END IF enough

   END IF toobig

ELSE fileopen

   ! Else file open failed.  Tell user.
   WRITE (*,1050) status
   1050 FORMAT (1X,'File open failed--status = ', I6)

END IF fileopen
 
END PROGRAM stats_4
