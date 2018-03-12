PROGRAM least_squares_fit
!
!  Purpose:
!    To perform a least-squares fit of an input data set
!    to a straight line, and print out the resulting slope
!    and intercept values.  The input data for this fit
!    comes from a user-specified input data file.
!
!  Record of revisions:
!      Date       Programmer          Description of change
!      ====       ==========          =====================
!    11/18/06    S. J. Chapman        Original code
!
IMPLICIT NONE

! Data dictionary: declare constants  
INTEGER, PARAMETER :: LU = 18 ! I/o unit for disk I/O

! Data dictionary: declare variable types, definitions, & units  
! Note that cumulative variables are all initialized to zero.
CHARACTER(len=24) :: filename ! Input file name (<= 24 chars)
INTEGER :: ierror             ! Status flag from I/O statements
INTEGER :: n = 0              ! Number of input data pairs (x,y)
REAL :: slope                 ! Slope of the line
REAL :: sum_x = 0.            ! Sum of all input X values
REAL :: sum_x2 = 0.           ! Sum of all input X values squared
REAL :: sum_xy = 0.           ! Sum of all input X*Y values
REAL :: sum_y = 0.            ! Sum of all input Y values
REAL :: x                     ! An input X value
REAL :: x_bar                 ! Average X value
REAL :: y                     ! An input Y value
REAL :: y_bar                 ! Average Y value
REAL :: y_int                 ! Y-axis intercept of the line

! Prompt user and get the name of the input file.
WRITE (*,1000)
1000 FORMAT (1X,'This program performs a least-squares fit of an ',/, &
             1X,'input data set to a straight line. Enter the name',/ &
             1X,'of the file containing the input (x,y) pairs:  ' )
READ (*,1010) filename
1010 FORMAT (A)

! Open the input file
OPEN (UNIT=LU, FILE=filename, STATUS='OLD', IOSTAT=ierror )

! Check to see of the OPEN failed.
errorcheck: IF ( ierror > 0 ) THEN 

   WRITE (*,1020) filename
   1020 FORMAT (1X,'ERROR: File ',A,' does not exist!')

ELSE
 
   ! File opened successfully. Read the (x,y) pairs from 
   ! the input file.
   DO 
      READ (LU,*,IOSTAT=ierror) x, y   ! Get pair
      IF ( ierror /= 0 ) EXIT
      n      = n + 1                   !
      sum_x  = sum_x + x               ! Calculate 
      sum_y  = sum_y + y               !   statistics
      sum_x2 = sum_x2 + x**2           !
      sum_xy = sum_xy + x * y          !
   END DO
 
   ! Now calculate the slope and intercept. 
   x_bar = sum_x / real(n)
   y_bar = sum_y / real(n)
   slope = (sum_xy - sum_x * y_bar) / ( sum_x2 - sum_x * x_bar)
   y_int = y_bar - slope * x_bar 
 
   ! Tell user.
   WRITE (*, 1030 ) slope, y_int, N
   1030 FORMAT ('0','Regression coefficients for the least-squares line:',&
          /,1X,'  slope (m)     = ', F12.3,&
          /,1X,'  Intercept (b) = ', F12.3,&
          /,1X,'  No of points  = ', I12 )
 
   ! Close input file, and quit.
   CLOSE (UNIT=LU)

END IF errorcheck

END PROGRAM least_squares_fit
