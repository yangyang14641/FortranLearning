PROGRAM test_running_average
!
!  Purpose:
!    To test running average subroutine. 
!
IMPLICIT NONE

! Declare variables:
INTEGER :: istat              ! I/O status
REAL :: ave                   ! Average
REAL :: std_dev               ! Standard deviation
INTEGER :: nvals              ! Number of values
REAL :: x                     ! Input data value
CHARACTER(len=20) :: file_name ! Input data file name

! Clear the running sums.
CALL running_average ( 0., ave, std_dev, nvals, .TRUE. )
 
! Get the name of the file containing the input data.
WRITE (*,*) ' Enter the file name containing the data: ' 
READ (*,'(A20)') file_name
 
! Open input data file.  Status is OLD because the input data must 
! already exist.
OPEN ( UNIT=21, FILE=file_name, STATUS='OLD', ACTION='READ', &
       IOSTAT=istat )
 
! Was the OPEN successful? 
openok: IF ( istat == 0 ) THEN
 
   ! The file was opened successfully, so read the data to calculate
   !  running averages for.
   calc: DO
      READ (21,*,IOSTAT=istat) x        ! Get next value
      IF ( istat /= 0 ) EXIT            ! EXIT if not valid.

      ! Get running average & standard deviation
      CALL running_average ( x, ave, std_dev, nvals, .FALSE. )

      ! Now write out the running statistics.
      WRITE (*,1020) 'Value = ', x, '  Ave = ', ave, &
                     '  Std_dev = ', std_dev, &
                     '  Nvals = ', nvals
      1020 FORMAT (1X,3(A,F10.4),A,I6) 
   END DO calc

ELSE openok

   ! Else file open failed.  Tell user.
   WRITE (*,1030) istat
   1030 FORMAT (1X,'File open failed--status = ', I6)

END IF openok

END PROGRAM test_running_average
