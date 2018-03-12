PROGRAM test_extremes
!
!  Purpose:
!    To read in a real input data set, and use it to test subroutine
!    "extremes".  The optional arguments feature of the subroutine
!    will be tested by calling the subroutine 3 times with different
!    combinations of arguments.
!
!  Record of revisions:
!      Date       Programmer          Description of change
!      ====       ==========          =====================
!    12/31/95    S. J. Chapman        Original code
!
USE procs
IMPLICIT NONE

! List of parameters:
INTEGER, PARAMETER :: max_size = 1000

! List of variables:
REAL, DIMENSION(max_size) :: a   ! Data array to sort
LOGICAL :: exceed = .FALSE.      ! Logical indicating that array 
                                 ! limits are exceeded.
CHARACTER(len=20) :: filename    ! Input data file name
REAL :: large                    ! Largest value in a
INTEGER :: large_pos             ! Pos of largest value in a
INTEGER :: nvals = 0             ! Number of data values in a
REAL :: small                    ! Smallest value in a
INTEGER :: small_pos             ! Pos of smallest value in a
INTEGER :: status                ! I/O status: 0 for success
REAL :: temp                     ! Temporary variable

! Get the name of the file containing the input data.
WRITE (*,*) 'Enter the file name with input data set: '
READ (*,'(A20)') filename
 
! Open input data file.  Status is OLD because the input data must 
! already exist.
OPEN ( UNIT=2, FILE=filename, STATUS='OLD', IOSTAT=status )
 
! Was the OPEN successful? 
fileopen: IF ( status == 0 ) THEN       ! Open successful
 
   ! The file was opened successfully, so read the data to sort 
   ! from it, sort the data, and write out the results.
   ! First read in data.
   DO
      READ (2, *, IOSTAT=status) temp      ! Get value
      IF ( status /= 0 ) EXIT              ! Exit on end of data 
      nvals = nvals + 1                    ! Bump count
      size: IF ( nvals <= max_size ) THEN  ! Too many values?
         a(nvals) = temp                   ! No: Save value in array
      ELSE
         exceed = .TRUE.                   ! Yes: Array overflow
      END IF size
   END DO
 
   ! Was the array size exceeded?  If so, tell user and quit.
   toobig: IF ( exceed ) THEN
      WRITE (*,1000) nvals, max_size
      1000 FORMAT (' Maximum array size exceeded: ', I6, ' > ', I6 )
   ELSE

      ! Limit not exceeded.  Find extremes specifying all arguments
      ! in order.  Tell user.
      CALL extremes (a, nvals, large, large_pos, small, small_pos)
      WRITE (*,1020) 'All arguments in order:           ', &
                     large, large_pos, small, small_pos
      1020 FORMAT (1X,A,2(2X,F6.2,2X,I4))

      ! Find extremes specifying all arguments in arbitrary
      ! order.  Tell user.
      CALL extremes (a, nvals, MAXVAL=large, MINVAL=small, &
                     POS_MAXVAL=large_pos, POS_MINVAL=small_pos)
      WRITE (*,1020) 'All arguments in arbitrary order: ', &
                     large, large_pos, small, small_pos

      ! Find extremes specifying only max and min values.
      CALL extremes (a, nvals, MAXVAL=large, MINVAL=small)
      WRITE (*,1030) 'Large and small only:             ', &
                     large, small
      1030 FORMAT (1X,A,2(2X,F6.2,6X))

   END IF toobig

ELSE fileopen
   
   ! If we get here, the file open failed.  Tell user.
   WRITE (*,'(A,I6)') ' File open failed: status = ', status

END IF fileopen
 
END PROGRAM
