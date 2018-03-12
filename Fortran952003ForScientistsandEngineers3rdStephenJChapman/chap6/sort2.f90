PROGRAM sort2
!
!  Purpose:
!    To read in a real input data set, sort it into ascending order
!    using the selection sort algorithm, and to write the sorted 
!    data to the standard output device.
!
!  Record of revisions:
!      Date       Programmer          Description of change
!      ====       ==========          =====================
!    11/15/06    S. J. Chapman        Original code
! 1. 11/16/06    S. J. Chapman        Modified to protect against array
!                                     overflow.
!
IMPLICIT NONE

! Data dictionary: declare constants
INTEGER, PARAMETER :: MAX_SIZE = 10  ! Maximum input data set size

! Data dictionary: declare variable types & definitions
REAL, DIMENSION(MAX_SIZE) :: a   ! Data array to sort
LOGICAL :: exceed = .FALSE.      ! Logical indicating that array 
                                 ! limits are exceeded.
CHARACTER(len=20) :: filename    ! Input data file name
INTEGER :: i                     ! Loop index
INTEGER :: iptr                  ! Pointer to smallest value
INTEGER :: j                     ! Loop index
INTEGER :: nvals = 0             ! Number of data values to sort
INTEGER :: status                ! I/O status: 0 for success
REAL :: temp                     ! Temporary variable for swapping

! Get the name of the file containing the input data.
WRITE (*,1000) 
1000 FORMAT (1X,'Enter the file name with the data to be sorted: ')
READ (*,'(A20)') filename
 
! Open input data file.  Status is OLD because the input data must 
! already exist.
OPEN ( UNIT=9, FILE=filename, STATUS='OLD', ACTION='READ', &
       IOSTAT=status )
 
! Was the OPEN successful? 
fileopen: IF ( status == 0 ) THEN       ! Open successful
 
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
   ELSE toobig

      ! Limit not exceeded: sort the data.
      outer: DO i = 1, nvals-1
 
         ! Find the minimum value in a(i) through a(nvals)
         iptr = i
         inner: DO j = i+1, nvals
            minval: IF ( a(j) < a(iptr) ) THEN
               iptr = j
            END IF minval
         END DO inner
 
         ! iptr now points to the minimum value, so swap a(iptr) with
         ! a(i) if i /= iptr.
         swap: IF ( i /= iptr ) THEN
            temp    = a(i)
            a(i)    = a(iptr)
            a(iptr) = temp
         END IF swap
 
      END DO outer
 
      ! Now write out the sorted data.
      WRITE (*,'(A)') ' The sorted output data values are: '
      WRITE (*,'(4X,F10.4)') ( a(i), i = 1, nvals )

   END IF toobig

ELSE fileopen

   ! Else file open failed.  Tell user.
   WRITE (*,1050) status
   1050 FORMAT (1X,'File open failed--status = ', I6)

END IF fileopen
 
END PROGRAM sort2
