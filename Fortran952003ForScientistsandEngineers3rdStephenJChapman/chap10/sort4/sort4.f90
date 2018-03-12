PROGRAM sort4
!
!  Purpose:
!    To read in a character input data set, sort it into ascending 
!    order using the selection sort algorithm, and to write the  
!    sorted data to the standard output device.  This program calls 
!    subroutine "sortc" to do the actual sorting.
!
!  Record of revisions:
!      Date       Programmer          Description of change
!      ====       ==========          =====================
!    11/25/06    S. J. Chapman        Original code
!
IMPLICIT NONE

! Data dictionary: declare constants
INTEGER, PARAMETER :: MAX_SIZE = 10     ! Max number to sort

! Data dictionary: declare variable types & definitions
CHARACTER(len=20), DIMENSION(MAX_SIZE) :: a   
                                 ! Data array to sort
LOGICAL :: exceed = .FALSE.      ! Logical indicating that array 
                                 !   limits are exceeded.
CHARACTER(len=20) :: filename    ! Input data file name
INTEGER :: i                     ! Loop index
INTEGER :: nvals = 0             ! Number of data values to sort
INTEGER :: status                ! I/O status: 0 for success
CHARACTER(len=20) :: temp        ! Temporary variable for reading

! Get the name of the file containing the input data.
WRITE (*,*) 'Enter the file name with the data to be sorted: '
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
   ELSE

      ! Limit not exceeded: sort the data.
      CALL sortc (a, nvals)

      ! Now write out the sorted data.
      WRITE (*,*) 'The sorted output data values are: '
      WRITE (*,'(4X,A)') ( a(i), i = 1, nvals )

   END IF toobig

ELSE fileopen

   ! Else file open failed.  Tell user.
   WRITE (*,1020) status
   1020 FORMAT (1X,'File open failed--status = ', I6)

END IF fileopen
 
END PROGRAM sort4

SUBROUTINE sortc (array, n )
!
!  Purpose:
!    To sort a character array into ascending order using a 
!    selection sort.  
!
!  Record of revisions:
!      Date       Programmer          Description of change
!      ====       ==========          =====================
!    11/25/06    S. J. Chapman        Original code
!
IMPLICIT NONE

! Data dictionary: declare calling parameter types & definitions
INTEGER, INTENT(IN) :: n                  ! Number of values
CHARACTER(len=20), DIMENSION(n), INTENT(INOUT) :: array  
                                          ! Array to be sorted
! Data dictionary: declare local variable types & definitions
INTEGER :: i                  ! Loop index
INTEGER :: iptr               ! Pointer to smallest value
INTEGER :: j                  ! Loop index
CHARACTER(len=20) :: temp     ! Temp variable for swaps

! Sort the array
outer: DO i = 1, n-1
 
   ! Find the minimum value in array(i) through array(n)
   iptr = i
   inner: DO j = i+1, n
      minval: IF ( array(j) < array(iptr) ) THEN
         iptr = j
      END IF minval
   END DO inner
 
   ! iptr now points to the minimum value, so swap array(iptr)
   ! with array(i) if i /= iptr.
   swap: IF ( i /= iptr ) THEN
      temp        = array(i)
      array(i)    = array(iptr)
      array(iptr) = temp
   END IF swap
 
END DO outer
 
END SUBROUTINE sortc
