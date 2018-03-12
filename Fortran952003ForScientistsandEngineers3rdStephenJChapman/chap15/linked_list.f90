PROGRAM linked_list
!
!  Purpose:
!    To read in a series of real values from an input data file
!    and store them in a linked list.  After the list is read in
!    it will be written back to the standard output device.
!
!  Record of revisions:
!      Date       Programmer          Description of change
!      ====       ==========          =====================
!    12/23/06    S. J. Chapman        Original code
!
IMPLICIT NONE

! Derived data type to store real values in
TYPE :: real_value
   REAL :: value
   TYPE (real_value), POINTER :: p
END TYPE

! Data dictionary: declare variable types & definitions
TYPE (real_value), POINTER :: head  ! Pointer to head of list
CHARACTER(len=20) :: filename       ! Input data file name
INTEGER :: nvals = 0                ! Number of data read
TYPE (real_value), POINTER :: ptr   ! Temporary pointer
TYPE (real_value), POINTER :: tail  ! Pointer to tail of list
INTEGER :: istat                    ! Status: 0 for success
REAL :: temp                        ! Temporary variable 

! Get the name of the file containing the input data.
WRITE (*,*) 'Enter the file name with the data to be read: '
READ (*,'(A20)') filename
 
! Open input data file.  
OPEN ( UNIT=9, FILE=filename, STATUS='OLD', ACTION='READ', &
       IOSTAT=istat )
 
! Was the OPEN successful? 
fileopen: IF ( istat == 0 ) THEN         ! Open successful
 
   ! The file was opened successfully, so read the data from
   ! it, and store it in the linked list.
   input: DO
      READ (9, *, IOSTAT=istat) temp      ! Get value
      IF ( istat /= 0 ) EXIT              ! Exit on end of data 
      nvals = nvals + 1                   ! Bump count

      IF (.NOT. ASSOCIATED(head)) THEN    ! No values in list
         ALLOCATE (head,STAT=istat)       ! Allocate new value
         tail => head                     ! Tail pts to new value
         NULLIFY (tail%p)                 ! Nullify p in new value
         tail%value = temp                ! Store number
      ELSE                                ! Values already in list
         ALLOCATE (tail%p,STAT=istat)     ! Allocate new value
         tail => tail%p                   ! Tail pts to new value
         NULLIFY (tail%p)                 ! Nullify p in new value
         tail%value = temp                ! Store number
      END IF
   END DO input
 
   ! Now, write out the data.
   ptr => head
   output: DO
      IF ( .NOT. ASSOCIATED(ptr) ) EXIT   ! Pointer valid?
      WRITE (*,'(1X,F10.4)') ptr%value    ! Yes: Write value
      ptr => ptr%p                        ! Get next pointer
   END DO output

ELSE fileopen

   ! Else file open failed.  Tell user.
   WRITE (*,'(1X,A,I6)') 'File open failed--status = ', istat

END IF fileopen
 
END PROGRAM linked_list
