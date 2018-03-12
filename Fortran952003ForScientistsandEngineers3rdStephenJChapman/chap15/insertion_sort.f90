PROGRAM insertion_sort
!
!  Purpose:
!    To read a series of integer values from an input data file
!    and sort them using an insertion sort.  After the values
!    are sorted, they will be written back to the standard
!    output device.
!
!  Record of revisions:
!      Date       Programmer          Description of change
!      ====       ==========          =====================
!    12/23/06    S. J. Chapman        Original code
!
IMPLICIT NONE

! Derived data type to store integer values in
TYPE :: int_value
   INTEGER :: value
   TYPE (int_value), POINTER :: next_value
END TYPE

! Data dictionary: declare variable types & definitions
TYPE (int_value), POINTER :: head   ! Pointer to head of list
CHARACTER(len=20) :: filename       ! Input data file name
INTEGER :: istat                    ! Status: 0 for success
INTEGER :: nvals = 0                ! Number of data read
TYPE (int_value), POINTER :: ptr    ! Ptr to new value
TYPE (int_value), POINTER :: ptr1   ! Temp ptr for search
TYPE (int_value), POINTER :: ptr2   ! Temp ptr for search
TYPE (int_value), POINTER :: tail   ! Pointer to tail of list
INTEGER :: temp                     ! Temporary variable

! Get the name of the file containing the input data.
WRITE (*,*) 'Enter the file name with the data to be sorted: '
READ (*,'(A20)') filename

! Open input data file.
OPEN ( UNIT=9, FILE=filename, STATUS='OLD', ACTION='READ', &
       IOSTAT=istat )

! Was the OPEN successful?
fileopen: IF ( istat == 0 ) THEN         ! Open successful

   ! The file was opened successfully, so read the data value
   ! to sort, allocate a variable for it, and locate the proper
   ! point to insert the new value into the list.
   input: DO
      READ (9, *, IOSTAT=istat) temp      ! Get value
      IF ( istat /= 0 ) EXIT input        ! Exit on end of data
      nvals = nvals + 1                   ! Bump count

      ALLOCATE (ptr,STAT=istat)           ! Allocate space
      ptr%value = temp                    ! Store number

      ! Now find out where to put it in the list.
      new: IF (.NOT. ASSOCIATED(head)) THEN ! No values in list
         head => ptr                      ! Place at front
         tail => head                     ! Tail pts to new value
         NULLIFY (ptr%next_value)         ! Nullify next ptr
      ELSE
         ! Values already in list.  Check for location.
         front: IF ( ptr%value < head%value ) THEN
            ! Add at front of list
            ptr%next_value => head
            head => ptr
         ELSE IF ( ptr%value >= tail%value ) THEN
            ! Add at end of list
            tail%next_value => ptr
            tail => ptr
            NULLIFY ( tail%next_value )
         ELSE
            ! Find place to add value
            ptr1 => head
            ptr2 => ptr1%next_value
            search: DO
               IF ( (ptr%value >= ptr1%value) .AND. &
                    (ptr%value < ptr2%value) ) THEN
                  ! Insert value here
                  ptr%next_value => ptr2
                  ptr1%next_value => ptr
                  EXIT search
               END IF
               ptr1 => ptr2
               ptr2 => ptr2%next_value
            END DO search
         END IF front
      END IF new
   END DO input

   ! Now, write out the data.
   ptr => head
   output: DO
      IF ( .NOT. ASSOCIATED(ptr) ) EXIT   ! Pointer valid?
      WRITE (*,'(1X,I10)') ptr%value      ! Yes: Write value
      ptr => ptr%next_value               ! Get next pointer
   END DO output

ELSE fileopen

   ! Else file open failed.  Tell user.
   WRITE (*,'(1X,A,I6)') 'File open failed--status = ', istat

END IF fileopen

END PROGRAM insertion_sort
