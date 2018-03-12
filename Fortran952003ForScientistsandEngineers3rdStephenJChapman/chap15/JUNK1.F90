PROGRAM binary_tree
!
!  Purpose:
!    To read in a series of random names and phone numbers
!    and store them in a binary tree.  After the values are
!    stored, they are written out in sorted order.  Then the
!    user is prompted for a name to retrieve, and the program
!    recovers the data associated with that name.
!
!  Record of revisions:
!      Date       Programmer          Description of change
!      ====       ==========          =====================
!    12/23/06    S. J. Chapman        Original code
!
USE btree
IMPLICIT NONE

! List of variables:
INTEGER :: error                    ! Error flag: 0=success
CHARACTER(len=20) :: filename       ! Input data file name
INTEGER :: istat                    ! Status: 0 for success
INTEGER :: nvals = 0                ! Number of data read
TYPE (node), POINTER :: root        ! Pointer to root node
TYPE (node), POINTER :: temp        ! Temp pointer to node

! Nullify new pointers
NULLIFY ( root, temp )

! Get the name of the file containing the input data.
WRITE (*,*) 'Enter the file name with the input data: '
READ (*,'(A20)') filename
 
! Open input data file.  Status is OLD because the input data must 
! already exist.
OPEN ( UNIT=9, FILE=filename, STATUS='OLD', IOSTAT=istat )
 
! Was the OPEN successful? 
fileopen: IF ( istat == 0 ) THEN         ! Open successful
 
   ! The file was opened successfully, allocate space for each 
   ! node, read the data into that node, and insert it into the
   ! bibary tree.
   input: DO
      ALLOCATE (temp,STAT=istat)          ! Allocate node
      NULLIFY ( temp%before, temp%after)  ! Nullify pointers

      READ (9, '(4A)', IOSTAT=istat) temp ! Get value
      IF ( istat /= 0 ) EXIT input        ! Exit on end of data 
      nvals = nvals + 1                   ! Bump count

      CALL add_node(root, temp)           ! Add to binary tree
   END DO input
       
   ! Now, write out the sorted data.
   WRITE (*,'(/,1X,A)') 'The sorted data list is: '
   CALL write_node(root)

   ! Prompt for a name to search for in the tree.
   WRITE (*,'(/,1X,A)') 'Enter name to recover from tree:'
   WRITE (*,*,ADVANCE='NO') 'Last Name: '
   READ (*,'(A)',ADVANCE='NO') temp%last
   WRITE (*,*,ADVANCE='NO') '   First Name: '
   READ (*,'(A)',ADVANCE='NO') temp%last
   WRITE (*,*,ADVANCE='NO') '   Middle Initial: '
   READ (*,'(A)',ADVANCE='YES') temp%last

   ! Locate record
   CALL find_node ( root, temp, error )
   check: IF ( error == 0 ) THEN
      WRITE (*,'(/,A)') 'The record is:'
      WRITE (*'(1X,7A)') 'temp%last, ', ', temp%first, ' ',&
                    temp%mi, '  ', temp%phone
   ELSE
      WRITE (*,'(/,A)') 'Specified node not found!'
   END IF check

END IF fileopen
 
END PROGRAM
