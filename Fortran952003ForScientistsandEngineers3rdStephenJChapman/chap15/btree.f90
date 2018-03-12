MODULE btree
!
!  Purpose:
!    To define the derived data type used as a node in the
!    binary tree, and to define the operations >, <. and ==
!    for this data type.  This module also contains the 
!    subroutines to add a node to the tree, write out the  
!    values in the tree, and find a value in the tree.
!
!  Record of revisions:
!      Date       Programmer          Description of change
!      ====       ==========          =====================
!    12/24/06    S. J. Chapman        Original code
!
IMPLICIT NONE

! Restrict access to module contents.
PRIVATE
PUBLIC :: node, OPERATOR(>), OPERATOR(<), OPERATOR(==)
PUBLIC :: add_node, write_node, find_node

! Declare type for a node of the binary tree.
TYPE :: node            
   CHARACTER(len=10) :: last
   CHARACTER(len=10) :: first
   CHARACTER :: mi
   CHARACTER(len=16) :: phone
   TYPE (node), POINTER :: before
   TYPE (node), POINTER :: after
END TYPE

INTERFACE OPERATOR (>)
   MODULE PROCEDURE greater_than
END INTERFACE

INTERFACE OPERATOR (<)
   MODULE PROCEDURE less_than
END INTERFACE

INTERFACE OPERATOR (==)
   MODULE PROCEDURE equal_to
END INTERFACE

CONTAINS
   RECURSIVE SUBROUTINE add_node (ptr, new_node)
   !
   !  Purpose:
   !    To add a new node to the binary tree structure.
   !
   TYPE (node), POINTER :: ptr  ! Pointer to current pos. in tree
   TYPE (node), POINTER :: new_node ! Pointer to new node

   IF ( .NOT. ASSOCIATED(ptr) ) THEN
      ! There is no tree yet.  Add the node right here.
      ptr => new_node
   ELSE IF ( new_node < ptr ) THEN
      IF ( ASSOCIATED(ptr%before) ) THEN
         CALL add_node ( ptr%before, new_node )
      ELSE
         ptr%before => new_node
      END IF
   ELSE 
      IF ( ASSOCIATED(ptr%after) ) THEN
         CALL add_node ( ptr%after, new_node )
      ELSE
         ptr%after => new_node
      END IF
   END IF
   END SUBROUTINE add_node
      
   RECURSIVE SUBROUTINE write_node (ptr)
   !
   !  Purpose:
   !    To write out the contents of the binary tree 
   !    structure in order. 
   !
   TYPE (node), POINTER :: ptr  ! Pointer to current pos. in tree

   ! Write contents of previous node.
   IF ( ASSOCIATED(ptr%before) ) THEN
      CALL write_node ( ptr%before )
   END IF
   
   ! Write contents of current node.
   WRITE (*,"(1X,A,', ',A,1X,A)") ptr%last, ptr%first, ptr%mi

   ! Write contents of next node.
   IF ( ASSOCIATED(ptr%after) ) THEN
      CALL write_node ( ptr%after )
   END IF
   END SUBROUTINE write_node
      
   RECURSIVE SUBROUTINE find_node (ptr, search, error)
   !
   !  Purpose:
   !    To find a particular node in the binary tree structure.
   !    "Search" is a pointer to the name to find, and will 
   !    also contain the results when the subroutine finishes
   !    if the node is found.
   !
   TYPE (node), POINTER :: ptr    ! Pointer to curr pos. in tree
   TYPE (node), POINTER :: search ! Pointer to value to find.
   INTEGER :: error               ! Error: 0 = ok, 1 = not found

   IF ( search < ptr ) THEN
      IF ( ASSOCIATED(ptr%before) ) THEN
         CALL find_node (ptr%before, search, error)
      ELSE
         error = 1
      END IF
   ELSE IF ( search == ptr ) THEN
      search = ptr
      error = 0
   ELSE 
      IF ( ASSOCIATED(ptr%after) ) THEN
         CALL find_node (ptr%after, search, error)
      ELSE
         error = 1
      END IF
   END IF
   END SUBROUTINE find_node
      
   LOGICAL FUNCTION greater_than (op1, op2)
   !
   !  Purpose:
   !    To test to see if operand 1 is > operand 2
   !    in alphabetical order.
   !
   TYPE (node), INTENT(IN) :: op1, op2
   CHARACTER(len=10) :: last1, last2, first1, first2
   CHARACTER :: mi1, mi2
   
   CALL ushift (op1, last1, first1, mi1 )
   CALL ushift (op2, last2, first2, mi2 )

   IF (last1 > last2) THEN
      greater_than = .TRUE.
   ELSE IF (last1 < last2) THEN
      greater_than = .FALSE.
   ELSE ! Last names match
      IF (first1 > first2) THEN
         greater_than = .TRUE.
      ELSE IF (first1 < first2) THEN
         greater_than = .FALSE.
      ELSE ! First names match
         IF (mi1 > mi2) THEN
            greater_than = .TRUE.
         ELSE
            greater_than = .FALSE.
         END IF
      END IF
   END IF
   END FUNCTION greater_than

   LOGICAL FUNCTION less_than (op1, op2)
   !
   !  Purpose:
   !    To test to see if operand 1 is < operand 2
   !    in alphabetical order.
   !
   TYPE (node), INTENT(IN) :: op1, op2
   CHARACTER(len=10) :: last1, last2, first1, first2
   CHARACTER :: mi1, mi2
   
   CALL ushift (op1, last1, first1, mi1 )
   CALL ushift (op2, last2, first2, mi2 )

   IF (last1 < last2) THEN
      less_than = .TRUE.
   ELSE IF (last1 > last2) THEN
      less_than = .FALSE.
   ELSE ! Last names match
      IF (first1 < first2) THEN
         less_than = .TRUE.
      ELSE IF (first1 > first2) THEN
         less_than = .FALSE.
      ELSE ! First names match
         IF (mi1 < mi2) THEN
            less_than = .TRUE.
         ELSE
            less_than = .FALSE.
         END IF
      END IF
   END IF
   END FUNCTION less_than

    LOGICAL FUNCTION equal_to (op1, op2)
   !
   !  Purpose:
   !    To test to see if operand 1 is equal to operand 2
   !    alphabetically.
   !
   TYPE (node), INTENT(IN) :: op1, op2

   CHARACTER(len=10) :: last1, last2, first1, first2
   CHARACTER :: mi1, mi2
   
   CALL ushift (op1, last1, first1, mi1 )
   CALL ushift (op2, last2, first2, mi2 )

   IF ( (last1 == last2) .AND. (first1 == first2) .AND. &
        (mi1 == mi2 ) ) THEN
      equal_to = .TRUE.
   ELSE
      equal_to = .FALSE.
   END IF
   END FUNCTION equal_to

   SUBROUTINE ushift( op, last, first, mi )
   !
   !  Purpose:
   !    To create upshifted versions of all strings for
   !    comparison.
   !
   TYPE (node), INTENT(IN) :: op
   CHARACTER(len=10), INTENT(INOUT) :: last, first 
   CHARACTER, INTENT(INOUT) :: mi 
   
   last = op%last
   first = op%first
   mi = op%mi
   CALL ucase (last)
   CALL ucase (first)
   CALL ucase (mi)
   END SUBROUTINE ushift

   SUBROUTINE ucase ( string )
   ! 
   !  Purpose: 
   !    To shift a character string to upper case on any processor,
   !    regardless of collating sequence.
   ! 
   !  Record of revisions:
   !      Date       Programmer          Description of change
   !      ====       ==========          =====================
   !    11/25/06    S. J. Chapman        Original code
   !
   IMPLICIT NONE

   ! Declare calling parameters:
   CHARACTER(len=*), INTENT(INOUT) :: string  

   ! Declare local variables:
   INTEGER :: i                 ! Loop index
   INTEGER :: length            ! Length of input string

   ! Get length of string
   length = LEN ( string )

   ! Now shift lower case letters to upper case.
   DO i = 1, length
      IF ( LGE(string(i:i),'a') .AND. LLE(string(i:i),'z') ) THEN
         string(i:i) = ACHAR ( IACHAR ( string(i:i) ) - 32 )
      END IF
   END DO

   END SUBROUTINE ucase

END MODULE btree


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
!    12/24/06    S. J. Chapman        Original code
!
USE btree
IMPLICIT NONE

! Data dictionary: declare variable types & definitions
INTEGER :: error                    ! Error flag: 0=success
CHARACTER(len=20) :: filename       ! Input data file name
INTEGER :: istat                    ! Status: 0 for success
TYPE (node), POINTER :: root        ! Pointer to root node
TYPE (node), POINTER :: temp        ! Temp pointer to node

! Nullify new pointers
NULLIFY ( root, temp )

! Get the name of the file containing the input data.
WRITE (*,*) 'Enter the file name with the input data: '
READ (*,'(A20)') filename
 
! Open input data file.  Status is OLD because the input data must 
! already exist.
OPEN ( UNIT=9, FILE=filename, STATUS='OLD', ACTION='READ', &
       IOSTAT=istat )
 
! Was the OPEN successful? 
fileopen: IF ( istat == 0 ) THEN         ! Open successful
 
   ! The file was opened successfully, allocate space for each 
   ! node, read the data into that node, and insert it into the
   ! binary tree.
   input: DO
      ALLOCATE (temp,STAT=istat)          ! Allocate node
      NULLIFY ( temp%before, temp%after)  ! Nullify pointers

      READ (9, 100, IOSTAT=istat) temp%last, temp%first, &
         temp%mi, temp%phone              ! Read data
      100 FORMAT (A10,1X,A10,1X,A1,1X,A16)
      IF ( istat /= 0 ) EXIT input        ! Exit on end of data 
      CALL add_node(root, temp)           ! Add to binary tree
   END DO input
       
   ! Now, write out the sorted data.
   WRITE (*,'(/,1X,A)') 'The sorted data list is: '
   CALL write_node(root)

   ! Prompt for a name to search for in the tree.
   WRITE (*,'(/,1X,A)') 'Enter name to recover from tree:'
   WRITE (*,'(1X,A)',ADVANCE='NO') 'Last Name:      '
   READ (*,'(A)') temp%last
   WRITE (*,'(1X,A)',ADVANCE='NO') 'First Name:     '
   READ (*,'(A)') temp%first                    
   WRITE (*,'(1X,A)',ADVANCE='NO') 'Middle Initial: '
   READ (*,'(A)') temp%mi

   ! Locate record
   CALL find_node ( root, temp, error )
   check: IF ( error == 0 ) THEN
      WRITE (*,'(/,1X,A)') 'The record is:'
      WRITE (*,'(1X,7A)') temp%last, ', ', temp%first, ' ', &
                    temp%mi, '  ', temp%phone
   ELSE
      WRITE (*,'(/,1X,A)') 'Specified node not found!'
   END IF check

ELSE fileopen

   ! Else file open failed.  Tell user.
   WRITE (*,'(1X,A,I6)') 'File open failed--status = ', istat

END IF fileopen
 
END PROGRAM binary_tree
