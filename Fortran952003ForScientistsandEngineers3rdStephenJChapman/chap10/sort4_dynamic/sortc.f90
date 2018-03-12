SUBROUTINE sortc (array, n)
!
!  Purpose:
!    To sort character array "array" into ascending order using 
!    a selection sort.  This version of the subroutine sorts 
!    according to the ASCII collating sequence.  It works for 
!    character arrays with any number of elements, with array
!    elements of any length, and on processors regardless of 
!    character set. 
!
!  Record of revisions:
!      Date       Programmer          Description of change
!      ====       ==========          =====================
!    11/25/06    S. J. Chapman        Original code
! 1. 11/25/06    S. J. Chapman        Modified to work with lexical
!                                     fns and arbitrary element 
!                                     lengths
IMPLICIT NONE

! Data dictionary: declare calling parameter types & definitions
INTEGER, INTENT(IN) :: n                ! Number of values
CHARACTER(len=*), DIMENSION(n), INTENT(INOUT) :: array  
                                        ! Array to be sorted

! Data dictionary: declare local variable types & definitions
INTEGER :: i                   ! Loop index
INTEGER :: iptr                ! Pointer to smallest value
INTEGER :: j                   ! Loop index
CHARACTER(len=len(array)) :: temp  ! Temp variable for swaps
 
! Sort the array
outer: DO i = 1, n-1
 
   ! Find the minimum value in array(i) through array(n)
   iptr = i
   inner: DO j = i+1, n
      minval: IF ( LLT(array(j),array(iptr)) ) THEN
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
