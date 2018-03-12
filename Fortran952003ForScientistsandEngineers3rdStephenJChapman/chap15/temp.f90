MODULE my_functions
CONTAINS
   FUNCTION every_fifth (ptr_array) RESULT (ptr_fifth )
   !
   !  Purpose:
   !    To produce a pointer to every fifth element in an
   !    input rank one array.
   !
   !  Record of revisions:
   !      Date       Programmer          Description of change
   !      ====       ==========          =====================
   !    12/23/06    S. J. Chapman        Original code
   !
   IMPLICIT NONE

   ! Declare calling arguments:
   INTEGER, DIMENSION(:), POINTER :: ptr_array
   INTEGER, DIMENSION(:), POINTER :: ptr_fifth

   ! Declare local variables:
   INTEGER :: low           ! Array lower bound
   INTEGER :: high          ! Array upper bound

   low = LBOUND(ptr_array,1)
   high = UBOUND(ptr_array,1)
   ptr_fifth => ptr_array(low:high:5)

   END FUNCTION every_fifth
END MODULE my_functions

PROGRAM temp
USE my_functions
INTEGER,DIMENSION(:),POINTER :: ptr_1
ALLOCATE (ptr_1(25) )
ptr_1 = (/ (i, i=1,25) /)
WRITE (*,*) every_fifth( ptr_1 )

END PROGRAM temp