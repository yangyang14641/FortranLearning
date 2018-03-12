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
   !    12/24/06    S. J. Chapman        Original code
   !
   IMPLICIT NONE

   ! Data dictionary: declare calling parameter types & definitions
   INTEGER, DIMENSION(:), POINTER :: ptr_array
   INTEGER, DIMENSION(:), POINTER :: ptr_fifth

   ! Data dictionary: declare local variable types & definitions
   INTEGER :: low           ! Array lower bound
   INTEGER :: high          ! Array upper bound

   low = LBOUND(ptr_array,1)
   high = UBOUND(ptr_array,1)
   ptr_fifth => ptr_array(low:high:5)

   END FUNCTION every_fifth
END MODULE

