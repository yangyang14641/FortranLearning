MODULE vector_module
!
!  Purpose:
!    To define the derived data type for 2D vectors,  
!    plus addition and subtraction operations.
!
!  Record of revisions:
!       Date       Programmer          Description of change
!       ====       ==========          =====================
!     12/04/06    S. J. Chapman        Original code
! 1.  12/22/06    S. J. Chapman        Use bound procedures
!
IMPLICIT NONE

! Declare type vector
TYPE :: vector
   REAL :: x                         ! X value
   REAL :: y                         ! Y value
CONTAINS
   PROCEDURE,PASS :: vector_add
   PROCEDURE,PASS :: vector_sub
END TYPE vector

! Add procedures
CONTAINS

   TYPE (vector) FUNCTION vector_add ( this, v2 )
   !
   !  Purpose:
   !    To add two vectors.
   !
   !  Record of revisions:
   !       Date       Programmer          Description of change
   !       ====       ==========          =====================
   !     12/04/06    S. J. Chapman        Original code
   ! 1.  12/22/06    S. J. Chapman        Use bound procedures
   !
   IMPLICIT NONE
   
   ! Data dictionary: declare calling parameter types & definitions
   CLASS(vector),INTENT(IN) :: this         ! First vector
   CLASS(vector),INTENT(IN) :: v2           ! Second vector

   ! Add the vectors
   vector_add%x = this%x + v2%x
   vector_add%y = this%y + v2%y

   END FUNCTION vector_add

   TYPE (vector) FUNCTION vector_sub ( this, v2 )
   !
   !  Purpose:
   !    To subtract two vectors.
   !
   !  Record of revisions:
   !       Date       Programmer          Description of change
   !       ====       ==========          =====================
   !     12/04/06    S. J. Chapman        Original code
   ! 1.  12/22/06    S. J. Chapman        Use bound procedures
   !
   IMPLICIT NONE
   
   ! Data dictionary: declare calling parameter types & definitions
   CLASS(vector),INTENT(IN) :: this         ! First vector
   CLASS(vector),INTENT(IN) :: v2           ! Second vector

   ! Add the points
   vector_sub%x = this%x - v2%x
   vector_sub%y = this%y - v2%y

   END FUNCTION vector_sub

END MODULE vector_module
