MODULE generic_procedure_module
!
!  Purpose:
!    To define the derived data type for 2D vectors,  
!    plus two generic bound procedures.
!
!  Record of revisions:
!       Date       Programmer          Description of change
!       ====       ==========          =====================
!     12/27/06    S. J. Chapman        Original code
!
IMPLICIT NONE

! Declare type vector
TYPE :: vector
   REAL :: x                         ! X value
   REAL :: y                         ! Y value
CONTAINS
   GENERIC :: add => vector_plus_vector, vector_plus_scalar
   PROCEDURE,PASS :: vector_plus_vector
   PROCEDURE,PASS :: vector_plus_scalar
END TYPE vector

! Add procedures
CONTAINS

   TYPE (vector) FUNCTION vector_plus_vector ( this, v2 )
   !
   !  Purpose:
   !    To add two vectors.
   !
   !  Record of revisions:
   !       Date       Programmer          Description of change
   !       ====       ==========          =====================
   !     12/27/06    S. J. Chapman        Original code
   !
   IMPLICIT NONE
   
   ! Data dictionary: declare calling parameter types & definitions
   CLASS(vector),INTENT(IN) :: this         ! First vector
   CLASS(vector),INTENT(IN) :: v2           ! Second vector

   ! Add the vectors
   vector_plus_vector%x = this%x + v2%x
   vector_plus_vector%y = this%y + v2%y

   END FUNCTION vector_plus_vector

   TYPE (vector) FUNCTION vector_plus_scalar ( this, s )
   !
   !  Purpose:
   !    To add a vector and a scalar.
   !
   !  Record of revisions:
   !       Date       Programmer          Description of change
   !       ====       ==========          =====================
   !     12/27/06    S. J. Chapman        Original code
   !
   IMPLICIT NONE
   
   ! Data dictionary: declare calling parameter types & definitions
   CLASS(vector),INTENT(IN) :: this         ! First vector
   REAL,INTENT(IN) :: s                     ! Scalar

   ! Add the points
   vector_plus_scalar%x = this%x + s
   vector_plus_scalar%y = this%y + s

   END FUNCTION vector_plus_scalar

END MODULE generic_procedure_module
