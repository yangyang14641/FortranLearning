MODULE square_class
!
!   This module implements a square class.  
!
!  Record of revisions:
!      Date       Programmer          Description of change
!      ====       ==========          =====================
!    12/29/06    S. J. Chapman        Original code
!
USE rectangle_class              ! USE parent class
IMPLICIT NONE

! Type definition
TYPE,PUBLIC,EXTENDS(rectangle) :: square  

   ! Additional instance variables. 
   !<none>

CONTAINS

   ! Bound procedures
   PROCEDURE,PUBLIC :: to_string => to_string_fn
   
END TYPE square

! Restrict access to the actual procedure names
PRIVATE :: to_string_fn

! Now add methods
CONTAINS

   CHARACTER(len=50) FUNCTION to_string_fn(this)
   ! 
   ! Return the a character description of this object.
   !
   IMPLICIT NONE
   
   ! Declare calling arguments
   CLASS(square) :: this             ! Square object
   
   ! Return description
   WRITE (to_string_fn,'(A,F6.2)') 'Square of length ', &
      this%l  

   END FUNCTION to_string_fn

END MODULE square_class
