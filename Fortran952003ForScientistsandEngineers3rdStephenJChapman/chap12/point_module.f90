MODULE point_module
IMPLICIT NONE

! Type definition
TYPE :: point
   REAL :: x
   REAL :: y
CONTAINS
   PROCEDURE,PASS :: add
END TYPE

CONTAINS

   TYPE(point) FUNCTION add(this, another_point)
   CLASS(point) :: this, another_point
   add%x = this%x + another_point%x
   add%y = this%y + another_point%y
   END FUNCTION add

END MODULE point_module
