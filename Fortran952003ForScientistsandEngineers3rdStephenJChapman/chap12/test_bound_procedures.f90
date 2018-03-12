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


PROGRAM test_bound_procedures
!
!  Purpose:
!    To test bound procedurres.
!
!  Record of revisions:
!       Date       Programmer          Description of change
!       ====       ==========          =====================
!     12/04/06    S. J. Chapman        Original code
!
USE point_module
IMPLICIT NONE

! Declare a variable of type point
TYPE (point) :: a, b, c

! Initialize variables
a%x = -10.
a%y = 5.
b%x = 4.
b%y = 2.

! Add points
c = a%add(b)

! Output variable using free format I/O
WRITE (*,*) 'c = ', c 

END PROGRAM test_bound_procedures
