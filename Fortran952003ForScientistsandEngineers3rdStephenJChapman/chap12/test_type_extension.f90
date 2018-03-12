PROGRAM test_type_extension
!
!  Purpose:
!    To illustrate type extension of derived data types.
!
!  Record of revisions:
!       Date       Programmer          Description of change
!       ====       ==========          =====================
!     12/04/06    S. J. Chapman        Original code
!
IMPLICIT NONE

! Declare type point
TYPE :: point
   REAL :: x
   REAL :: y
END TYPE

! Declare type point3d
TYPE, EXTENDS(point) :: point3d
   REAL :: z
END TYPE

! Declare a variable of type person:
TYPE (point3d) :: my_point

! Initialize variable
my_point%x = 1.
my_point%y = 2.
my_point%z = 3.

! Output variable using free format I/O
WRITE (*,*) 'my_point = ', my_point 

END PROGRAM test_type_extension
