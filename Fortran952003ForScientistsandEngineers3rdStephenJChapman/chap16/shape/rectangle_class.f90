MODULE rectangle_class
!
!   This module implements a rectangle class.  
!
!  Record of revisions:
!      Date       Programmer          Description of change
!      ====       ==========          =====================
!    12/29/06    S. J. Chapman        Original code
!
USE shape_class                  ! USE parent class
IMPLICIT NONE

! Type definition
TYPE,PUBLIC,EXTENDS(shape) :: rectangle  

   ! Additional instance variables. 
   REAL :: l = 0               ! Length
   REAL :: w = 0               ! Width

CONTAINS

   ! Bound procedures
   PROCEDURE,PUBLIC :: initialize => initialize_sub
   PROCEDURE,PUBLIC :: area => get_area_fn
   PROCEDURE,PUBLIC :: perimeter => get_perimeter_fn
   PROCEDURE,PUBLIC :: to_string => to_string_fn

END TYPE rectangle

! Restrict access to the actual procedure names
PRIVATE :: initialize_sub, get_area_fn, get_perimeter_fn
PRIVATE :: to_string_fn

! Now add methods
CONTAINS

   SUBROUTINE initialize_sub(this,l,w)
   ! 
   ! Initialize the rectangle object.
   !
   IMPLICIT NONE
   
   ! Declare calling arguments
   CLASS(rectangle) :: this            ! Rectangle object
   REAL,INTENT(IN) :: l                ! Length
   REAL,INTENT(IN) :: w                ! Width
   
   ! Initialize the rectangle
   this%l = l
   this%w = w

   END SUBROUTINE initialize_sub


   REAL FUNCTION get_area_fn(this)
   ! 
   ! Return the area of this object.
   !
   IMPLICIT NONE
   
   ! Declare calling arguments
   CLASS(rectangle) :: this             ! Rectangle object
   
   ! Calculate area
   get_area_fn = this%l * this%w

   END FUNCTION get_area_fn


   REAL FUNCTION get_perimeter_fn(this)
   ! 
   ! Return the perimeter of this object.
   !
   IMPLICIT NONE
   
   ! Declare calling arguments
   CLASS(rectangle) :: this             ! Rectangle object
   
   ! Calculate perimeter
   get_perimeter_fn = 2 * this%l + 2 * this%w

   END FUNCTION get_perimeter_fn


   CHARACTER(len=50) FUNCTION to_string_fn(this)
   ! 
   ! Return the a character description of this object.
   !
   IMPLICIT NONE
   
   ! Declare calling arguments
   CLASS(rectangle) :: this             ! Rectangle object
   
   ! Return description
   WRITE (to_string_fn,'(A,F6.2,A,F6.2)') 'Rectangle of length ', &
      this%l, ' and width ', this%w 

   END FUNCTION to_string_fn

END MODULE rectangle_class
