MODULE circle_class
!
!   This module implements a circle class.  
!
!  Record of revisions:
!      Date       Programmer          Description of change
!      ====       ==========          =====================
!    12/29/06    S. J. Chapman        Original code
!
USE shape_class                  ! USE parent class
IMPLICIT NONE

! Type definition
TYPE,PUBLIC,EXTENDS(shape) :: circle  

   ! Additional instance variables. 
   REAL :: r = 0               ! Radius

CONTAINS

   ! Bound procedures
   PROCEDURE,PUBLIC :: initialize => initialize_sub
   PROCEDURE,PUBLIC :: area => get_area_fn
   PROCEDURE,PUBLIC :: perimeter => get_perimeter_fn
   PROCEDURE,PUBLIC :: to_string => to_string_fn

END TYPE circle

! Declare constant PI
REAL,PARAMETER :: PI = 3.141593

! Restrict access to the actual procedure names
PRIVATE :: initialize_sub, get_area_fn, get_perimeter_fn
PRIVATE :: to_string_fn

! Now add methods
CONTAINS

   SUBROUTINE initialize_sub(this,r)
   ! 
   ! Initialize the circle object.
   !
   IMPLICIT NONE
   
   ! Declare calling arguments
   CLASS(circle) :: this               ! Circle object
   REAL,INTENT(IN) :: r                ! Radius
   
   ! Initialize the circle
   this%r = r

   END SUBROUTINE initialize_sub


   REAL FUNCTION get_area_fn(this)
   ! 
   ! Return the area of this object.
   !
   IMPLICIT NONE
   
   ! Declare calling arguments
   CLASS(circle) :: this             ! Circle object
   
   ! Calculate area
   get_area_fn = PI * this%r**2

   END FUNCTION get_area_fn


   REAL FUNCTION get_perimeter_fn(this)
   ! 
   ! Return the perimeter of this object.
   !
   IMPLICIT NONE
   
   ! Declare calling arguments
   CLASS(circle) :: this             ! Circle object
   
   ! Calculate perimeter
   get_perimeter_fn = 2.0 * PI * this%r

   END FUNCTION get_perimeter_fn


   CHARACTER(len=50) FUNCTION to_string_fn(this)
   ! 
   ! Return the a character description of this object.
   !
   IMPLICIT NONE
   
   ! Declare calling arguments
   CLASS(circle) :: this             ! Circle object
   
   ! Return description
   WRITE (to_string_fn,'(A,F6.2)') 'Circle of radius ', &
      this%r 

   END FUNCTION to_string_fn

END MODULE circle_class
