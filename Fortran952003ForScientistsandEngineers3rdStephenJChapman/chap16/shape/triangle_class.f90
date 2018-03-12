MODULE triangle_class
!
!   This module implements a triangle class.  
!
!  Record of revisions:
!      Date       Programmer          Description of change
!      ====       ==========          =====================
!    12/29/06    S. J. Chapman        Original code
!
USE shape_class                  ! USE parent class
IMPLICIT NONE

! Type definition
TYPE,PUBLIC,EXTENDS(shape) :: triangle  

   ! Additional instance variables. 
   REAL :: s = 0               ! Length of side

CONTAINS

   ! Bound procedures
   PROCEDURE,PUBLIC :: initialize => initialize_sub
   PROCEDURE,PUBLIC :: area => get_area_fn
   PROCEDURE,PUBLIC :: perimeter => get_perimeter_fn
   PROCEDURE,PUBLIC :: to_string => to_string_fn

END TYPE triangle

! Restrict access to the actual procedure names
PRIVATE :: initialize_sub, get_area_fn, get_perimeter_fn
PRIVATE :: to_string_fn

! Now add methods
CONTAINS

   SUBROUTINE initialize_sub(this,s)
   ! 
   ! Initialize the triangle object.
   !
   IMPLICIT NONE
   
   ! Declare calling arguments
   CLASS(triangle) :: this             ! Triangle object
   REAL,INTENT(IN) :: s                ! Length of side
   
   ! Initialize the triangle
   this%s = s

   END SUBROUTINE initialize_sub


   REAL FUNCTION get_area_fn(this)
   ! 
   ! Return the area of this object.
   !
   IMPLICIT NONE
   
   ! Declare calling arguments
   CLASS(triangle) :: this             ! Triangle object
   
   ! Calculate area
   get_area_fn = SQRT(3.0) / 4.0 * this%s**2

   END FUNCTION get_area_fn


   REAL FUNCTION get_perimeter_fn(this)
   ! 
   ! Return the perimeter of this object.
   !
   IMPLICIT NONE
   
   ! Declare calling arguments
   CLASS(triangle) :: this             ! Triangle object
   
   ! Calculate perimeter
   get_perimeter_fn = 3.0 * this%s

   END FUNCTION get_perimeter_fn


   CHARACTER(len=50) FUNCTION to_string_fn(this)
   ! 
   ! Return the a character description of this object.
   !
   IMPLICIT NONE
   
   ! Declare calling arguments
   CLASS(triangle) :: this             ! Triangle object
   
   ! Return description
   WRITE (to_string_fn,'(A,F6.2)') 'Equilateral triangle of side ', &
      this%s 

   END FUNCTION to_string_fn

END MODULE triangle_class
