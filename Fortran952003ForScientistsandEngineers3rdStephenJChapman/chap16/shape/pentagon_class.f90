MODULE pentagon_class
!
!   This module implements a pentagon class.  
!
!  Record of revisions:
!      Date       Programmer          Description of change
!      ====       ==========          =====================
!    12/29/06    S. J. Chapman        Original code
!
USE shape_class                  ! USE parent class
IMPLICIT NONE

! Type definition
TYPE,PUBLIC,EXTENDS(shape) :: pentagon  

   ! Additional instance variables. 
   REAL :: s = 0               ! Length of side

CONTAINS

   ! Bound procedures
   PROCEDURE,PUBLIC :: initialize => initialize_sub
   PROCEDURE,PUBLIC :: area => get_area_fn
   PROCEDURE,PUBLIC :: perimeter => get_perimeter_fn
   PROCEDURE,PUBLIC :: to_string => to_string_fn

END TYPE pentagon

! Restrict access to the actual procedure names
PRIVATE :: initialize_sub, get_area_fn, get_perimeter_fn
PRIVATE :: to_string_fn

! Now add methods
CONTAINS

   SUBROUTINE initialize_sub(this,s)
   ! 
   ! Initialize the pentagon object.
   !
   IMPLICIT NONE
   
   ! Declare calling arguments
   CLASS(pentagon) :: this            ! Pentagon object
   REAL,INTENT(IN) :: s               ! Length of side
   
   ! Initialize the pentagon
   this%s = s

   END SUBROUTINE initialize_sub


   REAL FUNCTION get_area_fn(this)
   ! 
   ! Return the area of this object.
   !
   IMPLICIT NONE
   
   ! Declare calling arguments
   CLASS(pentagon) :: this             ! Pentagon object
   
   ! Calculate area [0.72654253 is tan(PI/5)]
   get_area_fn = 1.25 * this%s**2 / 0.72654253

   END FUNCTION get_area_fn


   REAL FUNCTION get_perimeter_fn(this)
   ! 
   ! Return the perimeter of this object.
   !
   IMPLICIT NONE
   
   ! Declare calling arguments
   CLASS(pentagon) :: this             ! Pentagon object
   
   ! Calculate perimeter
   get_perimeter_fn = 5.0 * this%s

   END FUNCTION get_perimeter_fn


   CHARACTER(len=50) FUNCTION to_string_fn(this)
   ! 
   ! Return the a character description of this object.
   !
   IMPLICIT NONE
   
   ! Declare calling arguments
   CLASS(pentagon) :: this             ! Pentagon object
   
   ! Return description
   WRITE (to_string_fn,'(A,F6.2)') 'Pentagon of side ', &
      this%s 

   END FUNCTION to_string_fn

END MODULE pentagon_class
