MODULE shape_class
!
!   This module implements an abstract shape class.  
!
!  Record of revisions:
!      Date       Programmer          Description of change
!      ====       ==========          =====================
!    12/31/06    S. J. Chapman        Original code
!
IMPLICIT NONE

! Type definition
TYPE,PUBLIC :: shape  

   ! Instance variables. 
   ! <none >

CONTAINS

   ! Bound procedures
   PROCEDURE,PUBLIC :: area => calc_area_fn
   PROCEDURE,PUBLIC :: perimeter => calc_perimeter_fn
   PROCEDURE,PUBLIC :: to_string => to_string_fn
   
END TYPE shape

! Restrict access to the actual procedure names
PRIVATE :: calc_area_fn, calc_perimeter_fn, to_string_fn

CONTAINS

   REAL FUNCTION calc_area_fn(this)
   ! 
   ! Return the area of this object.
   !
   IMPLICIT NONE
   
   ! Declare calling arguments
   CLASS(shape) :: this               ! Shape object

   ! Return dummy area
   calc_area_fn = 0.

   END FUNCTION calc_area_fn

   REAL FUNCTION calc_perimeter_fn(this)
   ! 
   ! Return the perimeter of this object.
   !
   IMPLICIT NONE
   
   ! Declare calling arguments
   CLASS(shape) :: this               ! Shape object

   ! Return dummy perimeter
   calc_perimeter_fn = 0.

   END FUNCTION calc_perimeter_fn

   CHARACTER(len=50) FUNCTION to_string_fn(this)
   ! 
   ! Return the a character description of this object.
   !
   IMPLICIT NONE
   
   ! Declare calling arguments
   CLASS(shape) :: this               ! Shape object

   ! Return dummy string
   to_string_fn = ''

   END FUNCTION to_string_fn

END MODULE shape_class
