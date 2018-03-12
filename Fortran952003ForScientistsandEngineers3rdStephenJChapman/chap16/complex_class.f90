MODULE complex_class
IMPLICIT NONE

! Type definition
TYPE,PUBLIC :: complex_ob   ! This will be the name we instantiate
   PRIVATE
   REAL :: re               ! Real part
   REAL :: im               ! Imaginary part
END TYPE complex_ob

! Now add methods
CONTAINS

   !(Insert methods here)
   SUBROUTINE temp(x)
   REAL :: x
   x = 1.
   END SUBROUTINE temp

END MODULE complex_class
