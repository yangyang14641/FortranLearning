MODULE complex_class
IMPLICIT NONE

! Type definition
TYPE,PUBLIC :: complex_ob   ! This will be the name we instantiate
   !PRIVATE
   REAL :: re               ! Real part
   REAL :: im               ! Imaginary part
CONTAINS
   PRIVATE
   PROCEDURE :: ac => add_complex_to_complex
   PROCEDURE :: ar => add_real_to_complex
   GENERIC, PUBLIC :: add => ac, ar 
   PROCEDURE, PUBLIC :: ini
END TYPE complex_ob

! Declare access for methods
PRIVATE :: add_complex_to_complex, add_real_to_complex

! Now add methods
CONTAINS

   ! Method to initialise complex number
   SUBROUTINE ini(this, re, im)
   CLASS(complex_ob),INTENT(OUT) :: this
   REAL,INTENT(IN) :: re,im
   this%re = re
   this%im = im
   END SUBROUTINE ini
   
   ! Insert method add_complex_to_complex here:
   SUBROUTINE add_complex_to_complex( this, c2 )
   CLASS(complex_ob) :: this
   CLASS(complex_ob),INTENT(IN) :: c2
   this%re = this%re + c2%re
   this%im = this%im + c2%im
   END SUBROUTINE add_complex_to_complex

   ! Insert method add_real_to_complex here:
   SUBROUTINE add_real_to_complex( this, a )
   CLASS(complex_ob) :: this
   REAL,INTENT(IN) :: a
   this%re = this%re + a
   END SUBROUTINE add_real_to_complex

END MODULE complex_class


