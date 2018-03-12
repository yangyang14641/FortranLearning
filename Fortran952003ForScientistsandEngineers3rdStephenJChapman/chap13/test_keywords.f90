MODULE procs
CONTAINS
   REAL FUNCTION calc ( first, second, third )
   IMPLICIT NONE
   REAL, INTENT(IN) :: first, second, third
   calc = ( first - second ) / third
   END FUNCTION calc
END MODULE procs

PROGRAM test_keywords
 
USE procs 
IMPLICIT NONE

WRITE (*,*) calc ( 3., 1., 2. )
WRITE (*,*) calc ( first=3., second=1., third=2. )
WRITE (*,*) calc ( second=1., third=2., first=3. )
WRITE (*,*) calc ( 3., third=2., second=1.)

END PROGRAM test_keywords
