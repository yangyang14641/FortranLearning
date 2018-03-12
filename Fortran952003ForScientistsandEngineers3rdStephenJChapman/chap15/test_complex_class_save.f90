PROGRAM test_complex_class
USE complex_class
IMPLICIT NONE

! Declare data types
TYPE(complex_ob) :: c1
TYPE(complex_ob) :: c2
REAL :: scalar

! Initialise data
c1 = c1%ini(1.,-3.)
c2 = c2%ini(-4.,2.)
scalar = 5.

! Write the results of adding two complex numbers
WRITE (*,*) c1%add(c2)

! Write the results of adding a scalar to a complex number
WRITE (*,*) c1%add(scalar)

END PROGRAM test_complex_class
