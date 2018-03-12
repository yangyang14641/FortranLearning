PROGRAM test_ptr
IMPLICIT NONE
REAL, POINTER :: p
REAL, TARGET :: t1 = 10., t2 = -17.
p => t1
WRITE (*,*) 'p, t1, t2 = ', p, t1, t2
p => t2
WRITE (*,*) 'p, t1, t2 = ', p, t1, t2
END PROGRAM test_ptr
