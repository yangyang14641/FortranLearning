PROGRAM test_ptr2
IMPLICIT NONE
REAL, POINTER :: p1, p2
REAL, TARGET :: t1 = 10., t2 = -17.
p1 => t1
p2 => p1
WRITE (*,'(A,4F8.2)') ' p1, p2, t1, t2 = ', p1, p2, t1, t2
p1 => t2
WRITE (*,'(A,4F8.2)') ' p1, p2, t1, t2 = ', p1, p2, t1, t2
END PROGRAM test_ptr2
