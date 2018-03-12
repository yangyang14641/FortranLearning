PROGRAM test_ptr4
IMPLICIT NONE
REAL, POINTER :: p1, p2, p3
REAL, TARGET :: a = 11., b = 12.5, c
NULLIFY ( p1, p2, p3)     ! Nullify pointers
p1 => a                   ! p1 points to a
p2 => b                   ! p2 points to b
p3 => c                   ! p3 points to c
p3 = p1 + p2              ! Same as c = a + b
WRITE (*,*) 'p3 = ', p3
p2 => p1                  ! p2 points to a
p3 = p1 + p2              ! Same as c = a + a
WRITE (*,*) 'p3 = ', p3
p3 = p1                   ! Same as c = a
p3 => p1                  ! p3 points to a
WRITE (*,*) 'p3 = ', p3
WRITE (*,*) 'a, b, c = ', a, b, c
END PROGRAM test_ptr4
