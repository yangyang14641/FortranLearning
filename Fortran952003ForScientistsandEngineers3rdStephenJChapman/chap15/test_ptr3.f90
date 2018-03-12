PROGRAM test_ptr3
IMPLICIT NONE
REAL, POINTER :: p1, p2, p3
REAL, TARGET :: a = 11., b = 12.5, c = 3.141592
NULLIFY ( p1, p2, p3)     ! Nullify pointers
WRITE (*,*) ASSOCIATED(p1)
p1 => a                   ! p1 points to a
p2 => b                   ! p2 points to b
p3 => c                   ! p3 points to c
WRITE (*,*) ASSOCIATED(p1)
WRITE (*,*) ASSOCIATED(p1,b)
END PROGRAM test_ptr3
