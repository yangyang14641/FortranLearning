PROGRAM ptr_array
IMPLICIT NONE
TYPE :: ptr
   REAL, DIMENSION(:), POINTER :: p
END TYPE
TYPE (ptr), DIMENSION(3) :: p1
REAL, DIMENSION(4), TARGET :: a = (/ 1.,  2.,  3.,  4. /)
REAL, DIMENSION(4), TARGET :: b = (/ 5.,  6.,  7.,  8. /)
REAL, DIMENSION(4), TARGET :: c = (/ 9., 10., 11., 12. /)
p1(1)%p => a
p1(2)%p => b
p1(3)%p => c
WRITE (*,*) p1(3)%p
WRITE (*,*) p1(2)%p(3)
END PROGRAM ptr_array
