PROGRAM bad
IMPLICIT NONE
INTEGER :: i
INTEGER, DIMENSION(3) :: subs = (/ 1, 8, 11 /)
INTEGER, DIMENSION(16), TARGET :: info = (/ (i, i=1,16) /)
INTEGER, DIMENSION(:), POINTER :: ptr1
ptr1 => info(subs)
WRITE (*,'(A,16I3)') ' ptr1 = ', ptr1
END PROGRAM bad
