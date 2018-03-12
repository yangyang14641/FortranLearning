REAL, POINTER :: p1
REAL :: x1 = 11.
INTEGER, POINTER :: p2
INTEGER :: x2 = 12
p1 => x1
p2 => x2
WRITE (*,'(A,4G8.2)') ' p1, p2, t1, t2 = ', p1, p2, t1, t2
p1 => p2
p2 => t1
WRITE (*,'(A,4G8.2)') ' p1, p2, t1, t2 = ', p1, p2, t1, t2
END PROGRAM
