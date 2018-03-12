PROGRAM array_ptr
IMPLICIT NONE
INTEGER :: i
INTEGER, DIMENSION(16), TARGET :: info = (/ (i, i=1,16) /)
INTEGER, DIMENSION(:), POINTER :: ptr1, ptr2, ptr3, ptr4, ptr5
ptr1 => info
ptr2 => ptr1(2::2)
ptr3 => ptr2(2::2)
ptr4 => ptr3(2::2)
ptr5 => ptr4(2::2)
WRITE (*,'(A,16I3)') ' ptr1 = ', ptr1
WRITE (*,'(A,16I3)') ' ptr2 = ', ptr2
WRITE (*,'(A,16I3)') ' ptr3 = ', ptr3
WRITE (*,'(A,16I3)') ' ptr4 = ', ptr4
WRITE (*,'(A,16I3)') ' ptr5 = ', ptr5
END PROGRAM array_ptr
