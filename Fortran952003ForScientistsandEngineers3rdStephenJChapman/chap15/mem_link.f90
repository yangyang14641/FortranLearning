PROGRAM mem_leak
IMPLICIT NONE
INTEGER :: i, istat
INTEGER, DIMENSION(:), POINTER :: ptr1, ptr2

! Check associated status of ptrs. 
WRITE (*,'(A,2L5)') ' Are ptr1, ptr2 associated? ', &
      ASSOCIATED(ptr1), ASSOCIATED(ptr2)

! Allocate and initialize memory
ALLOCATE (ptr1(1:10), STAT=istat)
ALLOCATE (ptr2(1:10), STAT=istat)
ptr1 = (/ (i, i = 1,10 ) /)
ptr2 = (/ (i, i = 11,20 ) /)

! Check associated status of ptrs.
WRITE (*,'(A,2L5)') ' Are ptr1, ptr2 associated? ', &
      ASSOCIATED(ptr1), ASSOCIATED(ptr2)

WRITE (*,'(A,10I3)') ' ptr1 = ', ptr1   ! Write out data
WRITE (*,'(A,10I3)') ' ptr2 = ', ptr2

ptr2 => ptr1                            ! Reassign ptr2

WRITE (*,'(A,10I3)') ' ptr1 = ', ptr1   ! Write out data
WRITE (*,'(A,10I3)') ' ptr2 = ', ptr2

NULLIFY(ptr1)                           ! Nullify pointer      
DEALLOCATE(ptr2, STAT=istat)            ! Deallocate memory

END PROGRAM mem_leak
