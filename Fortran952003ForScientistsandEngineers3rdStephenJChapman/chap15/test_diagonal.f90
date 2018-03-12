PROGRAM test_diagonal
!
!  Purpose:
!    To test the diagonal extraction subroutine.
!
!  Record of revisions:
!      Date       Programmer          Description of change
!      ====       ==========          =====================
!    12/23/06    S. J. Chapman        Original code
!
IMPLICIT NONE

! Declare interface to subroutine diagonal: 
INTERFACE
   SUBROUTINE get_diagonal ( ptr_a, ptr_b, error )
   INTEGER, DIMENSION(:,:), POINTER :: ptr_a 
   INTEGER, DIMENSION(:), POINTER :: ptr_b  
   INTEGER, INTENT(OUT) :: error             
   END SUBROUTINE get_diagonal
END INTERFACE

! Data dictionary: declare variable types & definitions
INTEGER :: i, j, k                        ! Loop counter
INTEGER :: istat                          ! Allocate status
INTEGER, DIMENSION(:,:), POINTER :: ptr_a ! Ptr to square array
INTEGER, DIMENSION(:), POINTER :: ptr_b   ! Ptr to output array
INTEGER :: error                          ! Errors flag

! Call diagonal with nothing defined to see what happens.
CALL get_diagonal ( ptr_a, ptr_b, error )
WRITE (*,*) 'No pointers allocated: '
WRITE (*,*) ' Error = ', error

! Allocate both pointers, and call the subroutine.
ALLOCATE (ptr_a(10,10), STAT=istat )
ALLOCATE (ptr_b(10), STAT=istat )
CALL get_diagonal ( ptr_a, ptr_b, error )
WRITE (*,*) 'Both pointers allocated: '
WRITE (*,*) ' Error = ', error

! Allocate ptr_a only, but with unequal extents.
DEALLOCATE (ptr_a, STAT=istat)
DEALLOCATE (ptr_b, STAT=istat)
ALLOCATE (ptr_a(-5:5,10), STAT=istat )
CALL get_diagonal ( ptr_a, ptr_b, error )
WRITE (*,*) 'Array on ptr_a not square: '
WRITE (*,*) ' Error = ', error

! Allocate ptr_a only, initialize, and get results.
DEALLOCATE (ptr_a, STAT=istat)
ALLOCATE (ptr_a(-2:2,0:4), STAT=istat )
k = 0
DO j = 0, 4
   DO i = -2, 2
      k = k + 1                    ! Store the numbers 1 .. 25
      ptr_a(i,j) = k               ! in row order in the array
   END DO
END DO
CALL get_diagonal ( ptr_a, ptr_b, error )
WRITE (*,*) 'ptr_a allocated & square; ptr_b not allocated: '
WRITE (*,*) ' Error = ', error
WRITE (*,*) ' Diag  = ', ptr_b

END PROGRAM
