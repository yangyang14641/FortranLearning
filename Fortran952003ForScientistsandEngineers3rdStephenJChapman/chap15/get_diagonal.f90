SUBROUTINE get_diagonal ( ptr_a, ptr_b, error )
!
!  Purpose:
!    To extract the diagonal elements from the rank two
!    square array pointed to by ptr_a, and store them in
!    a rank one array allocated on ptr_b.  The following
!    error conditions are defined:
!      0 -- No error.
!      1 -- ptr_a not associated on input
!      2 -- ptr_b already associated on input
!      3 -- Array on ptr_a not suqare
!      4 -- Unable to allocate memory for ptr_b   
!
!  Record of revisions:
!      Date       Programmer          Description of change
!      ====       ==========          =====================
!    12/23/06    S. J. Chapman        Original code
!
IMPLICIT NONE

! Data dictionary: declare calling parameter types & definitions
INTEGER, DIMENSION(:,:), POINTER :: ptr_a ! Ptr to square array
INTEGER, DIMENSION(:), POINTER :: ptr_b   ! Ptr to output array
INTEGER, INTENT(OUT) :: error             ! Errors flag

! Data dictionary: declare variable types & definitions
INTEGER :: i                        ! Loop counter
INTEGER :: istat                    ! Allocate status
INTEGER, DIMENSION(2) :: l_bound    ! Lower bounds on ptr_a
INTEGER, DIMENSION(2) :: u_bound    ! Upper bounds on ptr_a
INTEGER, DIMENSION(2) :: extent     ! Extent of array on ptr_a

! Check error conditions
error_1: IF ( .NOT. ASSOCIATED ( ptr_a ) ) THEN
   error = 1
ELSE IF ( ASSOCIATED ( ptr_b ) ) THEN
   error = 2
ELSE 
   ! Check for square array
   l_bound = LBOUND ( ptr_a )
   u_bound = UBOUND ( ptr_a )
   extent = u_bound - l_bound + 1
   error_3: IF ( extent(1) /= extent(2) ) THEN
      error = 3
   ELSE
      ! Everything is ok so far, allocate ptr_b.
      ALLOCATE ( ptr_b(extent(1)), STAT=istat ) 
      error_4: IF ( istat /= 0 ) THEN
         error = 4
      ELSE
         ! Everything is ok, extract diagonal.
         ok: DO i = 1, extent(1)
            ptr_b(i) = ptr_a(l_bound(1)+i-1,l_bound(2)+i-1)
         END DO ok

         ! Reset error flag.
         error = 0
      END IF error_4
   END IF error_3
END IF error_1

END SUBROUTINE get_diagonal
