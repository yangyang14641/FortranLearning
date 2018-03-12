PROGRAM square_and_cube_roots
!
!  Purpose:
!    To calculate a table of numbers, square roots, and cube roots
!    using an implied DO loop to output the table.
!
!  Record of revisions:
!      Date       Programmer          Description of change
!      ====       ==========          =====================
!    11/15/06    S. J. Chapman        Original code
!
IMPLICIT NONE

! Data dictionary: declare constants
INTEGER, PARAMETER :: MAX_SIZE = 10      ! Max values in array

! Data dictionary: declare variable types, definitions, & units
INTEGER :: j                             ! Loop index
REAL, DIMENSION(MAX_SIZE) :: value       ! Array of numbers
REAL, DIMENSION(MAX_SIZE) :: square_root ! Array of square roots
REAL, DIMENSION(MAX_SIZE) :: cube_root   ! Array of cube roots

! Calculate the square roots & cube roots of the numbers.
DO j = 1, MAX_SIZE
   value(j) = real(j)
   square_root(j) = sqrt(value(j))
   cube_root(j) = value(j)**(1.0/3.0)
END DO
 
! Write out each number, its square root, and its cube root.
WRITE (*,100) 
100 FORMAT ('0',20X,'Table of Square and Cube Roots',/, &
             4X,'  Number    Square Root  Cube Root', &
             3X,'  Number    Square Root  Cube Root',/, &
             4X,'  ======    ===========  =========', &
             3X,'  ======    ===========  =========')
WRITE (*,110) (value(j), square_root(j), cube_root(j), j = 1, MAX_SIZE)
110 FORMAT (2(4X,F6.0,9X,F6.4,6X,F6.4))
 
END PROGRAM square_and_cube_roots

