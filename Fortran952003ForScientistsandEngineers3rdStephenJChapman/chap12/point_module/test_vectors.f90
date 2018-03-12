PROGRAM test_vectors
!
!  Purpose:
!    To test adding and subtracting 2D vectors.
!
!  Record of revisions:
!       Date       Programmer          Description of change
!       ====       ==========          =====================
!     12/04/06    S. J. Chapman        Original code
! 1.  12/22/06    S. J. Chapman        Use bound procedures
!
USE vector_module 
IMPLICIT NONE

! Enter first point
TYPE(vector) :: v1              ! First point
TYPE(vector) :: v2              ! Second point

! Get the first vector
WRITE (*,*) 'Enter the first vector (x,y):'
READ (*,*) v1%x, v1%y

! Get the second point
WRITE (*,*) 'Enter the second vector (x,y):'
READ (*,*) v2%x, v2%y

! Add the points
WRITE (*,1000) v1%vector_add(v2)
1000 FORMAT(1X,'The sum of the points is (',F8.2,',',F8.2,')')

! Subtract the points
WRITE (*,1010) v1%vector_sub(v2)
1010 FORMAT(1X,'The difference of the points is (',F8.2,',',F8.2,')')

END PROGRAM test_vectors
