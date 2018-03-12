PROGRAM test_generic_procedures
!
!  Purpose:
!    To test generic bound procedures.
!
!  Record of revisions:
!       Date       Programmer          Description of change
!       ====       ==========          =====================
!     12/27/06    S. J. Chapman        Original code
!
USE generic_procedure_module 
IMPLICIT NONE

! Enter first point
TYPE(vector) :: v1              ! First vector
TYPE(vector) :: v2              ! Second vector
REAL :: s                       ! Scalar

! Get the first vector
WRITE (*,*) 'Enter the first vector (x,y):'
READ (*,*) v1%x, v1%y

! Get the second vector
WRITE (*,*) 'Enter the second vector (x,y):'
READ (*,*) v2%x, v2%y

! Get a scalar
WRITE (*,*) 'Enter a scalar:'
READ (*,*) s

! Add the vectors
WRITE (*,1000) v1%add(v2)
1000 FORMAT(1X,'The sum of the vectors is (',F8.2,',',F8.2,')')

! Subtract the points
WRITE (*,1010) v1%add(s)
1010 FORMAT(1X,'The sum of the vector and scalar is (',F8.2,',',F8.2,')')

END PROGRAM test_generic_procedures
