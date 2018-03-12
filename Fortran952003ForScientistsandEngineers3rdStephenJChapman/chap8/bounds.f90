PROGRAM bounds
!
!  Purpose:
!    To illustrate the effect of accessing an out-of-bounds
!    array element.
!
!  Record of revisions:
!      Date       Programmer          Description of change
!      ====       ==========          =====================
!    06/24/02    S. J. Chapman        Original code
!
IMPLICIT NONE

! Declare the and initialize the variables used in this program.

INTEGER :: i                ! Loop index
REAL, DIMENSION(5) :: a = (/ 1., 2., 3., 4., 5./) 
REAL, DIMENSION(5) :: b = (/10.,20.,30.,40.,50./) 
 
! Write out the values of array a 
DO i = 1, 6
   WRITE (*,100) i, a(i)
   100 FORMAT ( 1X,'a(', I1, ') = ', F6.2 )
END DO

END PROGRAM
