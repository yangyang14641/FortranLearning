PROGRAM array2
!
!  Purpose:
!    To illustrate the effect of accessing an out-of-bounds
!    array element.
!
!  Record of revisions:
!      Date       Programmer          Description of change
!      ====       ==========          =====================
!    11/19/06    S. J. Chapman        Original code
!
IMPLICIT NONE

! Declare the and initialize the variables used in this program.
INTEGER :: i                                 ! Loop index
REAL, DIMENSION(5) :: a = 0.                 ! Array

! Call subroutine sub1.
CALL sub1( a, 5, 6 )

! Write out the values of array a 
DO i = 1, 6
   WRITE (6,100) i, a(i)
   100 FORMAT ( 1X,'a(', I1, ') = ', F6.2 )
END DO

END PROGRAM array2

!*****************************************************************
!*****************************************************************

SUBROUTINE sub1 ( a, ndim, n )
IMPLICIT NONE

INTEGER, INTENT(IN) :: ndim             ! size of array
REAL, INTENT(OUT), DIMENSION(ndim) :: a ! Dummy argument
INTEGER, INTENT(IN) :: n                ! # elements to process
INTEGER :: i                            ! Loop index

DO i = 1, n
   a(i) = i
END DO

END SUBROUTINE sub1
