PROGRAM bad_array
!
!  Purpose:
!    To illustrate how a subroutine can modify variables not
!    in its argument list if variables and arrays are mismatched
!    in the argument list.
!
IMPLICIT NONE

! Local variables.
REAL, DIMENSION(2) :: X = (/ 1., 2. /)
REAL, DIMENSION(3) :: Y = (/ 3., 4., 5. /)

! Write out X, Y, Z before call to bad_subroutine.
WRITE (*,100) 'Before: X = ', X, ' Y = ', Y
100 FORMAT (1X,A,2F10.2,A,3F10.2)
 
! Call subroutine bad_subroutine.
CALL bad_subroutine ( X )
 
! Write out X, Y, Z after call to bad_subroutine.
WRITE (*,100) 'After:  X = ', X, ' Y = ', Y
 
END PROGRAM


SUBROUTINE bad_subroutine ( X )
IMPLICIT NONE
INTEGER I
REAL :: X(3)
 
DO I = 1, 3
   X(I) = -REAL(I)
END DO
 
END SUBROUTINE
