PROGRAM bad_call
!
!  Purpose:
!    To illustrate misinterpreted calling arguments.
!
IMPLICIT NONE
REAL :: x = 1.                ! Declare real variable x.
CALL bad_argument ( x )       ! Call subroutine.
END PROGRAM bad_call

SUBROUTINE bad_argument ( i )
IMPLICIT NONE
INTEGER :: i                  ! Declare argument as integer.
WRITE (*,*) 'I = ', i         ! Write out i.
END SUBROUTINE bad_argument
