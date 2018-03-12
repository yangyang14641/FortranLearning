MODULE my_subs
CONTAINS
   SUBROUTINE bad_argument ( i )
   IMPLICIT NONE
   INTEGER, INTENT(IN) :: i      ! Declare argument as integer.
   WRITE (*,*) ' I = ', i        ! Write out i.
   END SUBROUTINE
END MODULE my_subs

!*****************************************************************
!*****************************************************************

PROGRAM bad_call2
!
!  Purpose:
!    To illustrate misinterpreted calling arguments.
!
USE my_subs
IMPLICIT NONE
REAL :: x = 1.                ! Declare real variable x.
CALL bad_argument ( x )       ! Call subroutine.
END PROGRAM bad_call2
