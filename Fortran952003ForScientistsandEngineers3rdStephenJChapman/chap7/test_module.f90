PROGRAM test_module
!
!  Purpose:
!    To illustrate sharing data via a module.
!
USE shared_data               ! Make data in module "test" visible
IMPLICIT NONE

REAL, PARAMETER :: PI = 3.141592  ! Pi

values = PI * (/ 1., 2., 3., 4., 5. /)

CALL sub1                     ! Call subroutine

END PROGRAM test_module

!*****************************************************************
!*****************************************************************

SUBROUTINE sub1
!
!  Purpose:
!    To illustrate sharing data via a module.
!
USE shared_data               ! Make data in module "test" visible
IMPLICIT NONE

WRITE (*,*) values

END SUBROUTINE sub1
