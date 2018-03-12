PROGRAM test_ucase
!
!  Purpose: 
!    To test subroutine ucase. 
!
IMPLICIT NONE
CHARACTER(len=20) string
WRITE (*,*) 'Enter test string (up to 20 characters): '
READ (*,'(A20)') string
CALL ucase(string)
WRITE (*,*) 'The shifted string is: ', string
END PROGRAM test_ucase
