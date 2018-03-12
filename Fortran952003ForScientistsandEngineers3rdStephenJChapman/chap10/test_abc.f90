PROGRAM test_abc
!
!  Purpose: 
!    To test function abc. 
!
USE character_subs
IMPLICIT NONE

INTEGER :: n                           ! String length

WRITE(*,*) 'Enter string length:'      ! Get string length
READ (*,*) n

WRITE (*,*) 'The string is: ', abc(n)  ! Tell user

END PROGRAM test_abc
