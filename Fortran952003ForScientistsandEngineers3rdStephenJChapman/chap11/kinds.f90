PROGRAM kinds
!
!  Purpose: 
!    To determine the kinds of single and double precision real
!    values on a particular computer.
! 
IMPLICIT NONE

! Write out the kinds of single & double precision values
WRITE (*,'(" The KIND for single precision is",I2)') KIND(0.0)
WRITE (*,'(" The KIND for double precision is",I2)') KIND(0.0D0)
 
END PROGRAM kinds
