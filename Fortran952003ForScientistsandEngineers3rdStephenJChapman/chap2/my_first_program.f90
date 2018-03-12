PROGRAM my_first_program
 
!  Purpose: 
!    To illustrate some of the basic features of a Fortran program.
!

! Declare the variables used in this program.
INTEGER :: i, j, k             ! All variables are integers

! Get two values to store in variables i and j
WRITE (*,*) 'Enter the numbers to multiply: '
READ (*,*) i, j

! Multiply the numbers together
k = i * j
 
!  Write out the result.
WRITE (*,*) 'Result = ', k
 
!  Finish up.
STOP
END PROGRAM my_first_program
