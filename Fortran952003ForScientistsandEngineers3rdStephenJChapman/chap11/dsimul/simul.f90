SUBROUTINE simul ( a, b, soln, ndim, n, error )
!
!  Purpose:
!    Subroutine to solve a set of N linear equations in N 
!    unknowns using Gaussian elimination and the maximum 
!    pivot technique.  This version of simul has been 
!    modified to use array sections and allocatable arrays
!    It DOES NOT DESTROY the original input values.
!
!  Record of revisions:
!      Date       Programmer          Description of change
!      ====       ==========          =====================
!    11/23/06    S. J. Chapman        Original code
! 1. 11/24/06    S. J. Chapman        Add automatic arrays
!
IMPLICIT NONE

! Data dictionary: declare calling parameter types & definitions
INTEGER, INTENT(IN) :: ndim          ! Dimension of arrays a and b
REAL, INTENT(IN), DIMENSION(ndim,ndim) :: a 
                                     ! Array of coefficients (N x N).
                                     ! This array is of size ndim x 
                                     ! ndim, but only N x N of the 
                                     ! coefficients are being used.
REAL, INTENT(IN), DIMENSION(ndim) :: b 
                                     ! Input: Right-hand side of eqns.
REAL, INTENT(OUT), DIMENSION(ndim) :: soln 
                                     ! Output: Solution vector.
INTEGER, INTENT(IN) :: n             ! Number of equations to solve.
INTEGER, INTENT(OUT) :: error        ! Error flag:
                                     !   0 -- No error
                                     !   1 -- Singular equations

! Data dictionary: declare constants
REAL, PARAMETER :: EPSILON = 1.0E-6  ! A "small" number for comparison
                                     ! when determining singular eqns 

! Data dictionary: declare local variable types & definitions
REAL, DIMENSION(n,n) :: a1           ! Copy of "a" which will be
                                     ! destroyed during the solution
REAL :: factor                       ! Factor to multiply eqn irow by
                                     ! before adding to eqn jrow
INTEGER :: irow                      ! Number of the equation currently
                                     ! being processed
INTEGER :: ipeak                     ! Pointer to equation containing
                                     ! maximum pivot value
INTEGER :: jrow                      ! Number of the equation compared
                                     ! to the current equation
REAL :: temp                         ! Scratch value
REAL, DIMENSION(n) :: temp1          ! Scratch array

! Make copies of arrays "a" and "b" for local use
a1 = a(1:n,1:n)
soln = b(1:n)

! Process N times to get all equations...
mainloop: DO irow = 1, n
 
   ! Find peak pivot for column irow in rows irow to N
   ipeak = irow
   max_pivot: DO jrow = irow+1, n
      IF (ABS(a1(jrow,irow)) > ABS(a1(ipeak,irow))) THEN
         ipeak = jrow
      END IF
   END DO max_pivot
 
   ! Check for singular equations.  
   singular: IF ( ABS(a1(ipeak,irow)) < EPSILON ) THEN
      error = 1
      RETURN
   END IF singular
 
   ! Otherwise, if ipeak /= irow, swap equations irow & ipeak
   swap_eqn: IF ( ipeak /= irow ) THEN
      temp1 = a1(ipeak,1:n)            
      a1(ipeak,1:n) = a1(irow,1:n)   ! Swap rows in a
      a1(irow,1:n) = temp1             
      temp = soln(ipeak)           
      soln(ipeak) = soln(irow)       ! Swap rows in b
      soln(irow)  = temp               
   END IF swap_eqn
 
   ! Multiply equation irow by -a1(jrow,irow)/a1(irow,irow), 
   ! and add it to Eqn jrow (for all eqns except irow itself).
   eliminate: DO jrow = 1, n
      IF ( jrow /= irow ) THEN
         factor = -a1(jrow,irow)/a1(irow,irow)
         a1(jrow,:) = a1(irow,1:n)*factor + a1(jrow,1:n)
         soln(jrow) = soln(irow)*factor + soln(jrow)
      END IF
   END DO eliminate
END DO mainloop
   
! End of main loop over all equations.  All off-diagonal terms
! are now zero.  To get the final answer, we must divide
! each equation by the coefficient of its on-diagonal term.
divide: DO irow = 1, n
   soln(irow) = soln(irow) / a1(irow,irow)
   a1(irow,irow) = 1.
END DO divide
 
! Set error flag to 0 and return.
error = 0

END SUBROUTINE simul

