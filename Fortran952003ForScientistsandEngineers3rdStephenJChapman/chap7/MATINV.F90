SUBROUTINE matinv ( a, b, ndim, n, error )
!
!  Purpose:
!    Subroutine to calculate the inverse of an N x N  
!    matrix "a" using Gaussian elimination and the 
!    maximum pivot technique. 
!
!  Record of revisions:
!      Date       Programmer          Description of change
!      ====       ==========          =====================
!    03/02/96    S. J. Chapman        Original code
!
IMPLICIT NONE

! Declare calling arguments:
INTEGER, INTENT(IN) :: ndim          ! Dimension of arrays a and b
REAL, INTENT(IN), DIMENSION(ndim,ndim) :: a 
                                     ! Array to invert (N x N).
REAL, INTENT(OUT), DIMENSION(ndim,ndim) :: b 
                                     ! Inverse of a (N x N).
INTEGER, INTENT(IN) :: n             ! Size of a and b.
INTEGER, INTENT(OUT) :: error        ! Error flag:
                                     !   0 -- No error
                                     !   1 -- Singular equations

! Declare local parameters
REAL, PARAMETER :: epsilon = 1.0E-6  ! A "small" number for comparison
                                     ! when determining singular eqns 

! Declare local variables:
REAL, DIMENSION(n,n) :: a1           ! Copy of "a" which will be
                                     ! destroyed during the inversion
REAL :: factor                       ! Factor to multiply eqn irow by
                                     ! before adding to eqn jrow
INTEGER :: irow                      ! Number of the equation currently
                                     ! currently being processed
INTEGER :: ipeak                     ! Pointer to equation containing
                                     ! maximum pivot value
INTEGER :: jrow                      ! Number of the equation compared
                                     ! to the current equation
REAL, DIMENSION(n) :: temp           ! Scratch array

! Make a copy of array "a" for local use
a1 = a(1:n,1:n)

! Initialize "b" to the identity matrix
b(1:n,1:n) = 0.
DO irow = 1, n
   b(irow,irow) = 1.
END DO

! Process N times to get all rows...
mainloop: DO irow = 1, n
 
   ! Find peak pivot for column irow in rows irow to N
   ipeak = irow
   max_pivot: DO jrow = irow+1, n
      IF (ABS(a1(jrow,irow)) > ABS(a1(ipeak,irow))) THEN
         ipeak = jrow
      END IF
   END DO max_pivot
 
   ! Check for singular equations.  
   singular: IF ( ABS(a1(ipeak,irow)) < epsilon ) THEN
      error = 1
      RETURN
   END IF singular
 
   ! Otherwise, if ipeak /= irow, swap rows irow & ipeak
   swap_eqn: IF ( ipeak /= irow ) THEN
      temp = a1(ipeak,1:n)            
      a1(ipeak,1:n) = a1(irow,1:n)   ! Swap rows in a
      a1(irow,1:n) = temp              
      temp = b(ipeak,1:n)            
      b(ipeak,1:n) = b(irow,1:n)     ! Swap rows in b
      b(irow,1:n) = temp              
   END IF swap_eqn
 
   ! Multiply equation irow by -a1(jrow,irow)/a1(irow,irow), 
   ! and add it to Eqn jrow (for all eqns except irow itself).
   eliminate: DO jrow = 1, n
      IF ( jrow /= irow ) THEN
         factor = -a1(jrow,irow)/a1(irow,irow)
         a1(jrow,:) = a1(irow,1:n)*factor + a1(jrow,1:n)
         b(jrow,:) = b(irow,1:n)*factor + b(jrow,1:n)
      END IF
   END DO eliminate
END DO mainloop
   
! End of main loop over all equations.  All off-diagonal
! terms are now zero.  To get the final answer, we must
! divide each equation by the coefficient of its on-diagonal
! term.
divide: DO irow = 1, n
   b(irow,irow)  = b(irow,irow) / a1(irow,irow)
END DO divide
 
! Set error flag to 0 and return.
error = 0

END SUBROUTINE matinv

