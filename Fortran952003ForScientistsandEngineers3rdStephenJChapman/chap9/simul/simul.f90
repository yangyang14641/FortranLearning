SUBROUTINE simul ( a, b, ndim, n, error )
!
!  Purpose:
!    Subroutine to solve a set of n linear equations in n 
!    unknowns using Gaussian elimination and the maximum 
!    pivot technique.
!
!  Record of revisions:
!      Date       Programmer          Description of change
!      ====       ==========          =====================
!    11/23/06    S. J. Chapman        Original code
!
IMPLICIT NONE

! Data dictionary: declare calling parameter types & definitions
INTEGER, INTENT(IN) :: ndim          ! Dimension of arrays a and b
REAL, INTENT(INOUT), DIMENSION(ndim,ndim) :: a 
                                     ! Array of coefficients (n x n).
                                     ! This array is of size ndim x 
                                     ! ndim, but only n x n of the 
                                     ! coefficients are being used.
                                     ! The declared dimension ndim 
                                     ! must be passed to the sub, or
                                     ! it won't be able to interpret
                                     ! subscripts correctly.  (This
                                     ! array is destroyed during
                                     ! processing.)
REAL, INTENT(INOUT), DIMENSION(ndim) :: b 
                                     ! Input: Right-hand side of eqns.
                                     ! Output: Solution vector.
INTEGER, INTENT(IN) :: n             ! Number of equations to solve.
INTEGER, INTENT(OUT) :: error        ! Error flag:
                                     !   0 -- No error
                                     !   1 -- Singular equations


! Data dictionary: declare constants
REAL, PARAMETER :: EPSILON = 1.0E-6  ! A "small" number for comparison
                                     ! when determining singular eqns 

! Data dictionary: declare local variable types & definitions
REAL :: factor                       ! Factor to multiply eqn irow by
                                     ! before adding to eqn jrow
INTEGER :: irow                      ! Number of the equation currently
                                     ! being processed
INTEGER :: ipeak                     ! Pointer to equation containing
                                     ! maximum pivot value
INTEGER :: jrow                      ! Number of the equation compared
                                     ! to the current equation
INTEGER :: kcol                      ! Index over all columns of eqn
REAL :: temp                         ! Scratch value

! Process n times to get all equations...
mainloop: DO irow = 1, n
 
   ! Find peak pivot for column irow in rows irow to n
   ipeak = irow
   max_pivot: DO jrow = irow+1, n
      IF (ABS(a(jrow,irow)) > ABS(a(ipeak,irow))) THEN
         ipeak = jrow
      END IF
   END DO max_pivot
 
   ! Check for singular equations.  
   singular: IF ( ABS(a(ipeak,irow)) < EPSILON ) THEN
      error = 1
      RETURN
   END IF singular
 
   ! Otherwise, if ipeak /= irow, swap equations irow & ipeak
   swap_eqn: IF ( ipeak /= irow ) THEN
      DO kcol = 1, n
         temp          = a(ipeak,kcol)
         a(ipeak,kcol) = a(irow,kcol)
         a(irow,kcol)  = temp 
      END DO
      temp     = b(ipeak)
      b(ipeak) = b(irow)
      b(irow)  = temp 
   END IF swap_eqn
 
   ! Multiply equation irow by -a(jrow,irow)/a(irow,irow),  
   ! and add it to Eqn jrow (for all eqns except irow itself).
   eliminate: DO jrow = 1, n
      IF ( jrow /= irow ) THEN
         factor = -a(jrow,irow)/a(irow,irow)
         DO kcol = 1, n
            a(jrow,kcol) = a(irow,kcol)*factor + a(jrow,kcol)
         END DO
         b(jrow) = b(irow)*factor + b(jrow)
      END IF
   END DO eliminate
END DO mainloop
   
! End of main loop over all equations.  All off-diagonal
! terms are now zero.  To get the final answer, we must
! divide each equation by the coefficient of its on-diagonal
! term.
divide: DO irow = 1, n
   b(irow)      = b(irow) / a(irow,irow)
   a(irow,irow) = 1.
END DO divide
 
! Set error flag to 0 and return.
error = 0
END SUBROUTINE simul

