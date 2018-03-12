PROGRAM select_kinds
!
!  Purpose: 
!    To illustrate the use of SELECTED_REAL_KIND to select
!    desired kinds of real variables in a processor-independent
!    manner.
! 
!  Record of revisions:
!       Date       Programmer          Description of change
!       ====       ==========          =====================
!     11/27/06    S. J. Chapman        Original code
!
IMPLICIT NONE

! Declare parameters:
INTEGER, PARAMETER :: SGL = SELECTED_REAL_KIND(p=6,r=37)
INTEGER, PARAMETER :: DBL = SELECTED_REAL_KIND(p=13,r=200)

! Declare variables of each type:
REAL(kind=SGL) :: var1 = 0.
REAL(kind=DBL) :: var2 = 0._DBL

! Write characteristics of selected variables.
WRITE (*,100) 'var1', KIND(var1), PRECISION(var1), RANGE(var1)
WRITE (*,100) 'var2', KIND(var2), PRECISION(var2), RANGE(var2)
100 FORMAT(1X,A,': kind = ',I2,', Precision = ',I2,', Range = ',I3)

END PROGRAM
