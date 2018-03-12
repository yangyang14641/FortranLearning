PROGRAM junk
REAL :: lamda
WRITE (*,*) 'Enter lamda: '
READ (*,*) lamda
DO k = 0, 10
   WRITE (*,100) k, poisson(k, 1., lamda )
   100 FORMAT (1x,I3,3x,F12.7)
END DO
END PROGRAM

FUNCTION poisson( k, t, lamda )
!
!  Purpose:
!    To calculate a sample value from the poisson 
!    distribution for specific values of k, t, and lamda.
!
!  Record of revisions:
!      Date       Programmer          Description of change
!      ====       ==========          =====================
!    01/06/97    S. J. Chapman        Original code 
!
IMPLICIT NONE

! List of calling arguments:
INTEGER,INTENT(IN) :: k          
INTEGER,INTENT(IN) :: t          
INTEGER,INTENT(IN) :: lamda
REAL :: poisson                 ! Sample of distribution

! List of local variables:
REAL :: fact                    ! Factorial function
INTEGER :: i                    ! Loop index

! Calculate k!
fact = 1.
DO i = 2, k
   fact = fact * k
END DO

! Calculate value from poission distribution.
poisson = EXP(-lamda*t) * (lamda*t)**k / fact

END FUNCTION poisson

 