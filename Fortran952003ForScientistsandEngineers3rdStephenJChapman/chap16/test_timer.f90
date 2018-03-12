PROGRAM test_timer
!
!  This program tests the timer class.
!
!  Record of revisions:
!      Date       Programmer          Description of change
!      ====       ==========          =====================
!    12/27/06    S. J. Chapman        Original code
!
USE timer_class                 ! Import timer class
IMPLICIT NONE

! Declare local variables
INTEGER :: i, j                 ! Loop index
INTEGER :: k                    ! Scratch variable
TYPE(timer) :: t                ! Timer object

! Reset the timer
CALL t%start_timer()

! Waste some time
DO i = 1, 10000
   DO j = 1, 10000
      k = i + j
   END DO
END DO

! Get the elapsed time
WRITE (*,'(A,F8.3,A)') 'Time =', t%elapsed_time(), ' s'
   
END PROGRAM test_timer
