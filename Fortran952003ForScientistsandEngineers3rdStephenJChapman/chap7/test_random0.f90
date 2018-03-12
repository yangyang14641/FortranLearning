PROGRAM test_random0
!
!  Purpose:
!    Subroutine test the random number generator random0. 
!
!  Record of revisions:
!      Date       Programmer          Description of change
!      ====       ==========          =====================
!    11/22/06    S. J. Chapman        Original code
!
IMPLICIT NONE

! Data dictionary: declare variable types & definitions
REAL :: ave          ! Average of random numbers
INTEGER :: i         ! DO loop index
INTEGER :: iseed     ! Seed for random number sequence
INTEGER :: iseq      ! DO loop index
REAL :: ran          ! A random number
REAL :: sum          ! Sum of random numbers

! Get seed.
WRITE (*,*) 'Enter seed: '
READ (*,*) iseed
 
! Set seed.
CALL SEED ( iseed )
 
! Print out 10 random numbers.
WRITE (*,*) '10 random numbers: '
DO i = 1, 10
   CALL random0 ( ran )
   WRITE (*,'(3X,F16.6)') ran
END DO

! Average 5 consecutive 1000-value sequences.
WRITE (*,*) 'Averages of 5 consecutive 1000-sample sequences:'
DO iseq = 1, 5
   sum = 0.
   DO i = 1, 1000
      CALL random0 ( ran )
      sum = sum + ran
   END DO
   ave = sum / 1000.
   WRITE (*,'(3X,F16.6)') ave
END DO
 
END PROGRAM test_random0
