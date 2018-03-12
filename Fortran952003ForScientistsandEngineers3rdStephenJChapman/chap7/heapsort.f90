SUBROUTINE heapsort ( array, n, error )
!
!  Purpose:
!     Subroutine to sort a real array. This routine uses the
!     heapsort technique.  It was based upon the examples
!     found in NUMERICAL RECIPES, by Press, Flannery, Teukolsky,
!     and Vetterling.
!
IMPLICIT NONE

! Declare local parameters
INTEGER, PARAMETER :: SGL = SELECTED_REAL_KIND(p=6) ! Precision

! Declare calling arguments
INTEGER, INTENT(IN) :: n                ! Size of array to sort
REAL(KIND=SGL), DIMENSION(n), INTENT(INOUT) :: array
                                        ! Array to sort
INTEGER, INTENT(OUT) :: error           ! Error flag:
                                        ! 0 = success
                                        ! 1 = n <= 0

! List of local variables:
INTEGER :: i                      ! Index variable
INTEGER :: ir                     ! Retirement phase pointer
INTEGER :: j                      ! Index variable
INTEGER :: l                      ! Hiring phase pointer
REAL(KIND=SGL) :: temp            ! Temp variable for swapping

! Check for error.
IF ( n <= 0 ) THEN

   ! Set error code and get out.
   error = 1

ELSE IF ( n == 1 ) THEN

   ! no sort required, but no error either.  With only one
   ! value, it's already sorted!
   error = 0

ELSE

   L  = n / 2 + 1
   ir = n
   10 CONTINUE
      IF ( l > 1 ) THEN
          l    = l - 1
          TEMP = array(L)
      ELSE
          TEMP      = array(ir)
          array(ir) = array(1)
          ir        = ir - 1
          IF ( ir == 1 ) THEN
!
!            All done.  Store final value.
!
             array(1) = TEMP
!
!            Clear error code and exit.
!
             error = 0
             GO TO 9999
!
          END IF
       END IF
       I = L
       J = L + L
!
!      Sift down TEMP to its proper level.
!
   20  IF ( J <= ir ) THEN
          IF ( J < ir ) THEN
             IF ( array(J) < array(J+1) ) J = J + 1
          END IF
          IF ( TEMP < array(J) ) THEN
             array(I) = array(J)
             I = J
             J = J + J
          ELSE
             J = ir + 1
          END IF
          GO TO 20
       END IF
       array(I) = TEMP
   GO TO 10
END IF
!
9999 CONTINUE

END SUBROUTINE heapsort
