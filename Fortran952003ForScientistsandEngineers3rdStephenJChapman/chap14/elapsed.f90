MODULE save_timer
!
!  Purpose:
!    To hold the time from the call to set_timer for use 
!    in calls to subroutine elapsed_time. 
!
IMPLICIT NONE
INTEGER,SAVE,DIMENSION(8) :: t1  ! Time of call to set_timer
INTEGER,SAVE,DIMENSION(8) :: t2  ! Time of call to elapsed_time
END MODULE save_timer

SUBROUTINE set_timer
!
!  Purpose:
!    To start (or reset) the elapsed time counter.  Elapsed
!    time is returned by subsequent calls to subroutine 
!    elapsed_time.
!
!  Record of revisions:
!      Date       Programmer          Description of change
!      ====       ==========          =====================
!    11/22/06    S. J. Chapman        Original code
!
!  List of calling arguments:
!     none
!
USE save_timer
IMPLICIT NONE

! Get current time.
CALL DATE_AND_TIME ( VALUES=t1 )

END SUBROUTINE set_timer

SUBROUTINE elapsed_time ( sec )
!
!  Purpose:
!    To return the time in seconds since the last call
!    to subroutine set_timer. 
!
!  Record of revisions:
!      Date       Programmer          Description of change
!      ====       ==========          =====================
!    11/22/06    S. J. Chapman        Original code
!
!  List of calling arguments:
!     none
!
USE save_timer
IMPLICIT NONE

! Dummy arguments
REAL :: sec              ! Elapsed time in seconds

! Local variables
INTEGER(KIND=SELECTED_INT_KIND(9)) :: msec

! Get current time.
CALL DATE_AND_TIME ( VALUES=t2 )

! Now get the elapsed time in milliseconds.
msec = (3600000*t2(5) + 60000*t2(6) + 1000*t2(7) + t2(8) ) &
     - (3600000*t1(5) + 60000*t1(6) + 1000*t1(7) + t1(8) )

! Convert to floating point seconds and return result.
sec = 0.001 * REAL(msec)

END SUBROUTINE elapsed_time
