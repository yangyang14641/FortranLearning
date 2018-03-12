MODULE timer_class1
!
!  This module implements a timer class.
!
!  Record of revisions:
!      Date       Programmer          Description of change
!      ====       ==========          =====================
!    12/27/06    S. J. Chapman        Original code
!
IMPLICIT NONE

! Declare constants
INTEGER,PARAMETER :: DBL = SELECTED_REAL_KIND(p=14)

! Type definition
TYPE,PUBLIC :: timer   ! This will be the name we instantiate

   ! Instance variables
   PRIVATE
   REAL(KIND=DBL) :: saved_time  ! Saved time in ms
   
!CONTAINS

   ! Bound procedures 
!   PROCEDURE :: start_timer ==> start_timer_sub
!   PROCEDURE :: elapsed_time ==> elapsed_time_fn
   
END TYPE timer

! Restrict access to actual procedure names
!PRIVATE :: start_timer_sub, elapsed_time_fn

! Now add methods
CONTAINS

   SUBROUTINE start_timer_sub(this)
   ! 
   ! Subroutine to get and save the initial time
   !
   IMPLICIT NONE
   
   ! Declare calling arguments
   TYPE(timer) :: this              ! Timer object
   
   ! Declare local variables
   INTEGER,DIMENSION(8) :: value     ! Time value array
   
   ! Get time
   CALL date_and_time ( VALUES=value )
   this%saved_time = 86400.D0 * value(3) + 3600.D0 * value(5) &
                   + 60.D0 * value(6) + value(7) + 0.001D0 * value(8)
                   
   END SUBROUTINE start_timer_sub


   REAL FUNCTION elapsed_time_fn(this)
   ! 
   ! Function to calculate elapsed time
   !
   IMPLICIT NONE
   
   ! Declare calling arguments
   TYPE(timer) :: this              ! Timer object
   
   ! Declare local variables
   INTEGER,DIMENSION(8) :: value     ! Time value array
   REAL(KIND=DBL) :: current_time    ! Current time (ms)
   
   ! Get time
   CALL date_and_time ( VALUES=value )
   current_time = 86400.D0 * value(3) + 3600.D0 * value(5) &
                + 60.D0 * value(6) + value(7) + 0.001D0 * value(8)
                
   ! Get elapsed time in seconds
   elapsed_time_fn = current_time - this%saved_time
                   
   END FUNCTION elapsed_time_fn
   
END MODULE timer_class1

PROGRAM test_timer1
!
!  This program test the timer class.
!
!  Record of revisions:
!      Date       Programmer          Description of change
!      ====       ==========          =====================
!    12/27/06    S. J. Chapman        Original code
!
USE timer_class1                ! Import timer class
IMPLICIT NONE

! Declare local variables
INTEGER :: i, j                 ! Loop index
INTEGER :: k                    ! Scratch variable
TYPE(timer) :: t                ! Timer object

! Reset the timer
CALL start_timer_sub(t)

! Waste some time
DO i = 1, 10000
   DO j = 1, 10000
      k = SIN(3.14)
   END DO
END DO

! Get the elapsed time
WRITE (*,'(A,F8.3,A)') 'Time =', elapsed_time_fn(t), ' s'
   
END PROGRAM test_timer1
