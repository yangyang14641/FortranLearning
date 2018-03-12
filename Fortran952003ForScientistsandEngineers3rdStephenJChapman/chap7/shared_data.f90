MODULE shared_data
!
!  Purpose:
!    To declare data to share between two routines.

IMPLICIT NONE
SAVE

INTEGER, PARAMETER :: NUM_VALS = 5       ! Max number of values in array
REAL, DIMENSION(NUM_VALS) :: values      ! Data values

END MODULE shared_data
