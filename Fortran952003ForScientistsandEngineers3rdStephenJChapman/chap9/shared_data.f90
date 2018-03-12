MODULE shared_data
!
!  Purpose:
!    To declare data to share between two routines.

IMPLICIT NONE
SAVE

INTEGER, PARAMETER :: num_vals = 5       ! Max number of values in array
REAL, DIMENSION(num_vals) :: values      ! Data values

END MODULE shared_data
