PROGRAM test_vectors
!
!  Purpose:
!    To test the definitions, operations, and assignments 
!    associated with the vector data type.
!
!  Record of revisions:
!      Date       Programmer          Description of change
!      ====       ==========          =====================
!    12/15/06    S. J. Chapman        Original code
!
USE vectors
IMPLICIT NONE

! Data dictionary: declare variable types & definitions
REAL, DIMENSION(3) :: array_out         ! Output array
TYPE (vector) :: vec_1, vec_2           ! Test vectors

! Test assignments by assigning an array to vec_1 and 
! assigning vec_1 to array_out.
vec_1 = (/ 1., 2., 3. /)
array_out = vec_1
WRITE (*,1000) vec_1, array_out
1000 FORMAT (' Test assignments: ',/, &
             ' vec_1 =     ', 3F8.2,/, &
             ' array_out = ', 3F8.2)

! Test addition and subtraction.
vec_1 = (/ 10., 20., 30. /)
vec_2 = (/ 1., 2., 3. /)
WRITE (*,1010) vec_1, vec_2, vec_1 + vec_2, vec_1 - vec_2
1010 FORMAT (/' Test addition and subtraction: ',/, &
              ' vec_1 =         ', 3F8.2,/, &
              ' vec_2 =         ', 3F8.2,/, &
              ' vec_1 + vec_2 = ', 3F8.2,/, &
              ' vec_1 - vec_2 = ', 3F8.2)

! Test multiplication by a scalar.
vec_1 = (/ 1., 2., 3. /)
WRITE (*,1020) vec_1, 2.*vec_1, vec_1*2., 2*vec_1, vec_1*2
1020 FORMAT (/' Test multiplication by a scalar: ',/, &
              ' vec_1 =         ', 3F8.2,/, &
              ' 2. * vec_1 =    ', 3F8.2,/, &
              ' vec_1 * 2. =    ', 3F8.2,/, &
              ' 2 * vec_1  =    ', 3F8.2,/, &
              ' vec_1 * 2  =    ', 3F8.2)

! Test division by a scalar.
vec_1 = (/ 10., 20., 30. /)
WRITE (*,1030) vec_1, vec_1/5., vec_1/5
1030 FORMAT (/' Test division by a scalar: ',/, &
              ' vec_1 =         ', 3F8.2,/, &
              ' vec_1 / 5. =    ', 3F8.2,/, &
              ' vec_1 / 5 =     ', 3F8.2)

! Test dot product.
vec_1 = (/ 1., 2., 3. /)
vec_2 = (/ 1., 2., 3. /)
WRITE (*,1040) vec_1, vec_2, vec_1 .DOT. vec_2
1040 FORMAT (/' Test dot product: ',/, &
              ' vec_1 =             ', 3F8.2,/, &
              ' vec_2 =             ', 3F8.2,/, &
              ' vec_1 .DOT. vec_2 = ', 3F8.2)
 
! Test cross product.
vec_1 = (/ 1., -1., 1. /)
vec_2 = (/ -1., 1., 1. /)
WRITE (*,1050) vec_1, vec_2, vec_1*vec_2
1050 FORMAT (/' Test cross product: ',/, &
              ' vec_1 =         ', 3F8.2,/, &
              ' vec_2 =         ', 3F8.2,/, &
              ' vec_1 * vec_2 = ', 3F8.2)

END PROGRAM test_vectors
