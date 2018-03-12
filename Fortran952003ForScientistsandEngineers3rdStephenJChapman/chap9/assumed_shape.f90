MODULE test_module
!  Purpose:
!    To illustrate the use of assumed-shape arrays. 
!
CONTAINS 

   SUBROUTINE test_array(array)
   IMPLICIT NONE
   REAL, DIMENSION(:,:) :: array     ! Assumed-shape array
   INTEGER :: i1, i2                 ! Bounds of first dimension
   INTEGER :: j1, j2                 ! Bounds of second dimension

   ! Get details about array.
   i1 = LBOUND(array,1)
   i2 = UBOUND(array,1)
   j1 = LBOUND(array,2)
   j2 = UBOUND(array,2)
   WRITE (*,100) i1, i2, j1, j2
   100 FORMAT (1X,'The bounds are: (',I2,':',I2,',',I2,':',I2,')')
   WRITE (*,110) SHAPE(array)
   110 FORMAT (1X,'The shape is:    ',2I4)
   WRITE (*,120) SIZE(array)
   120 FORMAT (1X,'The size is:     ',I4)
   END SUBROUTINE test_array
   
END MODULE test_module

PROGRAM assumed_shape
!
!  Purpose:
!    To illustrate the use of assumed-shape arrays. 
!
USE test_module
IMPLICIT NONE

! Declare local variables
REAL, DIMENSION(-5:5,-5:5) :: a = 0. ! Array a
REAL, DIMENSION(10,2) :: b = 1.      ! Array b

! Call test_array with array a.
WRITE (*,*) 'Calling test_array with array a:'
CALL test_array(a)

! Call test_array with array b.
WRITE (*,*) 'Calling test_array with array b:'
CALL test_array(b)

END PROGRAM assumed_shape
