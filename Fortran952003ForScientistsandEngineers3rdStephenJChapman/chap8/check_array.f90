PROGRAM check_array
!
!  Purpose:
!    To illustrate the use of array inquiry functions. 
!
!  Record of revisions:
!      Date       Programmer          Description of change
!      ====       ==========          =====================
!    11/19/06    S. J. Chapman        Original code
!
IMPLICIT NONE

! List of variables:
REAL,DIMENSION(-5:5,0:3) :: a = 0. ! Array to examine

! Get the shape, size, and bounds of the array.
WRITE (*,100) SHAPE(a)
100 FORMAT (1X,'The shape of the array is:         ',7I6)

WRITE (*,110) SIZE(a)
110 FORMAT (1X,'The size of the array is:          ',I6)

WRITE (*,120) LBOUND(a)
120 FORMAT (1X,'The lower bounds of the array are: ',7I6)

WRITE (*,130) UBOUND(a)
130 FORMAT (1X,'The upper bounds of the array are: ',7I6)
 
END PROGRAM check_array
