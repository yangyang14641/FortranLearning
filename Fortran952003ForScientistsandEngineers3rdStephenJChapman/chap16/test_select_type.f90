PROGRAM test_select_type
!
!  This program tests the select type construct.
!
!  Record of revisions:
!      Date       Programmer          Description of change
!      ====       ==========          =====================
!    12/29/06    S. J. Chapman        Original code
!
IMPLICIT NONE

! Declare a 2D point type
TYPE :: point
   REAL :: x
   REAL :: y
END TYPE point

! Declare a 3D point type
TYPE,EXTENDS(point) :: point3d
   REAL :: z
END TYPE point3d

! Declare a 2D point with temperature data
TYPE,EXTENDS(point) :: point_temp
   REAL :: temp
END TYPE point_temp

! Declare variables
TYPE(point),TARGET :: p2
TYPE(point3d),TARGET :: p3
TYPE(point_temp),TARGET :: pt
CLASS(point),POINTER :: p

! Initialize objects here...
p2%x = 1.
p2%y = 2.
p3%x = -1.
p3%y = 7.
p3%z = -2.
pt%x = 10.
pt%y = 0.
pt%temp = 700.

! Assign one of the objects to "p"
p => pt

! Now access the data in that object
SELECT TYPE (p)
TYPE IS ( point3d )
   WRITE (*,*) 'Type is point3d'
   WRITE (*,*) p%x, p%y, p%z
TYPE IS ( point_temp )
   WRITE (*,*) 'Type is point_temp'
   WRITE (*,*) p%x, p%y, p%temp
CLASS IS ( point )
   WRITE (*,*) 'Class is point'
   WRITE (*,*) p%x, p%y
END SELECT

END PROGRAM test_select_type
