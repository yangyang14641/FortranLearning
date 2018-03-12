PROGRAM test_shape
!
!  This program tests polymorphism using the shape class
!  and its subclasses.
!
!  Record of revisions:
!      Date       Programmer          Description of change
!      ====       ==========          =====================
!    12/29/06    S. J. Chapman        Original code
!
USE circle_class                 ! Import circle class
USE square_class                 ! Import square class
USE rectangle_class              ! Import rectangle class
USE triangle_class               ! Import triangle class
USE pentagon_class               ! Import pentagon class
IMPLICIT NONE

! Declare variables
TYPE(circle),POINTER :: cir      ! Circle object
TYPE(square),POINTER :: squ      ! Square object
TYPE(rectangle),POINTER :: rec   ! Rectangle object
TYPE(triangle),POINTER :: tri    ! Triangle object
TYPE(pentagon),POINTER :: pen    ! Pentagon object
INTEGER :: i                     ! Loop index
CHARACTER(len=50) :: id_string   ! ID string
INTEGER :: istat                 ! Allocate status

! Create an array of shape pointers
TYPE :: shape_ptr
   CLASS(shape),POINTER :: p     ! Pointer to shapes
END TYPE shape_ptr
TYPE(shape_ptr),DIMENSION(5) :: shapes

! Create and initialize circle
ALLOCATE( cir, STAT=istat )
CALL cir%initialize(2.0)

! Create and initialize square
ALLOCATE( squ, STAT=istat )
CALL squ%initialize(2.0,2.0)

! Create and initialize rectangle
ALLOCATE( rec, STAT=istat )
CALL rec%initialize(2.0,1.0)

! Create and initialize triangle
ALLOCATE( tri, STAT=istat )
CALL tri%initialize(2.0)

! Create and initialize pentagon
ALLOCATE( pen, STAT=istat )
CALL pen%initialize(2.0)

! Create the array of shape pointers
shapes(1)%p => cir
shapes(2)%p => squ
shapes(3)%p => rec
shapes(4)%p => tri
shapes(5)%p => pen

! Now display the results using the array of
! shape pointers.  
DO i = 1, 5

    ! Get ID string
    id_string = shapes(i)%p%to_string()
    WRITE (*,'(/A)') id_string
    
    ! Get the area and perimeter
    WRITE (*,'(A,F8.4)') 'Area      = ', shapes(i)%p%area()
    WRITE (*,'(A,F8.4)') 'Perimeter = ', shapes(i)%p%perimeter()
END DO

END PROGRAM test_shape
