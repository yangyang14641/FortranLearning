MODULE vector_class
!
!   This module implements a vector class.  This initial
!   version of the class holds an arbitrary-length rank 1
!   REAL vector.  It includes procedures to put and gut 
!   the data, as well as a finalizer to deallocate the 
!   data before an object of this type is destroyed.
!
!  Record of revisions:
!      Date       Programmer          Description of change
!      ====       ==========          =====================
!    12/29/06    S. J. Chapman        Original code
!
IMPLICIT NONE

! Type definition
TYPE,PUBLIC :: vector  ! This will be the name we instantiate

   ! Instance variables. 
   PRIVATE
   REAL,DIMENSION(:),POINTER :: v
   LOGICAL :: v_allocated = .FALSE.
   
!CONTAINS
!
!   ! Bound procedures
!   PUBLIC
!   PROCEDURE :: set_vector ==> set_vector_sub
!   PROCEDURE :: get_vector ==> get_vector_sub
!   FINAL,PRIVATE :: clean_vector ==> clean_vector_sub
   
END TYPE vector

! Restrict access to the actual procedure names
!PRIVATE :: set_vector_sub, get_vector_sub, clean_vector_sub

! Now add methods
CONTAINS

   SUBROUTINE set_vector_sub(this, array)
   ! 
   ! Subroutine to store data in the vector
   !
   IMPLICIT NONE
   
   ! Declare calling arguments
   TYPE(vector) :: this                   ! Vector object
   REAL,DIMENSION(:),INTENT(IN) :: array ! Input data
   
   ! Declare local variables
   INTEGER :: istat                      ! Allocate status

   ! Save data, for deleting any data that might have been
   ! stored in this object.
   IF ( this%v_allocated ) THEN
      DEALLOCATE(this%v,STAT=istat)
   END IF
   ALLOCATE(this%v(SIZE(array,1)),STAT=istat)
   this%v = array
   this%v_allocated = .TRUE.
                   
   END SUBROUTINE set_vector_sub


   SUBROUTINE get_vector_sub(this, array)
   ! 
   ! Subroutine to get data in the vector
   !
   IMPLICIT NONE
   
   ! Declare calling arguments
   TYPE(vector) :: this                   ! Vector object
   REAL,DIMENSION(:),INTENT(OUT) :: array ! Output data
   
   ! Declare local variables
   INTEGER :: array_length                ! Length of array
   INTEGER :: data_length                 ! Length of data vector
   INTEGER :: istat                       ! Allocate status

   ! Retrieve data.  If the size of the stored data does
   ! not match the array size, then return only a subset
   ! of the data or else pad the real data with zeros.
   IF ( this%v_allocated ) THEN
   
      ! Return as much data as possible, truncating or
      ! zero padding as necessary.
      array_length = SIZE(array,1)
      data_length  = SIZE(this%v,1)
      IF ( array_length > data_length ) THEN
         array(1:data_length) = this%v
         array(data_length+1:array_length) = 0
      ELSE IF ( array_length == data_length ) THEN
         array = this%v
      ELSE
         array = this%v(1:array_length)
      END IF
      
   ELSE
   
      ! No data--return zeros.
      array = 0
   
   END IF
                   
   END SUBROUTINE get_vector_sub


   SUBROUTINE clean_vector_sub(this)
   ! 
   ! Subroutine to finalize the vector
   !
   IMPLICIT NONE
   
   ! Declare calling arguments
   TYPE(vector) :: this                   ! Vector object
   
   ! Declare local variables
   INTEGER :: istat                      ! Allocate status

   ! Debugging message
   WRITE (*,*) 'In finalizer ...'

   ! Save data, for deleting any data that might have been
   ! stored in this object.
   IF ( this%v_allocated ) THEN
      DEALLOCATE(this%v,STAT=istat)
   END IF
                   
   END SUBROUTINE clean_vector_sub

END MODULE vector_class
