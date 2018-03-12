MODULE test_module
!  Purpose:
!    To illustrate the use of allocatable arguments.  
!
CONTAINS 

   SUBROUTINE test_alloc(array)
   IMPLICIT NONE
   REAL,DIMENSION(:),ALLOCATABLE,INTENT(INOUT) :: array  
                                     ! Test array
                                     
   ! Local variables
   INTEGER :: i             ! Loop index
   INTEGER :: istat         ! Allocate status
   
   ! Get the status of this array
   IF ( ALLOCATED(array) ) THEN
      WRITE (*,'(A)') 'Sub: the array is allocated'
      WRITE (*,'(A,6F4.1)') 'Sub: Array on entry = ', array
   ELSE
      WRITE (*,*) 'In sub: the array is not allocated'
   END IF
   
   ! Deallocate the array
   IF ( ALLOCATED(array) ) THEN
      DEALLOCATE( array, STAT=istat )
   END IF
   
   ! Reallocate as a 5 element vector
   ALLOCATE(array(5), STAT=istat )
   
   ! Save data
   DO i = 1, 5
       array(i) = 6 - i
   END DO
   
   ! Display contents of array a on exit
   WRITE (*,'(A,6F4.1)') 'Sub: Array on exit = ', array

   ! Return to caller
   END SUBROUTINE test_alloc
   
END MODULE test_module


PROGRAM allocatable_arguments
!
!  Purpose:
!    To illustrate the use of allocatable arguments. 
!
USE test_module
IMPLICIT NONE

! Declare local variables
REAL,ALLOCATABLE,DIMENSION(:) :: a 
INTEGER :: istat         ! Allocate status

! Allocate array
ALLOCATE( a(6), STAT=istat )

! Initialize array
a = (/ 1., 2., 3., 4., 5., 6. /)

! Display a before call
WRITE (*,'(A,6F4.1)') 'Main: Array a before call = ', a

! Call subroutine
CALL test_alloc(a)

! Display a after call
WRITE (*,'(A,6F4.1)') 'Main: Array a after call = ', a

END PROGRAM allocatable_arguments
