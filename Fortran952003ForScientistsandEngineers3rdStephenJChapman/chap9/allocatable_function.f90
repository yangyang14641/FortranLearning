MODULE test_module
!  Purpose:
!    To illustrate the use of allocatable arguments.  
!
CONTAINS 

   FUNCTION test_alloc_fun(n)
   IMPLICIT NONE
   INTEGER,INTENT(IN) :: n  ! Number of elements to return
   REAL,ALLOCATABLE,DIMENSION(:) :: test_alloc_fun  
                                     
   ! Local variables
   INTEGER :: i             ! Loop index
   INTEGER :: istat         ! Allocate status
   
   ! Get the status of the array
   IF ( ALLOCATED(test_alloc_fun) ) THEN
      WRITE (*,'(A)') 'Array is allocated'
   ELSE
      WRITE (*,'(A)') 'Array is NOT allocated'
   END IF
   
   ! Allocate as an n element vector
   ALLOCATE(test_alloc_fun(n), STAT=istat )
   
   ! Initialize data
   DO i = 1, n
       test_alloc_fun(i) = 6 - i
   END DO
   
   ! Display contents of array a on exit
   WRITE (*,'(A,20F4.1)') 'Array on exit = ', test_alloc_fun

   ! Return to caller
   END FUNCTION test_alloc_fun
   
END MODULE test_module


PROGRAM allocatable_function
!
!  Purpose:
!    To illustrate the use of allocatable arguments. 
!
USE test_module
IMPLICIT NONE

! Declare local variables
INTEGER :: n = 5         ! Number of elements to allocate

! Call function and display results
WRITE (*,'(A,20F4.1)') 'Function return = ', test_alloc_fun(n)

END PROGRAM allocatable_function
