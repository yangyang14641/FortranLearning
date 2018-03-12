PROGRAM interface_example
!
!  Purpose:
!    To illustrate the use of interface blocks to create explicit
!    interfaces.  This program uses an interface block to create
!    an explicit interface to subroutine "sort", and then takes
!    advantage of that interface to use keyword arguments.
!
!  Record of revisions:
!      Date       Programmer          Description of change
!      ====       ==========          =====================
!    12/08/06    S. J. Chapman        Original code
!
IMPLICIT NONE

! Declare interface to subroutine "sort"
INTERFACE
   SUBROUTINE sort(a,n)
   IMPLICIT NONE
   REAL, DIMENSION(:), INTENT(INOUT) :: a
   INTEGER, INTENT(IN) :: n
   END SUBROUTINE sort
END INTERFACE

! Data dictionary: declare local variable types & definitions
REAL, DIMENSION(6) :: array = (/ 1., 5., 3., 2., 6., 4. /)
INTEGER :: nvals = 6

! Call "sort" to sort data into ascending order.
CALL sort ( N=nvals, A=array)

! Write out sorted array.
WRITE (*,*) array

END PROGRAM interface_example

