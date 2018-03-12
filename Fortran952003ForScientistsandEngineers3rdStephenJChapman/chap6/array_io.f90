PROGRAM array_io
!
!  Purpose:
!    To illustrate array I/O.
!
!  Record of revisions:
!      Date       Programmer          Description of change
!      ====       ==========          =====================
!    11/16/06    S. J. Chapman        Original code
!
IMPLICIT NONE

! Data dictionary: declare variable types, definitions, & units
REAL, DIMENSION(5) :: a = (/1.,2.,3.,20.,10./) ! 5-element test array
INTEGER, DIMENSION(4) :: vec = (/4,3,4,5/)     ! vector subscript
 
! Output entire array.
WRITE (*,100) a 
100 FORMAT ( 2X, 5F8.3 )

! Output array section selected by a triplet.
WRITE (*,100) a(2::2) 

! Output array section selected by a vector subscript.
WRITE (*,100) a(vec)

END PROGRAM array_io
