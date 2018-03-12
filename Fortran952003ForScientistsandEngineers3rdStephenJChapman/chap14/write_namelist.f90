PROGRAM write_namelist

!  Purpose:
!    To illustrate a NAMELIST-directed WRITE statement.
!
IMPLICIT NONE

! Data dictionary: declare variable types & definitions
INTEGER :: i = 1, j = 2                       ! Integer variables
REAL :: a = -999., b = 0.                     ! Real variables 
CHARACTER(len=12) :: string = 'Test string.'  ! Char variables
NAMELIST / mylist / i, j, string, a, b        ! Declare namelist

OPEN (8,FILE='output.nml',DELIM='APOSTROPHE') ! Open output file 
WRITE (UNIT=8, NML=mylist)                    ! Write namelist
CLOSE (8)                                     ! Close file

END PROGRAM write_namelist
