PROGRAM read_namelist

!  Purpose:
!    To illustrate a NAMELIST-directed READ statement.
!
IMPLICIT NONE

! Data dictionary: declare variable types & definitions
INTEGER :: i = 1, j = 2                       ! Integer variables
REAL :: a = -999., b = 0.                     ! Real variables 
CHARACTER(len=12) :: string = 'Test string.'  ! Char variables
NAMELIST / mylist / i, j, string, a, b        ! Declare namelist

OPEN (7,FILE='input.nml',DELIM='APOSTROPHE')  ! Open input file.

! Write NAMELIST before update
WRITE (*,'(1X,A)') 'Namelist file before update: '
WRITE (UNIT=*, NML=mylist) 

READ (UNIT=7,NML=mylist)                      ! Read namelist file.

! Write NAMELIST after update
WRITE (*,'(1X,A)') 'Namelist file after update: '
WRITE (UNIT=*, NML=mylist) 

END PROGRAM read_namelist
