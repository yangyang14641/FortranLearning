PROGRAM stop_test
!
!  Purpose:
!    To illustrate multiple STOP statements in a program.
!
IMPLICIT NONE

! Declare parameters:
INTEGER, PARAMETER :: lu = 12      ! I/O unit

! Declare variables:
INTEGER :: error                   ! Error flag
CHARACTER(len=20) :: filename      ! File name

! Prompt user and get the name of the input file.
WRITE (*,*) 'Enter file name: '
READ (*,'(A)') filename

! Open the input file
OPEN (UNIT=lu, FILE=filename, STATUS='OLD', IOSTAT=error )
 
! Check to see of the OPEN failed.
IF ( error > 0 ) THEN 
   WRITE (*,1020) filename
   1020 FORMAT (1X,'ERROR: File ',A,' does not exist!')
   STOP
END IF
 
! Normal processing...
...
 
! Close input file, and quit.
CLOSE (LU)
 
STOP 'Normal completion.'
END PROGRAM stop_test
