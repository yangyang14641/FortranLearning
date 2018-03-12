PROGRAM read_file
!
!  Purpose:
!    To illustrate how to read an unknown number of values from
!    an input data file, detecting both any formatting errors and
!    the end of file.
!
!  Record of revisions:
!      Date       Programmer          Description of change
!      ====       ==========          =====================
!    11/18/06    S. J. Chapman        Original code
!
IMPLICIT NONE

! Data dictionary: declare variable types, definitions, & units  
CHARACTER(len=20) :: filename     ! Name of file to open
INTEGER :: nvals = 0              ! Number of values read in
INTEGER :: status                 ! I/O status
REAL :: value                     ! The real value read in

! Get the file name, and echo it back to the user.
WRITE (*,*) 'Please enter input file name: '
READ  (*,*) filename
WRITE (*,1000) filename
1000 FORMAT (' ','The input file name is: ', A)
 
! Open the file, and check for errors on open.
OPEN (UNIT=3, FILE=filename, STATUS='OLD', ACTION='READ', &
      IOSTAT=status )
openif: IF ( status == 0 ) THEN
 
   ! OPEN was ok.  Read values.
   readloop: DO
      READ (3,*,IOSTAT=status) value    ! Get next value
      IF ( status /= 0 ) EXIT           ! EXIT if not valid.
      nvals = nvals + 1                 ! Valid: increase count
      WRITE (*,1010) nvals, value       ! Echo to screen
      1010 FORMAT (' ','Line ', I6, ': Value = ',F10.4 )
   END DO readloop
 
   ! The WHILE loop has terminated.  Was it because of a READ 
   ! error or because of the end of the input file?
   readif: IF ( status > 0 ) THEN ! a READ error occurred.  Tell user.
 
      WRITE (*,1020) nvals + 1
      1020 FORMAT ('0','An error occurred reading line ', I6)

   ELSE ! the end of the data was reached.  Tell user.
 
      WRITE (*,1030) nvals
      1030 FORMAT ('0','End of file reached.  There were ', I6, &
                     ' values in the file.')
   END IF readif

ELSE openif 
   WRITE (*,1040) status 
   1040 FORMAT (' ','Error opening file: IOSTAT = ', I6 )
END IF openif

! Close file
CLOSE ( UNIT=3 )
 
END PROGRAM read_file
