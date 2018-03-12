PROGRAM scratch_file
!
!  Purpose:
!    To illustrate the use of a scratch file and positioning 
!    commands as follows:
!    1.  Read in an arbitrary number of positive or zero
!        values, saving them in a scratch file.  Stop
!        reading when a negative value is encountered.
!    2.  Ask the user for a record number to display.
!    3.  Rewind the file, get that value, and display it. 
!
!  Record of revisions:
!      Date       Programmer          Description of change
!      ====       ==========          =====================
!    11/19/06    S. J. Chapman        Original code
!
IMPLICIT NONE

! Data dictionary: declare constants  
INTEGER, PARAMETER :: LU = 8  ! i/o unit for scratch file

! Data dictionary: declare variable types, definitions, & units  
REAL :: data           ! Data value stored in a disk file
INTEGER :: icount = 0  ! The number of input data records
INTEGER :: irec        ! Record number to recover and display
INTEGER :: j           ! Loop index

! Open the scratch file
OPEN (UNIT=LU, STATUS='SCRATCH' )

! Prompt user and get input data.
WRITE (*, 100)
100 FORMAT (1X,'Enter positive or zero input values. ',/, &
            1X,'A negative value terminates input.' )

! Get the input values, and write them to the scratch file
DO
   WRITE (*, 110) icount + 1     ! Prompt for next value
   110 FORMAT (1X,'Enter sample ',I4,':' )
   READ (*,*) data               ! Read value
   IF ( data < 0. ) EXIT         ! Exit on negative numbers
   icount = icount + 1           ! Valid value: bump count
   WRITE (LU,120) data           ! Write data to scratch file
   120 FORMAT (1X, ES16.6)
END DO

! Now we have all of the records.  Ask which record to see.
! icount records are in the file.
WRITE (*,130) icount
130 FORMAT (1X,'Which record do you want to see (1 to ',I4, ')? ')
READ (*,*) irec

! Do we have a legal record number?  If so, get the record.
! If not, tell the user and stop.
IF ( (irec >= 1) .AND. (irec <= icount) ) THEN

   ! This is a legal record.  Rewind the scratch file.
   REWIND (UNIT=LU)

   ! Read forward to the desired record.
   DO j = 1, irec 
      READ (LU,*) data
   END DO

   ! Tell user.
   WRITE (*,140) irec, data
   140 FORMAT (1X,'The value of record ', I4, ' is ', ES14.5 )
 
ELSE

   ! We have an illegal record number.  Tell user.
   WRITE (*,150) irec
   150 FORMAT (1X,'Illegal record number entered: ', I8)

END IF

! Close file
CLOSE(LU)

END PROGRAM scratch_file
