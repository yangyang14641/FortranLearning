PROGRAM direct_access_formatted
!
!  Purpose:
!    To illustrate the use of direct access Fortran files.
!
!  Record of revisions:
!      Date       Programmer          Description of change
!      ====       ==========          =====================
!    12/11/06    S. J. Chapman        Original code
!
IMPLICIT NONE

! Data dictionary: declare variable types & definitions
INTEGER :: i                   ! Index variable
INTEGER :: irec                ! Number of record in file
CHARACTER(len=40) :: line      ! String containing current line.

! Open a direct access formatted file with 40 characters per record.
OPEN ( UNIT=8, FILE='dirio.fmt', ACCESS='DIRECT', &
       FORM='FORMATTED', STATUS='REPLACE', RECL=40 )
 
! Insert 100 records into this file.
DO i = 1, 100
   WRITE ( 8, '(A,I3,A)', REC=i ) 'This is record ', i, '.'
END DO

! Find out which record the user wants to retrieve.
WRITE (*,'(A)',ADVANCE='NO') ' Which record would you like to see? '
READ (*,'(I3)') irec
 
! Retrieve the desired record.
READ ( 8, '(A)', REC=irec ) line
 
! Display the record.
WRITE (*, '(A,/,5X,A)' ) ' The record is: ', line

END PROGRAM direct_access_formatted
