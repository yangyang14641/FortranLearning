PROGRAM open_file
!
!  Purpose:
!    To illustrate the process of checking before overwriting an 
!    output file. 
!
IMPLICIT NONE

! Data dictionary: declare variable types & definitions
CHARACTER(len=20) :: name           ! File name
CHARACTER :: yn                     ! Yes / No flag
LOGICAL :: lexist                   ! True if file exists
LOGICAL :: lopen = .FALSE.          ! True if file is open

! Do until file is open
openfile: DO

   ! Get output file name.
   WRITE (*,*) 'Enter output file name: '
   READ (*,'(A)') file_name
 
   ! Does file already exist?
   INQUIRE ( FILE=file_name, EXIST=lexist )
   exists: IF ( .NOT. lexist ) THEN
      ! It's OK, the file didn't already exist.  Open file.
      OPEN (UNIT=9, FILE=name, STATUS='NEW', ACTION='WRITE')
      lopen = .TRUE.
 
   ELSE
      ! File exists.  Should we replace it?
      WRITE (*,*) 'Output file exists.  Overwrite it? (Y/N) '
      READ (*,'(A)') yn
      CALL ucase ( yn )                ! Shift to upper case

      replace: IF ( yn == 'Y' ) THEN 
         ! It's OK.  Open file.           
         OPEN (UNIT=9, FILE=name, STATUS='REPLACE', ACTION='WRITE')
         lopen = .TRUE.
      END IF replace

   END IF exists
   IF ( lopen ) EXIT
END DO openfile

! Now write output data, and close and save file.
WRITE (9,*) 'This is the output file!'
CLOSE (9,STATUS='KEEP')

END PROGRAM open_file
