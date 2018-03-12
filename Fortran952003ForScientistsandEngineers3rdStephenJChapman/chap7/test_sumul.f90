PROGRAM test_simul
!
!  Purpose:
!    To test subroutine simul, which solves a set of N linear
!    equations in N unknowns.
!
!  Record of revisions:
!      Date       Programmer          Description of change
!      ====       ==========          =====================
!    06/26/02    S. J. Chapman        Original code
!
IMPLICIT NONE

! Declare parameters:
INTEGER, PARAMETER :: max_size = 10    ! Max number of eqns 

! Declare variables:
INTEGER :: i, j, n, istat, error
REAL, DIMENSION(max_size,max_size) :: a 
REAL, DIMENSION(max_size) :: b
CHARACTER(len=20) :: file_name
 
! Get the name of the disk file containing the equations.
WRITE (*,1000) 
1000 FORMAT (' Enter the file name containing the eqns: ')
READ (*,'(A20)') file_name
 
! Open input data file.  Status is OLD because the input data must 
! already exist.
OPEN ( UNIT=1, FILE=file_name, STATUS='OLD', ACTION='READ', &
       IOSTAT=istat )

! Was the OPEN successful? 
fileopen: IF ( istat == 0 ) THEN
   ! The file was opened successfully, so read the number of 
   ! equations in the system.
   READ (1,*) n
 
   ! If the number of equations is <= max_size, read them in
   ! and process them.
   size_ok: IF ( n <= max_size ) THEN
      DO i = 1, n
         READ (1,*) (a(i,j), j=1,n), b(i)
      END DO
 
      ! Display coefficients.
      WRITE (*,1020)
      1020 FORMAT (/,1X,'Coefficients before call:')
      DO i = 1, n
         WRITE (*,1030) (a(i,j), j=1,n), b(i)
         1030 FORMAT (1X,7F11.4)
      END DO
 
      ! Solve equations.
      CALL simul1 (a, b, max_size, n, error )
 
      ! Check for error.
      error_check: IF ( error /= 0 ) THEN

         WRITE (*,1040) 
         1040 FORMAT (/1X,'Zero pivot encountered!', &
                     //1X,'There is no unique solution to this system.')

      ELSE error_check
 
         ! No errors. Display coefficients.
         WRITE (*,1050)
         1050 FORMAT (/,1X,'Coefficients after call:')
         DO  i = 1, n
             WRITE (*,1030) (a(i,j), j=1,n), b(i)
         END DO
 
         ! Write final answer.
         WRITE (*,1060)
         1060 FORMAT (/,1X,'The solutions are:')
         DO i = 1, n
            WRITE (*,1070) i, b(i)
            1070 FORMAT (3X,'X(',I2,') = ',F16.6)
         END DO

      END IF error_check
   END IF size_ok
ELSE fileopen

   ! Else file open failed.  Tell user.
   WRITE (*,1080) istat
   1080 FORMAT (1X,'File open failed--status = ', I6)

END IF fileopen
END PROGRAM

