PROGRAM test_simul
!
!  Purpose:
!    To test subroutine simul, which solves a set of N linear
!    equations in N unknowns.
!
!  Record of revisions:
!      Date       Programmer          Description of change
!      ====       ==========          =====================
!    11/23/06    S. J. Chapman        Original code
!
IMPLICIT NONE

! Data dictionary: declare constants
INTEGER, PARAMETER :: MAX_SIZE = 10    ! Max number of eqns 

! Data dictionary: declare local variable types & definitions
REAL, DIMENSION(MAX_SIZE,MAX_SIZE) :: a 
                                     ! Array of coefficients (n x n).
                                     ! This array is of size ndim x 
                                     ! ndim, but only n x n of the 
                                     ! coefficients are being used.
                                     ! The declared dimension ndim 
                                     ! must be passed to the sub, or
                                     ! it won't be able to interpret
                                     ! subscripts correctly.  (This
                                     ! array is destroyed during
                                     ! processing.)
REAL, DIMENSION(MAX_SIZE) :: b       ! Input: Right-hand side of eqns.
                                     ! Output: Solution vector.
INTEGER :: error                     ! Error flag:
                                     !   0 -- No error
                                     !   1 -- Singular equations
CHARACTER(len=20) :: file_name       ! Name of file with eqns
INTEGER :: i                         ! Loop index
INTEGER :: j                         ! Loop index
INTEGER :: n                         ! Number of simul eqns (<= MAX_SIZE)
INTEGER :: istat                     ! I/O status
 
! Get the name of the disk file containing the equations.
WRITE (*,"(' Enter the file name containing the eqns: ')") 
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
 
   ! If the number of equations is <= MAX_SIZE, read them in
   ! and process them.
   size_ok: IF ( n <= MAX_SIZE ) THEN
      DO i = 1, n
         READ (1,*) (a(i,j), j=1,n), b(i)
      END DO
 
      ! Display coefficients.
      WRITE (*,"(/,1X,'Coefficients before call:')")
      DO i = 1, n
         WRITE (*,"(1X,7F11.4)") (a(i,j), j=1,n), b(i)
      END DO
 
      ! Solve equations.
      CALL simul (a, b, MAX_SIZE, n, error )
 
      ! Check for error.
      error_check: IF ( error /= 0 ) THEN

         WRITE (*,1010) 
         1010 FORMAT (/1X,'Zero pivot encountered!', &
                     //1X,'There is no unique solution to this system.')

      ELSE error_check
 
         ! No errors. Display coefficients.
         WRITE (*,"(/,1X,'Coefficients after call:')")
         DO  i = 1, n
             WRITE (*,"(1X,7F11.4)") (a(i,j), j=1,n), b(i)
         END DO
 
         ! Write final answer.
         WRITE (*,"(/,1X,'The solutions are:')")
         DO i = 1, n
            WRITE (*,"(3X,'X(',I2,') = ',F16.6)") i, b(i)
         END DO

      END IF error_check
   END IF size_ok
ELSE fileopen

   ! Else file open failed.  Tell user.
   WRITE (*,1020) istat
   1020 FORMAT (1X,'File open failed--status = ', I6)

END IF fileopen
END PROGRAM test_simul

