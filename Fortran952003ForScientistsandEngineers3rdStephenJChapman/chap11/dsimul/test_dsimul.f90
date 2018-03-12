PROGRAM test_dsimul
!
!  Purpose:
!    To test subroutine dsimul, which solves a set of N linear
!    equations in N unknowns.  This test driver calls subroutine
!    simul to solve the problem in single precision, and subrou-
!    tine dsimul to solve the problem in double precision.  The
!    results of the two solutions together with their errors are
!    displayed in a summary table.
!
!  Record of revisions:
!      Date       Programmer          Description of change
!      ====       ==========          =====================
!    11/27/06    S. J. Chapman        Original code
!
IMPLICIT NONE

! Data dictionary: declare constants
INTEGER, PARAMETER :: SGL = SELECTED_REAL_KIND(p=6)   ! Single
INTEGER, PARAMETER :: DBL = SELECTED_REAL_KIND(p=13)  ! Double

! Data dictionary: declare variable types & definitions
REAL(KIND=SGL), ALLOCATABLE, DIMENSION(:,:) :: a
                                 ! Single-precision coefficients
REAL(KIND=SGL), ALLOCATABLE, DIMENSION(:) :: b
                                 ! Single-precision constant values
REAL(KIND=SGL), ALLOCATABLE, DIMENSION(:) :: soln
                                 ! Single-precision solution
REAL(KIND=SGL), ALLOCATABLE, DIMENSION(:) :: serror
                                 ! Array of single-precision errors
REAL(KIND=SGL) :: serror_max     ! Max single precision error
REAL(KIND=DBL), ALLOCATABLE, DIMENSION(:,:) :: da
                                 ! Double-precision coefficients
REAL(KIND=DBL), ALLOCATABLE, DIMENSION(:) :: db
                                 ! Double-precision constant values
REAL(KIND=DBL), ALLOCATABLE, DIMENSION(:) :: dsoln
                                 ! Double-precision solution
REAL(KIND=DBL), ALLOCATABLE, DIMENSION(:) :: derror
                                 ! Array of double-precision errors
REAL(KIND=DBL) :: derror_max     ! Max double precision error
INTEGER :: error_flag            ! Error flag from subroutines
INTEGER :: i, j                  ! Loop index
INTEGER :: istat                 ! I/O status
INTEGER :: n                     ! Size of system of eqns to solve
CHARACTER(len=20) :: filename    ! Input data file name

! Get the name of the disk file containing the equations.
WRITE (*,*) 'Enter the file name containing the eqns: '
READ (*,'(A20)') filename
 
! Open input data file.  Status is OLD because the input data must 
! already exist.
OPEN ( UNIT=1, FILE=filename, STATUS='OLD', ACTION='READ', &
     IOSTAT=istat )
 
! Was the OPEN successful? 
open_ok: IF ( istat == 0 ) THEN
 
   ! The file was opened successfully, so read the number of 
   ! equations in the system.
   READ (1,*) n

   ! Allocate memory for that number of equations
   ALLOCATE ( a(n,n), b(n), soln(n), serror(n), &
              da(n,n), db(n), dsoln(n), derror(n), STAT=istat )

   ! If the memory is available, read in equations and 
   ! process them.
   solve: IF ( istat == 0 ) THEN

      DO i = 1, n
         READ (1,*) (da(i,j), j=1,n), db(i)
      END DO
 
      ! Copy the coefficients in single precision for the
      ! single precision solution.
      a = da
      b = db
    
      ! Display coefficients.
      WRITE (*,1010)
      1010 FORMAT (/,1X,'Coefficients:')
      DO i = 1, n
         WRITE (*,'(1X,7F11.4)') (a(i,j), j=1,n), b(i)
      END DO
 
      ! Solve equations.
      CALL simul  (a,  b,  soln,  n, n, error_flag )
      CALL dsimul (da, db, dsoln, n, n, error_flag )
 
      ! Check for error.
      error_check: IF ( error_flag /= 0 ) THEN
         WRITE (*,1020) 
         1020 FORMAT (/1X,'Zero pivot encountered!', &
              //1X,'There is no unique solution to this system.')

      ELSE error_check
 
         ! No errors.  Check for roundoff by substituting into 
         ! the original equations, and calculate the differences.
         serror_max = 0.
         derror_max = 0._DBL
         serror = 0.
         derror = 0._DBL
         DO i = 1, n         
            serror(i) = SUM ( a(i,:)  * soln(:)  ) - b(i)
            derror(i) = SUM ( da(i,:) * dsoln(:) ) - db(i)
         END DO
         serror_max = MAXVAL ( ABS ( serror ) )
         derror_max = MAXVAL ( ABS ( derror ) )
 
         ! Tell user about it.
         WRITE (*,1030)
         1030 FORMAT (/1X,'  i     SP x(i)        DP x(i)      ', &
              '       SP Err         DP Err  ')
         WRITE (*,1040)
         1040 FORMAT ( 1X,' ===   =========      =========     ', &
              '      ========       ======== ')
         DO i = 1, n
            WRITE (*,1050) i, soln(i), dsoln(i), serror(i), derror(i)
            1050 FORMAT (1X, I3, 2X, G15.6, G15.6, F15.8, F15.8)
         END DO
 
         ! Write maximum errors.
         WRITE (*,1060) serror_max, derror_max
         1060 FORMAT (/,1X,'Max single-precision error:',F15.8, &
              /,1X,'Max double-precision error:',F15.8)
 
      END IF error_check
   END IF solve

   ! Deallocate dynamic memory
   DEALLOCATE ( a, b, soln, serror, da, db, dsoln, derror )

ELSE open_ok 
   ! Else file open failed.  Tell user.
   WRITE (*,1070) istat
   1070 FORMAT (1X,'File open failed--status = ', I6)
END IF open_ok

END PROGRAM test_dsimul
