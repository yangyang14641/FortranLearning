MODULE types
!
!  Purpose:
!    To define the derived data type used for the customer 
!    database.
!
!  Record of revisions:
!       Date       Programmer          Description of change
!       ====       ==========          =====================
!     12/04/06    S. J. Chapman        Original code
!
IMPLICIT NONE

! Declare type personal_info
TYPE :: personal_info
   CHARACTER(len=12) :: first        ! First name
   CHARACTER         :: mi           ! Middle Initial
   CHARACTER(len=12) :: last         ! Last name
   CHARACTER(len=26) :: street       ! Street Address
   CHARACTER(len=12) :: city         ! City
   CHARACTER(len=2)  :: state        ! State 
   INTEGER           :: zip          ! Zip code
END TYPE personal_info

END MODULE types


PROGRAM customer_database
!
!  Purpose:
!    To read in a character input data set, sort it into ascending 
!    order using the selection sort algorithm, and to write the  
!    sorted data to the standard output device.  This program calls 
!    subroutine "sort_database" to do the actual sorting.
!
!  Record of revisions:
!      Date       Programmer          Description of change
!      ====       ==========          =====================
!     12/04/06    S. J. Chapman        Original code
!
USE types                      ! Declare the module types
IMPLICIT NONE

! Data dictionary: declare constants
INTEGER, PARAMETER :: MAX_SIZE = 100  ! Max addresses in database

! Data dictionary: declare external functions
LOGICAL, EXTERNAL :: lt_last     ! Comparison fn for last names
LOGICAL, EXTERNAL :: lt_city     ! Comparison fn for cities
LOGICAL, EXTERNAL :: lt_zip      ! Comparison fn for zip codes

! Data dictionary: declare variable types & definitions
TYPE(personal_info), DIMENSION(MAX_SIZE) :: customers   
                                 ! Data array to sort
INTEGER :: choice                ! Choice of how to sort database
LOGICAL :: exceed = .FALSE.      ! Logical indicating that array 
                                 !   limits are exceeded.
CHARACTER(len=20) :: filename    ! Input data file name
INTEGER :: i                     ! Loop index
INTEGER :: nvals = 0             ! Number of data values to sort
INTEGER :: status                ! I/O status: 0 for success
TYPE(personal_info) :: temp      ! Temporary variable for reading

! Get the name of the file containing the input data.
WRITE (*,*) 'Enter the file name with customer database: '
READ (*,'(A20)') filename
 
! Open input data file.  Status is OLD because the input data must 
! already exist.
OPEN ( UNIT=9, FILE=filename, STATUS='OLD', IOSTAT=status )
 
! Was the OPEN successful? 
fileopen: IF ( status == 0 ) THEN       ! Open successful
 
   ! The file was opened successfully, so read the customer 
   ! database from it.
   DO
      READ (9, 1010, IOSTAT=status) temp   ! Get value
      1010 FORMAT (A12,1X,A1,1X,A12,1X,A26,1X,A12,1X,A2,1X,I5)
      IF ( status /= 0 ) EXIT              ! Exit on end of data 
      nvals = nvals + 1                    ! Bump count
      size: IF ( nvals <= MAX_SIZE ) THEN  ! Too many values?
         customers(nvals) = temp           ! No: Save value in array
      ELSE
         exceed = .TRUE.                   ! Yes: Array overflow
      END IF size
   END DO
 
   ! Was the array size exceeded?  If so, tell user and quit.
   toobig: IF ( exceed ) THEN
      WRITE (*,1020) nvals, MAX_SIZE
      1020 FORMAT (' Maximum array size exceeded: ', I6, ' > ', I6 )
   ELSE

      ! Limit not exceeded: find out how to sort data.
      WRITE (*,1030) 
      1030 FORMAT (1X,'Enter way to sort database:',/, &
                   1X,'  1 -- By last name ',/, &
                   1X,'  2 -- By city ',/, &
                   1X,'  3 -- By zip code ')
      READ (*,*) choice

      ! Sort database
      SELECT CASE ( choice)
      CASE (1)
         CALL sort_database (customers, nvals, lt_last )
      CASE (2)
         CALL sort_database (customers, nvals, lt_city )
      CASE (3)
         CALL sort_database (customers, nvals, lt_zip )
      CASE DEFAULT
         WRITE (*,*) 'Invalid choice entered!'
      END SELECT

      ! Now write out the sorted data.
      WRITE (*,'(A)') ' The sorted database values are: '
      WRITE (*,1040) ( customers(i), i = 1, nvals )
      1040 FORMAT (1X,A12,1X,A1,1X,A12,1X,A26,1X,A12,1X,A2,1X,I5)

   END IF toobig

ELSE fileopen

   ! Status /= 0, so an open error occurred.
   WRITE (*,'(A,I6)') ' File open error: IOSTAT = ', status

END IF fileopen
 
END PROGRAM customer_database


SUBROUTINE sort_database (array, n, lt_fun )
!
!  Purpose:
!    To sort array "array" into ascending order using a selection
!    sort, where "array" is an array of the derived data type
!    "personal_info".  The sort is based on the external 
!    comparison function "lt_fun", which will differ depending on
!    which component of the derived type array is used for 
!    comparison.
!
!  Record of revisions:
!      Date       Programmer          Description of change
!      ====       ==========          =====================
!    12/04/06    S. J. Chapman        Original code
!
USE types                      ! Declare the module types
IMPLICIT NONE

! Data dictionary: declare calling parameter types & definitions
INTEGER, INTENT(IN) :: n                  ! Number of values
TYPE(personal_info), DIMENSION(n), INTENT(INOUT) :: array  
                                          ! Array to be sorted
LOGICAL, EXTERNAL :: lt_fun               ! Comparison function

! Data dictionary: declare local variable types & definitions
INTEGER :: i                   ! Loop index
INTEGER :: iptr                ! Pointer to smallest value
INTEGER :: j                   ! Loop index
TYPE(personal_info) :: temp    ! Temp variable for swaps
 
! Sort the array
outer: DO i = 1, n-1
 
   ! Find the minimum value in array(i) through array(n)
   iptr = i
   inner: DO j = i+1, n
      minval: IF ( lt_fun(array(J),array(iptr)) ) THEN
         iptr = j
      END IF minval
   END DO inner
 
   ! iptr now points to the minimum value, so swap array(iptr)  
   ! with array(i) if i /= iptr.
   swap: IF ( i /= iptr ) THEN
      temp        = array(i)
      array(i)    = array(iptr)
      array(iptr) = temp
   END IF swap
 
END DO outer
END SUBROUTINE sort_database


LOGICAL FUNCTION lt_last (a, b)
!
!  Purpose:
!    To compare variables "a" and "b" and determine which  
!    has the smaller last name (lower alphabetical order).
!
USE types                    ! Declare the module types
IMPLICIT NONE

! Data dictionary: declare calling parameter types & definitions
TYPE (personal_info), INTENT(IN) :: a, b

! Make comparison.
lt_last = LLT ( a%last, b%last )

END FUNCTION lt_last


LOGICAL FUNCTION lt_city (a, b)
!
!  Purpose:
!    To compare variables "a" and "b" and determine which 
!    has the smaller city (lower alphabetical order).
!
USE types                    ! Declare the module types
IMPLICIT NONE

! Data dictionary: declare calling parameter types & definitions
TYPE (personal_info), INTENT(IN) :: a, b

! Make comparison.
lt_city = LLT ( a%city, b%city )

END FUNCTION lt_city


LOGICAL FUNCTION lt_zip (a, b)
!
!  Purpose:
!    To compare variables "a" and "b" and determine which 
!    has the smaller zip code (lower numerical value).
!
USE types                    ! Declare the module types
IMPLICIT NONE

! Data dictionary: declare calling parameter types & definitions
TYPE (personal_info), INTENT(IN) :: a, b

! Make comparison.
lt_zip = a%zip < b%zip

END FUNCTION lt_zip

