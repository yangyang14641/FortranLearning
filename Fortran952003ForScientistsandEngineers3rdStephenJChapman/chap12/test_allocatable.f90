PROGRAM test_allocatable
!
!  Purpose:
!    To illustrate I/O of variables of derived data types.
!
!  Record of revisions:
!       Date       Programmer          Description of change
!       ====       ==========          =====================
!     12/04/06    S. J. Chapman        Original code
!
IMPLICIT NONE

! Declare type person
TYPE :: personal_info
   CHARACTER(len=12) :: first        ! First name
   CHARACTER         :: mi           ! Middle Initial
   CHARACTER(len=12) :: last         ! Last name
   CHARACTER(len=26) :: street       ! Street Address
   CHARACTER(len=12) :: city         ! City
   CHARACTER(len=2)  :: state        ! State 
   INTEGER           :: zip          ! Zip code
END TYPE personal_info
INTEGER :: istat                     ! Allocate status

! Declare a variable of type person:
TYPE (personal_info),ALLOCATABLE :: john

! Allocate variable
ALLOCATE( person, STAT=istat )

! Initialize variable
john = person('John','R','Jones','323-6439',21,'M','123-45-6789')

! Output variable using free format I/O
WRITE (*,*) 'Free format: ', john 

! Output variable using formatted I/O
WRITE (*,1000) john 
1000 FORMAT (' Formatted I/O:',/,4(1X,A,/),1X,I4,/,1X,A,/,1X,A)

END PROGRAM test_allocatable
