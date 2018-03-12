PROGRAM test_io
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
TYPE :: person
   CHARACTER(len=14) :: first_name
   CHARACTER :: middle_initial
   CHARACTER(len=14) :: last_name
   CHARACTER(len=14) :: phone
   INTEGER :: age
   CHARACTER :: sex
   CHARACTER(len=11) :: ssn
END TYPE person

! Declare a variable of type person:
TYPE (person) :: john

! Initialize variable
john = person('John','R','Jones','323-6439',21,'M','123-45-6789')

! Output variable using free format I/O
WRITE (*,*) 'Free format: ', john 

! Output variable using formatted I/O
WRITE (*,1000) john 
1000 FORMAT (' Formatted I/O:',/,4(1X,A,/),1X,I4,/,1X,A,/,1X,A)

END PROGRAM test_io
