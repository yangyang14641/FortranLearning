MODULE character_subs

CONTAINS
   FUNCTION abc( n )
   ! 
   !  Purpose: 
   !    To return a string containing the first N characters
   !    of the alphabet. 
   ! 
   !  Record of revisions:
   !      Date       Programmer          Description of change
   !      ====       ==========          =====================
   !    11/25/06    S. J. Chapman        Original code
   !
   IMPLICIT NONE

   ! Declare calling parameters:
   INTEGER, INTENT(IN) :: n         ! Length of string to return
   CHARACTER(len=n) abc             ! Returned string  

   ! Declare local variables:
   CHARACTER(len=26) :: alphabet = 'abcdefghijklmnopqrstuvwxyz'
 
   ! Get string to return
   abc = alphabet(1:n)

   END FUNCTION abc

END MODULE character_subs
