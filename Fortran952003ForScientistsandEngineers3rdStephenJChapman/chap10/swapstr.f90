MODULE x
CONTAINS
FUNCTION swap_string(string)
IMPLICIT NONE
CHARACTER(len=*), INTENT(IN) :: string
CHARACTER(len=len(string)) :: swap_string
INTEGER :: length, i
length = LEN(string)
DO i = 1, length
    swap_string(length-i+1:length-i+1) = string(i:i)
END DO
END FUNCTION
END MODULE
PROGRAM test
USE x
CHARACTER(len=12) :: str
str = 'ABCDEFGHI'
WRITE (*,*) swap_string(str)
END PROGRAM test

