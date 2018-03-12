SUBROUTINE sub1(input,output)

REAL, INTENT(IN) :: input
REAL, INTENT(OUT) :: output

output = 2. * input
input = -1.

END SUBROUTINE sub1
