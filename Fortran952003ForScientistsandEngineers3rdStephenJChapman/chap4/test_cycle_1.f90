PROGRAM test_cycle_1
INTEGER :: i, j, product
DO i = 1, 3 
   DO j = 1, 3 
      IF ( j == 2) CYCLE
      product = i * j
      WRITE (*,*) i, ' * ', j, ' = ', product
   END DO
END DO
END PROGRAM
