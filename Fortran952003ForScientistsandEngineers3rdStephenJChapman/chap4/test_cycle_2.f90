PROGRAM test_cycle_2
INTEGER :: i, j, product
outer: DO i = 1, 3 
   inner: DO j = 1, 3 
      IF ( j == 2) CYCLE outer
      product = i * j
      WRITE (*,*) i, ' * ', j, ' = ', product
   END DO inner
END DO outer
END PROGRAM
