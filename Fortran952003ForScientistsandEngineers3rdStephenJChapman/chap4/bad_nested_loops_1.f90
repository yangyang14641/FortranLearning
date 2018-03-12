PROGRAM bad_nested_loops_1
INTEGER :: i, j, product
DO i = 1, 3 
   DO j = 1, 3 
      product = i * j
      WRITE (*,*) i, ' * ', j, ' = ', product
END DO
END PROGRAM bad_nested_loops_1
