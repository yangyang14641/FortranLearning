PROGRAM bad_nested_loops_2
INTEGER :: i, j, product
outer: DO i = 1, 3 
   inner: DO j = 1, 3 
      product = i * j
      WRITE (*,*) i, ' * ', j, ' = ', product
END DO outer
END PROGRAM bad_nested_loops_2
