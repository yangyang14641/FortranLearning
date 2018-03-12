program b1
real x(3), y(3)
x = (/ 1.,2.,3./)
call process(x,y,3)
write(*,*) x
write(*,*) y
end program

SUBROUTINE process ( data1, data2, nvals )
REAL, DIMENSION(*) :: data1, data2
INTEGER :: nvals

DO I = 1, nvals 
   data2(i) = 3. * data1(i)
END DO 

data2 = 3. * data1

end subroutine