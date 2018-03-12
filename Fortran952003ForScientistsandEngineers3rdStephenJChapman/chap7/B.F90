program b
real, dimension(3) :: x = (/1,2,3/), y=(/10,20,30/)
call b1(x,y,3)
write (*,'(3F10.4)') x, y
end program
subroutine b1(a,b,n)
real, dimension(n) :: a, b
b(1:2) = b(1:2) / a(1:2)
end subroutine

