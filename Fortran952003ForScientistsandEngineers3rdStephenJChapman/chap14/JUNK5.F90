program test_EOR
open (7,file='yyy1')
icount = 0
read (7,'(i9)',advance='yes', IOSTAT=j) i
write (*,*) 'i = ', i, '  icount = ', icount, 'iostat = ', j
read (7,'(i9)',advance='yes', IOSTAT=j) i
write (*,*) 'i = ', i, '  icount = ', icount, 'iostat = ', j
read (7,'(i9)',advance='yes', IOSTAT=j) i
write (*,*) 'i = ', i, '  icount = ', icount, 'iostat = ', j
read (7,'(i9)',advance='yes', IOSTAT=j) i
write (*,*) 'i = ', i, '  icount = ', icount, 'iostat = ', j
read (7,'(i9)',advance='yes', IOSTAT=j) i
write (*,*) 'i = ', i, '  icount = ', icount, 'iostat = ', j
go to 9999
!9998 Write (*,*) 'EOR reached!'
9999 continue
end program

