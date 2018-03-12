program test_EOR
open (7,file='yyy')
read (7,100,advance='yes',size=icount, EOR=9998) i
write (*,*) 'i = ', i, '  icount = ', icount
read (7,100,advance='yes',size=icount, EOR=9998) i
write (*,*) 'i = ', i, '  icount = ', icount
read (7,100,advance='no',size=icount, EOR=9998) i
write (*,*) 'i = ', i, '  icount = ', icount
read (7,100,advance='no',size=icount, EOR=9998) i
write (*,*) 'i = ', i, '  icount = ', icount
read (7,100,advance='no',size=icount, EOR=9998) i
write (*,*) 'i = ', i, '  icount = ', icount
100 format (I10)
go to 9999
9998 Write (*,*) 'EOR reached!'
9999 continue
end program
