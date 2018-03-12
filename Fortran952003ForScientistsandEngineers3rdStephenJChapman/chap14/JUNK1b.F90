program test_EOR
open (7,file='yyy')
read (7,'(i9)',advance='no',size=icount, IOSTAT=istat) i
IF ( istat /= 0 ) GO TO 9998
write (*,*) 'i = ', i, '  icount = ', icount
read (7,'(i9)',advance='no',size=icount, IOSTAT=istat) i
IF ( istat /= 0 ) GO TO 9998
write (*,*) 'i = ', i, '  icount = ', icount
read (7,'(i9)',advance='no',size=icount, IOSTAT=istat) i
IF ( istat /= 0 ) GO TO 9998
write (*,*) 'i = ', i, '  icount = ', icount
read (7,'(i9)',advance='no',size=icount, IOSTAT=istat) i
IF ( istat /= 0 ) GO TO 9998
write (*,*) 'i = ', i, '  icount = ', icount
read (7,'(i9)',advance='no',size=icount, IOSTAT=istat) i
IF ( istat /= 0 ) GO TO 9998
write (*,*) 'i = ', i, '  icount = ', icount
go to 9999
9998 Write (*,*) 'EOR reached!  ISTAT = ', istat
9999 continue
end program

