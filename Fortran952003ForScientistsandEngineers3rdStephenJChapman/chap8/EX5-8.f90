PROGRAM test_output
INTEGER, PARAMETER :: minval = 0, maxval = 3
INTEGER, DIMENSION(minval:maxval,minval:maxval) :: my_data
my_data(0,:) = (/  1,  2,  3,  4 /)
my_data(1,:) = (/  5,  6,  7,  8 /)
my_data(2,:) = (/  9, 10, 11, 12 /)
my_data(3,:) = (/ 13, 14, 15, 16 /)

WRITE (*,100) my_data
100 FORMAT (/,(4(1X,I4)))
WRITE (*,110) my_data
110 FORMAT (/,(6(1X,I4)))
WRITE (*,120) ((my_data(I,J), J=minval,maxval), I=minval,maxval)
120 FORMAT (/,(6(1X,I4)))
WRITE (*,130) ((my_data(I,J), J=minval,maxval), I=minval,maxval)
130 FORMAT (/,(4(1X,I4)))
WRITE (*,140) (my_data(I,:), I=minval,maxval)
140 FORMAT (/,(4(1X,I4)))
WRITE (*,150) ' '
150 FORMAT (A)
DO I = minval, maxval
   WRITE (*,160) (my_data(I,J), J=minval,maxval)
   160 FORMAT (6(1X,I4))
END DO
END PROGRAM
