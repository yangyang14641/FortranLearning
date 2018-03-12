character(len=20) :: string
READ 100, x, y
100 FORMAT (2F10.2)

READ '(2F10.2)', x, y

string = '(2F10.2)'
READ string, x, y
end program