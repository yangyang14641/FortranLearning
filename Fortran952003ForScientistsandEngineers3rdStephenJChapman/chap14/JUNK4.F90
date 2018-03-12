program test
      CHARACTER(len=10) :: acc, fmt, blk, act, delim
      INTEGER :: lu1 = 35
      LOGICAL :: lexist, lnamed, lopen 
      OPEN (lu1, STATUS='SCRATCH')
      INQUIRE (UNIT=lu1,OPENED=lopen,EXIST=lexist, &
               NAMED=lnamed,ACCESS=acc,FORM=fmt, & 
               BLANK=blk, ACTION=act, DELIM=delim)
      WRITE (*,100) lexist, lopen, lnamed, acc, fmt, &
                    blk, act, delim
      100 FORMAT (1X,'File status:  Exists = ',L1, &
                ' Opened = ', L1, ' Named = ',L1, &
                ' Access = ', A,/,' Format = ',A, &
                ' Blanks = ', A,/,' Action = ',A, &
                ' Delims = ', A)
END PROGRAM
