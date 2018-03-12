PROGRAM get_command_line

! Declare local variables
INTEGER :: i                   ! Loop index
CHARACTER(len=128) :: command  ! Command line
CHARACTER(len=80) :: arg       ! Single argument

! Get the program name
CALL get_command_argument(0, command)
WRITE (*,'(A,A)') 'Program name is: ', TRIM(command)

! Now get the individual arguments
DO i = 1, command_argument_count()
   CALL get_command_argument(i, arg)
   WRITE (*,'(A,I2,A,A)') 'Argument ', i, ' is ', TRIM(arg)
END DO

END PROGRAM get_command_line