PROGRAM get_env

! Declare local variables
INTEGER :: length              ! Length
INTEGER :: status              ! Status
CHARACTER(len=80) :: value     ! Environment variable value


! Get the value of the "windir" environment variable
CALL get_environment_variable('windir',value,length,status)

! Tell user
WRITE (*,*) 'Get ''windir'' environment variable:'
WRITE (*,'(A,I6)') 'Status = ', status
IF ( status <= 0 ) THEN
   WRITE (*,'(A,A)') 'Value = ', TRIM(value)
END IF

END PROGRAM get_env