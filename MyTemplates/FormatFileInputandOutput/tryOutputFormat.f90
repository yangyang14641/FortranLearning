!--------------------------------------------------------------
! This program is designed for try FORTRAN's Output Format
!--------------------------------------------------------------
! Author: Yang Yang
! Date: 2017/04/05
!--------------------------------------------------------------
!--------------------------------------------------------------




!--------------------------------------------------------------
PROGRAM MAIN
IMPLICIT NONE
! program parameters
INTEGER,PARAMETER:: M = 4, N = 4

! vars in main function
REAL*8 :: A(M,N)
INTEGER i,j


! Define value of array A
DO j = 1,N
  DO i = 1,M
       A(i,j) = (i-1)*N + j + 1.D-2
  END DO
END DO

! Output array to file by calling subroutine
CALL dataOutput(A,M,N)

END PROGRAM MAIN
!--------------------------------------------------------------
!--------------------------------------------------------------




!--------------------------------------------------------------
SUBROUTINE dataOutput(A,M,N)
IMPLICIT NONE

CHARACTER(len=20),PARAMETER:: outputFileName = 'result.txt'   ! Output file name
! Subroutine's input vars
INTEGER M,N
REAL*8 A(M,N)

! Subroutine's locaL vars
INTEGER i,j,ierror
CHARACTER(len=100) M_Temp
WRITE(M_Temp,*) M

! Open OutputFile
OPEN(unit = 10, file = TRIM(outputFileName), IOSTAT = ierror)
IF (ierror.EQ.0) THEN
  PRINT*, 'File result.txt successfully opened!...'
END IF

! Output data to file
DO i = 1,M
  DO j = 1,N
        WRITE(10,100) A(i,j)
  END DO
  WRITE(10,200)
END DO


100 FORMAT(1x,ES24.15,\)          ! Write data to file without enter
200 FORMAT(/)                     ! Wirte enter to file
! Close Output File
CLOSE(10)

END SUBROUTINE dataOutput
!--------------------------------------------------------------
!--------------------------------------------------------------
