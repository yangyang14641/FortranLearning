PROGRAM generate
!
!  Purpose:
!    To calculate total instantaneous power supplied by a generating
!    station at each instant of time, and to calculate the average
!    power supplied by each generator over the period of measurement.
!
!  Record of revisions:
!      Date       Programmer          Description of change
!      ====       ==========          =====================
!    11/19/06    S. J. Chapman        Original code
!
IMPLICIT NONE

! Data dictionary: declare constants
INTEGER, PARAMETER :: MAX_GEN = 4     ! Max number of generators
INTEGER, PARAMETER :: MAX_TIME = 6    ! Max number of times

! Data dictionary: declare variable types, definitions, & units
CHARACTER(len=20) :: filename          ! Input data file name
INTEGER :: igen                        ! Loop index: generators
INTEGER :: itime                       ! Loop index: time
REAL, DIMENSION(MAX_TIME,MAX_GEN) :: power 
                                       ! Pwr of each gen at each time (MW)
REAL, DIMENSION(MAX_GEN) :: power_ave  ! Ave power of each gen (MW)
REAL, DIMENSION(MAX_TIME) :: power_sum ! Total power at each time (MW)
INTEGER :: status                      ! I/O status: 0 = success

! Initialize sums to zero.
power_ave = 0.
power_sum = 0.
 
! Get the name of the file containing the input data.
WRITE (*,1000) 
1000 FORMAT (' Enter the file name containing the input data: ')
READ (*,'(A20)') filename
 
! Open input data file.  Status is OLD because the input data must 
! already exist.
OPEN ( UNIT=9, FILE=filename, STATUS='OLD', ACTION='READ', &
       IOSTAT=status )
 
! Was the OPEN successful? 
fileopen: IF ( status == 0 ) THEN
 
   ! The file was opened successfully, so read the data to process. 
   READ (9, *, IOSTAT=status) power
 
   ! Calculate the instantaneous output power of the station at
   ! each time.
   sum1: DO itime = 1, MAX_TIME
      sum2: DO igen = 1, MAX_GEN
         power_sum(itime) = power(itime,igen) + power_sum(itime) 
      END DO sum2
   END DO sum1
 
   ! Calculate the average output power of each generator over the
   ! time being measured.
   ave1: DO igen = 1, MAX_GEN
      ave2: DO itime = 1, MAX_TIME
         power_ave(igen) = power(itime,igen) + power_ave(igen) 
      END DO ave2
      power_ave(igen) = power_ave(igen) / REAL(MAX_TIME)
   END DO ave1
 
   ! Tell user.
   out1: DO itime = 1, MAX_TIME
      WRITE (*,1010) itime, power_sum(itime)
      1010 FORMAT (' The instantaneous power at time ', I1, ' is ', &
                     F7.2, ' MW.')
   END DO out1
 
   out2: DO igen = 1, MAX_GEN
      WRITE (*,1020) igen, power_ave(igen) 
      1020 FORMAT (' The average power of generator  ', I1, ' is ', &
                     F7.2, ' MW.')
   END DO out2
 
ELSE fileopen

   ! Else file open failed.  Tell user.
   WRITE (*,1030) status
   1030 FORMAT (1X,'File open failed--status = ', I6)

END IF fileopen
 
END PROGRAM generate
