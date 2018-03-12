PROGRAM direct_access
!
!  Purpose:
!    To compare direct access formatted and unformatted files.
!
!  Record of revisions:
!      Date       Programmer          Description of change
!      ====       ==========          =====================
!    12/11/06    S. J. Chapman        Original code
!
IMPLICIT NONE

! List of parameters:
INTEGER, PARAMETER :: SINGLE = SELECTED_REAL_KIND(p=6)
INTEGER, PARAMETER :: DOUBLE = SELECTED_REAL_KIND(p=14)
INTEGER, PARAMETER :: MAX_RECORDS = 20000     ! Max # of records
INTEGER, PARAMETER :: NUMBER_OF_READS = 50000 ! # of reads

! Data dictionary: declare variable types & definitions
INTEGER :: i, j                ! Index variable
INTEGER :: length_fmt = 80     ! Length of each record in 
                               !   formatted file
INTEGER :: length_unf          ! Length of each record in 
                               !   unformatted file
INTEGER :: irec                ! Number of record in file
REAL(KIND=SINGLE) :: time_fmt  ! Time for formatted reads
REAL(KIND=SINGLE) :: time_unf  ! Time for unformatted reads
REAL(KIND=SINGLE) :: value     ! Value returned from random0
REAL(KIND=DOUBLE), DIMENSION(4) :: values ! Values in record

! Get the length of each record in the unformatted file.
INQUIRE (IOLENGTH=length_unf) values
WRITE (*,'(A,I2)') ' The unformatted record length is ', &
                   length_unf
WRITE (*,'(A,I2)') ' The formatted record length is ', &
                   length_fmt

! Open a direct access unformatted file.
OPEN ( UNIT=8, FILE='dirio.unf', ACCESS='DIRECT', &
       FORM='UNFORMATTED', STATUS='REPLACE', RECL=length_unf )

! Open a direct access formatted file.
OPEN ( UNIT=9, FILE='dirio.fmt', ACCESS='DIRECT', &
       FORM='FORMATTED', STATUS='REPLACE', RECL=length_fmt )
 
! Generate records and insert into each file.
DO i = 1, MAX_RECORDS
   DO j = 1, 4
      CALL random0(value)                ! Generate records
      values(j) = 30._double * value
   END DO
   WRITE (8,REC=i) values                ! Write unformatted
   WRITE (9,'(4ES20.14)',REC=i) values   ! Write formatted
END DO

! Measure the time to recover random records from the 
! unformatted file.
CALL set_timer
DO i = 1, NUMBER_OF_READS
   CALL random0(value)
   irec = (MAX_RECORDS-1) * value + 1
   READ (8,REC=irec) values
END DO
CALL elapsed_time (time_unf)

! Measure the time to recover random records from the 
! formatted file.
CALL set_timer
DO i = 1, NUMBER_OF_READS
   CALL random0(value)
   irec = (MAX_RECORDS-1) * value + 1
   READ (9,'(4ES20.14)',REC=irec) values
END DO
CALL elapsed_time (time_fmt)

! Tell user.
WRITE (*,'(A,F6.2)') ' Time for unformatted file = ', time_unf
WRITE (*,'(A,F6.2)') ' Time for formatted file =   ', time_fmt

END PROGRAM direct_access
