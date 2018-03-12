PROGRAM stock
! 
!  Purpose: 
!    To maintain an inventory of stockroom supplies, and generate
!    warning messages when supplies get low.
! 
!  Record of revisions:
!      Date       Programmer          Description of change
!      ====       ==========          =====================
!    12/17/06    S. J. Chapman        Original code
!
IMPLICIT NONE

! Data dictionary: declare constants
INTEGER, PARAMETER :: LU_DB = 7       ! Unit for db file
INTEGER, PARAMETER :: LU_M = 8        ! Unit for message file
INTEGER, PARAMETER :: LU_T = 9        ! Unit for trans file

! Declare derived data type for a database item
TYPE :: database_record
   INTEGER :: stock_number            ! Item number
   CHARACTER(len=30) :: description   ! Description of item
   CHARACTER(len=10) :: vendor        ! Vendor of item
   CHARACTER(len=20) :: vendor_number ! Vendor stock number
   INTEGER :: number_in_stock         ! Number in stock
   INTEGER :: minimum_quanitity       ! Minimum quantity
END TYPE

! Declare derived data type for transaction
TYPE :: transaction_record
   INTEGER :: stock_number            ! Item number
   INTEGER :: number_in_transaction   ! Number in transaction
END TYPE

! Data dictionary: declare variable types & definitions
TYPE (database_record) :: item        ! Database item
TYPE (transaction_record) :: trans    ! Transaction item
CHARACTER(len=3) :: file_stat         ! File status
INTEGER :: istat                      ! I/O status
LOGICAL :: exist                      ! True if file exists

CHARACTER(len=24) :: db_file = 'stock.db'    ! Database file
CHARACTER(len=24) :: msg_file = 'stock.msg'  ! Message file
CHARACTER(len=24) :: trn_file = 'stock.trn'  ! Trans. file

! Begin execution: open database file, and check for error.
OPEN (LU_DB, FILE=db_file, STATUS='OLD', ACCESS='DIRECT', &
      FORM='FORMATTED', RECL=78, IOSTAT=istat )
IF ( istat /= 0 ) THEN
   WRITE (*,100) db_file, istat
   100 FORMAT (' Open failed on file ',A,'. IOSTAT = ',I6)
   STOP
END IF
 
! Open transaction file, and check for error.
OPEN (LU_T, FILE=trn_file, STATUS='OLD', ACCESS='SEQUENTIAL', &
     IOSTAT=istat )
IF ( istat /= 0 ) THEN
   WRITE (*,100) trn_file, istat
   STOP
END IF
 
! Open message file, and position file pointer at end of file.
! Check for error.
INQUIRE (FILE=msg_file,EXIST=exist)  ! Does the msg file exist?
IF ( exist ) THEN
   file_stat = 'OLD'                 ! Yes, append to it.
ELSE
   file_stat = 'NEW'                 ! No, create it.
END IF
OPEN (LU_M, FILE=msg_file, STATUS=file_stat, POSITION='APPEND', &
      ACCESS='SEQUENTIAL', IOSTAT=istat )
IF ( istat /= 0 ) THEN
   WRITE (*,100) msg_file, istat
   STOP
END IF

! Now begin processing loop for as long as transactions exist.
process: DO 
   ! Read transaction.
   READ (LU_T,*,IOSTAT=istat) trans

   ! If we are at the end of the data, exit now.
   IF ( istat /= 0 ) EXIT

   ! Get database record, and check for error.
   READ (LU_DB,'(A6,A30,A10,A20,I6,I6)',REC=trans%stock_number, &
         IOSTAT=istat) item 
   IF ( istat /= 0 ) THEN
      WRITE (*,'(A,I6,A,I6)') &
           ' Read failed on database file record ', &
             trans%stock_number, ' IOSTAT = ', istat
      STOP
   END IF
 
   ! Read ok, so update record.
   item%number_in_stock = item%number_in_stock &
                        + trans%number_in_transaction
 
   ! Check for errors.
   IF ( item%number_in_stock < 0 ) THEN
      ! Write error message & reset quantity to zero.
      WRITE (LU_M,'(A,I6,A)') ' ERROR: Stock number ', &
             trans%stock_number, ' has quantity < 0! '
      item%number_in_stock = 0
   END IF
 
   ! Check for quantities < minimum.
   IF ( item%number_in_stock < item%minimum_quanitity ) THEN
      ! Write reorder message to message file.
      WRITE (LU_M,110) ' Reorder stock number ', &
             trans%stock_number, ' from vendor ', &
             item%vendor, '   Description:  ', &
             item%description
      110 FORMAT (A,I6,A,A,/,A,A)
   END IF
 
   ! Update database record
   WRITE (LU_DB,'(A6,A30,A10,A20,I6,I6)',REC=trans%stock_number, &
         IOSTAT=istat) item 

END DO process

! End of updates.  Close files and exit.
CLOSE ( LU_DB )
CLOSE ( LU_T )
CLOSE ( LU_M )

END PROGRAM stock
