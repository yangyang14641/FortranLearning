PROGRAM extremes
!
!  Purpose:
!    To find the largest and smallest values in a data set,
!    and to print out the data set with the largest and smallest
!    values labeled.
!
!  Record of revisions:
!      Date       Programmer          Description of change
!      ====       ==========          =====================
!    11/15/06    S. J. Chapman        Original code
!
IMPLICIT NONE

! Data dictionary: declare constants  
INTEGER, PARAMETER :: MAX_SIZE = 10    ! Max size of data set

! Data dictionary: declare variable types, definitions, & units  
INTEGER, DIMENSION(MAX_SIZE) :: input  ! Input values
INTEGER :: ilarge                      ! Pointer to largest value
INTEGER :: ismall                      ! Pointer to smallest value
INTEGER :: j                           ! DO loop index
INTEGER :: nvals                       ! Number of vals in data set
INTEGER :: temp                        ! Temporary variable

! Get number of values in data set
WRITE (*,*) 'Enter number of values in data set:'
READ (*,*) nvals
 
! Is the number <= MAX_SIZE?
size: IF ( nvals <= MAX_SIZE ) THEN 
 
   ! Get input values.
   in: DO J = 1, nvals
      WRITE (*,100) 'Enter value ', j
      100 FORMAT (' ',A,I3,': ')
      READ (*,*) input(j)
   END DO in
 
   ! Find the largest value.
   temp = input(1)
   ilarge = 1
   large: DO j = 2, nvals
      IF ( input(j) > temp ) THEN
        temp = input(j)
        ilarge = j
      END IF
   END DO large
 
   ! Find the smallest value.
   temp = input(1)
   ismall = 1
   small: DO j = 2, nvals
      IF ( input(j) < temp ) THEN
        temp = input(j)
        ismall = j
      END IF
   END DO small
 
   ! Write out list.
   WRITE (*,110) 
   110 FORMAT ('0','The values are:')
   out: DO j = 1, nvals
      IF ( j == ilarge ) THEN
         WRITE (*,'(1X,I6,2X,A)') input(j), 'LARGEST'
      ELSE IF ( J == ismall ) THEN
         WRITE (*,'(1X,I6,2X,A)') input(j), 'SMALLEST'
      ELSE
         WRITE (*,'(1X,I6)') input(j)
      END IF
   END DO out
 
ELSE size
 
   ! nvals > max_size.  Tell user and quit.
   WRITE (*,120) nvals, MAX_SIZE
   120 FORMAT (1X,'Too many input values: ', I6, ' > ', I6)
 
END IF size
 
END PROGRAM extremes

