MODULE generic_maxval
!
!  Purpose:
!    To produce a generic procedure maxval that returns the
!    maximum value in an array and optionally the location
!    of that maximum value for the following input data types:
!    integer, single precision real, double precision real,
!    single precision complex, and double precision complex.
!    Complex comparisons are done on the absolute values of
!    values in the input array.
!
!  Record of revisions:
!       Date       Programmer          Description of change
!       ====       ==========          =====================
!     12/15/06    S. J. Chapman        Original code
!
IMPLICIT NONE

! Declare parameters:
INTEGER, PARAMETER :: SGL = SELECTED_REAL_KIND(p=6)
INTEGER, PARAMETER :: DBL = SELECTED_REAL_KIND(p=13)

! Declare generic interface.
INTERFACE maxval
   MODULE PROCEDURE maxval_i
   MODULE PROCEDURE maxval_r
   MODULE PROCEDURE maxval_d
   MODULE PROCEDURE maxval_c
   MODULE PROCEDURE maxval_dc
END INTERFACE

CONTAINS 
   SUBROUTINE maxval_i ( array, nvals, value_max, pos_maxval )
   IMPLICIT NONE

   ! List of calling arguments:
   INTEGER, INTENT(IN) :: nvals                     ! # vals.
   INTEGER, INTENT(IN), DIMENSION(nvals) :: array   ! Input data.
   INTEGER, INTENT(OUT) :: value_max                ! Max value.
   INTEGER, INTENT(OUT), OPTIONAL :: pos_maxval     ! Position
   
   !  List of local variables:
   INTEGER :: i                            ! Index
   INTEGER :: pos_max                      ! Pos of max value

   ! Initialize the values to first value in array.
   value_max = array(1)
   pos_max = 1
 
   ! Find the extreme values in array(2) through array(nvals).
   DO i = 2, nvals
      IF ( array(i) > value_max ) THEN
         value_max = array(i)
         pos_max = i
      END IF
   END DO

   ! Report the results
   IF ( PRESENT(pos_maxval) ) THEN
      pos_maxval = pos_max
   END IF

   END SUBROUTINE maxval_i

   SUBROUTINE maxval_r ( array, nvals, value_max, pos_maxval )
   IMPLICIT NONE

   ! List of calling arguments:
   INTEGER, INTENT(IN) :: nvals                             
   REAL(KIND=SGL), INTENT(IN), DIMENSION(nvals) :: array   
   REAL(KIND=SGL), INTENT(OUT) :: value_max                        
   INTEGER, INTENT(OUT), OPTIONAL :: pos_maxval
   
   !  List of local variables:
   INTEGER :: i                            ! Index
   INTEGER :: pos_max                      ! Pos of max value

   ! Initialize the values to first value in array.
   value_max = array(1)
   pos_max = 1
 
   ! Find the extreme values in array(2) through array(nvals).
   DO i = 2, nvals
      IF ( array(i) > value_max ) THEN
         value_max = array(i)
         pos_max = i
      END IF
   END DO

   ! Report the results
   IF ( PRESENT(pos_maxval) ) THEN
      pos_maxval = pos_max
   END IF

   END SUBROUTINE maxval_r

   SUBROUTINE maxval_d ( array, nvals, value_max, pos_maxval )
   IMPLICIT NONE

   ! List of calling arguments:
   INTEGER, INTENT(IN) :: nvals                             
   REAL(KIND=DBL), INTENT(IN), DIMENSION(nvals) :: array   
   REAL(KIND=DBL), INTENT(OUT) :: value_max                        
   INTEGER, INTENT(OUT), OPTIONAL :: pos_maxval
   
   !  List of local variables:
   INTEGER :: i                            ! Index
   INTEGER :: pos_max                      ! Pos of max value

   ! Initialize the values to first value in array.
   value_max = array(1)
   pos_max = 1
 
   ! Find the extreme values in array(2) through array(nvals).
   DO i = 2, nvals
      IF ( array(i) > value_max ) THEN
         value_max = array(i)
         pos_max = i
      END IF
   END DO

   ! Report the results
   IF ( PRESENT(pos_maxval) ) THEN
      pos_maxval = pos_max
   END IF

   END SUBROUTINE maxval_d

   SUBROUTINE maxval_c ( array, nvals, value_max, pos_maxval )
   IMPLICIT NONE

   ! List of calling arguments:
   INTEGER, INTENT(IN) :: nvals                             
   COMPLEX(KIND=SGL), INTENT(IN), DIMENSION(nvals) :: array   
   REAL(KIND=SGL), INTENT(OUT) :: value_max                        
   INTEGER, INTENT(OUT), OPTIONAL :: pos_maxval
   
   !  List of local variables:
   INTEGER :: i                            ! Index
   INTEGER :: pos_max                      ! Pos of max value

   ! Initialize the values to first value in array.
   value_max = ABS(array(1))
   pos_max = 1
 
   ! Find the extreme values in array(2) through array(nvals).
   DO i = 2, nvals
      IF ( ABS(array(i)) > value_max ) THEN
         value_max = ABS(array(i))
         pos_max = i
      END IF
   END DO

   ! Report the results
   IF ( PRESENT(pos_maxval) ) THEN
      pos_maxval = pos_max
   END IF

   END SUBROUTINE maxval_c

   SUBROUTINE maxval_dc ( array, nvals, value_max, pos_maxval )
   IMPLICIT NONE

   ! List of calling arguments:
   INTEGER, INTENT(IN) :: nvals                             
   COMPLEX(KIND=DBL), INTENT(IN), DIMENSION(nvals) :: array   
   REAL(KIND=DBL), INTENT(OUT) :: value_max                        
   INTEGER, INTENT(OUT), OPTIONAL :: pos_maxval
   
   !  List of local variables:
   INTEGER :: i                            ! Index
   INTEGER :: pos_max                      ! Pos of max value

   ! Initialize the values to first value in array.
   value_max = ABS(array(1))
   pos_max = 1
 
   ! Find the extreme values in array(2) through array(nvals).
   DO i = 2, nvals
      IF ( ABS(array(i)) > value_max ) THEN
         value_max = ABS(array(i))
         pos_max = i
      END IF
   END DO

   ! Report the results
   IF ( PRESENT(pos_maxval) ) THEN
      pos_maxval = pos_max
   END IF

   END SUBROUTINE maxval_dc

END MODULE generic_maxval
