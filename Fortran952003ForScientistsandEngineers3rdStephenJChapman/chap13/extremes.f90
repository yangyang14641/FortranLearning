MODULE procs

CONTAINS
   SUBROUTINE extremes(a, n, maxval, pos_maxval, minval, pos_minval)
   !
   !  Purpose:
   !    To find the maximum and minimum values in an array, and 
   !    the location of those values in the array.  This subroutine
   !    returns its output values in optional arguments.
   !
   !  Record of revisions:
   !       Date       Programmer          Description of change
   !       ====       ==========          =====================
   !     12/08/06    S. J. Chapman        Original code
   !
   IMPLICIT NONE

   ! Data dictionary: declare calling parameter types & definitions
   INTEGER, INTENT(IN) :: n                     ! # vals in array a
   REAL, INTENT(IN), DIMENSION(n) :: a          ! Input data.
   REAL, INTENT(OUT), OPTIONAL :: maxval        ! Maximum value.
   INTEGER, INTENT(OUT), OPTIONAL :: pos_maxval ! Pos of maxval
   REAL, INTENT(OUT), OPTIONAL :: minval        ! Minimum value.
   INTEGER, INTENT(OUT), OPTIONAL :: pos_minval ! Pos of minval
   
   ! Data dictionary: declare local variable types & definitions
   INTEGER :: i                            ! Index
   REAL :: real_max                        ! Max value
   INTEGER :: pos_max                      ! Pos of max value
   REAL :: real_min                        ! Min value
   INTEGER :: pos_min                      ! Pos of min value

   ! Initialize the values to first value in array.
   real_max = a(1)
   pos_max = 1
   real_min = a(1)
   pos_min = 1
 
   ! Find the extreme values in a(2) through a(n).
   DO i = 2, n
      max: IF ( a(i) > real_max ) THEN
         real_max = a(i)
         pos_max = i
      END IF max
      min: IF ( a(i) < real_min ) THEN
         real_min = a(i)
         pos_min = i
      END IF min
   END DO

   ! Report the results
   IF ( PRESENT(maxval) ) THEN
      maxval = real_max
   END IF
   IF ( PRESENT(pos_maxval) ) THEN
      pos_maxval = pos_max
   END IF
   IF ( PRESENT(minval) ) THEN
      minval = real_min
   END IF
   IF ( PRESENT(pos_minval) ) THEN
      pos_minval = pos_min
   END IF

   END SUBROUTINE extremes
END MODULE procs
