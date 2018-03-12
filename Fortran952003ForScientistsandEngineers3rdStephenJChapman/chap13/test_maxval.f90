PROGRAM test_maxval
!
!  Purpose:
!    To test the generic subroutine maxval with five different types
!    of input data sets.
!
!  Record of revisions:
!      Date       Programmer          Description of change
!      ====       ==========          =====================
!    12/15/06    S. J. Chapman        Original code
!
USE generic_maxval
IMPLICIT NONE

! Data dictionary: declare variable types & definitions
INTEGER, DIMENSION(6) :: array_i            ! Integer array
REAL(KIND=SGL), DIMENSION(6) :: array_r     ! Sng prec real arr
REAL(KIND=DBL), DIMENSION(6) :: array_d     ! Dbl prec real arr
COMPLEX(KIND=SGL), DIMENSION(6) :: array_c  ! Sing. prec. cx arr
COMPLEX(KIND=DBL), DIMENSION(6) :: array_dc ! Sing. prec. cx arr
INTEGER :: value_max_i                      ! Max value
REAL(KIND=SGL) :: value_max_r               ! Max value
REAL(KIND=DBL) :: value_max_d               ! Max value
INTEGER :: pos_maxval                       ! Pos of max value

! Initialize arrays
array_i  = (/ -13,  3,  2,  0,  25,  -2  /)
array_r  = (/ -13., 3., 2., 0., 25., -2. /)
array_d  = (/ -13._DBL, 3._DBL, 2._DBL, 0._DBL, 25._DBL, -2._DBL /)
array_c  = (/ (1.,2.), (-4.,-6.), (4.,-7), (3.,4.), &
              (0.,1.), (6.,-8.) /)
array_dc = (/ (1._DBL,2._DBL), (-4._DBL,-6._DBL), &
              (4._DBL,-7._DBL), (3._DBL,4._DBL), &
              (0._DBL,1._DBL), (6._DBL,-8._DBL) /)

! Test integer subroutine.  Include optional argument.
CALL maxval ( array_i, 6, value_max_i, pos_maxval )
WRITE (*,1000) value_max_i, pos_maxval
1000 FORMAT (' Integer args: max value = ',I3, &
             ';  position = ', I3 )

! Test SGL prec real subroutine.  Leave out optional arg.
CALL maxval ( array_r, 6, value_max_r )
WRITE (*,1010) value_max_r
1010 FORMAT (' Single prec real args: max value = ',F7.3)

! Test DBL prec real subroutine.  Use keywords.
CALL maxval ( ARRAY=array_d, NVALS=6, VALUE_MAX=value_max_d )
WRITE (*,1020) value_max_d
1020 FORMAT (' Double prec real args: max value = ',F7.3)

! Test SGL prec cmplx subroutine.  Use scrambled keywords.
CALL maxval ( NVALS=6, ARRAY=array_c, VALUE_MAX=value_max_r, &
              POS_MAXVAL=pos_maxval )
WRITE (*,1030) value_max_r, pos_maxval
1030 FORMAT (' Single precision complex args:' &
             ' max abs value = ',F7.3, &
             ';  position = ', I3 )

! Test DBL prec cmplx subroutine.  Leave out optional arg.
CALL maxval ( array_dc, 6, value_max_d )
WRITE (*,1040) value_max_r
1040 FORMAT (' Double precision complex args:' &
             ' max abs value = ',F7.3 )
 
END PROGRAM test_maxval
