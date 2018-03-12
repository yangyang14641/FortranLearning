MODULE vectors
!
!  Purpose:
!    To define a derived data type called vector, and the
!    operations which can be performed on it.  The module
!    defines 8 operations which can be performed on vectors:
!
!                  Operation                     Operator
!                  =========                     ========
!      1.  Creation from a real array               =
!      2.  Conversion to real array                 =
!      3.  Vector addition                          +
!      4.  Vector subtraction                       -
!      5.  Vector-scalar multiplication (4 cases)   *
!      6.  Vector-scalar division (2 cases)         /
!      7.  Dot product                            .DOT. 
!      8.  Cross product                            *
!
!    It contains a total of 12 procedures to implement those
!    operations:  array_to_vector, vector_to_array, vector_add,
!    vector_subtract, vector_times_real, real_times_vector,
!    vector_times_int, int_times_vector, vector_div_real,
!    vector_div_int, dot_product, and cross_product.  These
!    procedures are private to the module; they can only be
!    accessed from the outside via the defined operators.
!
!  Record of revisions:
!       Date       Programmer          Description of change
!       ====       ==========          =====================
!     12/15/06    S. J. Chapman        Original code
! 1.  12/16/06    S. J. Chapman        Modified to hide non-
!                                        essential items.
!
IMPLICIT NONE

! Declare all items to be private except for type vector and
! the operators defined for it.
PRIVATE
PUBLIC :: vector, assignment(=), operator(+), operator(-), &
          operator(*), operator(/), operator(.DOT.)

! Declare vector data type:
TYPE :: vector
   REAL :: x
   REAL :: y
   REAL :: z
END TYPE

! Declare interface operators
INTERFACE ASSIGNMENT (=)
   MODULE PROCEDURE array_to_vector
   MODULE PROCEDURE vector_to_array
END INTERFACE

INTERFACE OPERATOR (+)
   MODULE PROCEDURE vector_add
END INTERFACE

INTERFACE OPERATOR (-)
   MODULE PROCEDURE vector_subtract
END INTERFACE

INTERFACE OPERATOR (*)
   MODULE PROCEDURE vector_times_real
   MODULE PROCEDURE real_times_vector
   MODULE PROCEDURE vector_times_int
   MODULE PROCEDURE int_times_vector
   MODULE PROCEDURE cross_product
END INTERFACE

INTERFACE OPERATOR (/)
   MODULE PROCEDURE vector_div_real
   MODULE PROCEDURE vector_div_int
END INTERFACE

INTERFACE OPERATOR (.DOT.)
   MODULE PROCEDURE dot_product
END INTERFACE


! Now define the implementing functions.
CONTAINS
   SUBROUTINE array_to_vector(vec_result, array)
      TYPE (vector), INTENT(OUT) :: vec_result
      REAL, DIMENSION(3), INTENT(IN) :: array
      vec_result%x = array(1)
      vec_result%y = array(2)
      vec_result%z = array(3)
   END SUBROUTINE array_to_vector


   SUBROUTINE vector_to_array(array_result, vec_1)
      REAL, DIMENSION(3), INTENT(OUT) :: array_result
      TYPE (vector), INTENT(IN) :: vec_1
      array_result(1) = vec_1%x
      array_result(2) = vec_1%y
      array_result(3) = vec_1%z
   END SUBROUTINE vector_to_array


   FUNCTION vector_add(vec_1, vec_2)
      TYPE (vector) :: vector_add
      TYPE (vector), INTENT(IN) :: vec_1, vec_2
      vector_add%x = vec_1%x + vec_2%x
      vector_add%y = vec_1%y + vec_2%y
      vector_add%z = vec_1%z + vec_2%z
   END FUNCTION vector_add


   FUNCTION vector_subtract(vec_1, vec_2)
      TYPE (vector) :: vector_subtract
      TYPE (vector), INTENT(IN) :: vec_1, vec_2
      vector_subtract%x = vec_1%x - vec_2%x
      vector_subtract%y = vec_1%y - vec_2%y
      vector_subtract%z = vec_1%z - vec_2%z
   END FUNCTION vector_subtract


   FUNCTION vector_times_real(vec_1, real_2)
      TYPE (vector) :: vector_times_real
      TYPE (vector), INTENT(IN) :: vec_1
      REAL, INTENT(IN) :: real_2
      vector_times_real%x = vec_1%x * real_2
      vector_times_real%y = vec_1%y * real_2
      vector_times_real%z = vec_1%z * real_2
   END FUNCTION vector_times_real


   FUNCTION real_times_vector(real_1, vec_2)
      TYPE (vector) :: real_times_vector
      REAL, INTENT(IN) :: real_1
      TYPE (vector), INTENT(IN) :: vec_2
      real_times_vector%x = real_1 * vec_2%x
      real_times_vector%y = real_1 * vec_2%y
      real_times_vector%z = real_1 * vec_2%z
   END FUNCTION real_times_vector


   FUNCTION vector_times_int(vec_1, int_2)
      TYPE (vector) :: vector_times_int
      TYPE (vector), INTENT(IN) :: vec_1
      INTEGER, INTENT(IN) :: int_2
      vector_times_int%x = vec_1%x * REAL(int_2)
      vector_times_int%y = vec_1%y * REAL(int_2)
      vector_times_int%z = vec_1%z * REAL(int_2)
   END FUNCTION vector_times_int


   FUNCTION int_times_vector(int_1, vec_2)
      TYPE (vector) :: int_times_vector
      INTEGER, INTENT(IN) :: int_1
      TYPE (vector), INTENT(IN) :: vec_2
      int_times_vector%x = REAL(int_1) * vec_2%x
      int_times_vector%y = REAL(int_1) * vec_2%y
      int_times_vector%z = REAL(int_1) * vec_2%z
   END FUNCTION int_times_vector


   FUNCTION vector_div_real(vec_1, real_2)
      TYPE (vector) :: vector_div_real
      TYPE (vector), INTENT(IN) :: vec_1
      REAL, INTENT(IN) :: real_2
      vector_div_real%x = vec_1%x / real_2
      vector_div_real%y = vec_1%y / real_2
      vector_div_real%z = vec_1%z / real_2
   END FUNCTION vector_div_real


   FUNCTION vector_div_int(vec_1, int_2)
      TYPE (vector) :: vector_div_int
      TYPE (vector), INTENT(IN) :: vec_1
      INTEGER, INTENT(IN) :: int_2
      vector_div_int%x = vec_1%x / REAL(int_2)
      vector_div_int%y = vec_1%y / REAL(int_2)
      vector_div_int%z = vec_1%z / REAL(int_2)
   END FUNCTION vector_div_int


   FUNCTION dot_product(vec_1, vec_2)
      REAL :: dot_product
      TYPE (vector), INTENT(IN) :: vec_1, vec_2
      dot_product = vec_1%x*vec_2%x + vec_1%y*vec_2%y &
                  + vec_1%z*vec_2%z 
   END FUNCTION dot_product


   FUNCTION cross_product(vec_1, vec_2)
      TYPE (vector) :: cross_product
      TYPE (vector), INTENT(IN) :: vec_1, vec_2
      cross_product%x = vec_1%y*vec_2%z - vec_1%z*vec_2%y
      cross_product%y = vec_1%z*vec_2%x - vec_1%x*vec_2%z
      cross_product%z = vec_1%x*vec_2%y - vec_1%y*vec_2%x
   END FUNCTION cross_product

END MODULE vectors
