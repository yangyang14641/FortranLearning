! Copyright (c) 1993 Unicomp, Inc.
!
! Developed at Unicomp, Inc.
!
! Permission to use, copy, modify, and distribute this
! software is freely granted, provided that this notice 
! is preserved.

! The module named BIG_INTEGERS defines a new data type
! BIG_INTEGER.  This data type represents nonnegative integers
! up to 10 ** n-1, where n is the parameter (named constant)
! NR_OF_DECIMAL_DIGITS.  This value may be changed, but the
! module must then be recompiled (it is not dynamic).  The
! module also defines the following operations, where b
! represents a big_integer, i represents an ordinary
! Fortran integer, and c represents a Fortran character
! string.
! 
! big_int (i), big_int (b), big_int (c)
! b = i, b = c
! b + i, i + b, b + b
! b - i, b - b
! b * i, i * b, b * b
! b / i
! b ** i
! b ? i, i ? b, b ? b, where ? is ==. /=, <=, <, >=, >
! huge (b)
! modulo (b, i)
! digits (b)
! radix (b)
! range (b)
! sqrt (b)
! prime (b)
! factorial (b)
! call print (b)
! call print_factors (b)

MODULE BIG_INTEGERS

   IMPLICIT NONE

   PRIVATE

   INTEGER, PARAMETER :: &
         NR_OF_DECIMAL_DIGITS = 50, &
         D = (RANGE (0) - 1) / 2, &
         BASE = 10 ** D, &
         NR_OF_DIGITS = NR_OF_DECIMAL_DIGITS / D + 1

   ! Base of number system is 10 ** D,
   ! so that each "DIGIT" is 0 to 10**D - 1

   TYPE, PUBLIC :: BIG_INTEGER
      PRIVATE
      INTEGER, DIMENSION (0 : NR_OF_DIGITS) &
            :: DIGIT
   END TYPE BIG_INTEGER

   INTERFACE BIG_INT
      MODULE PROCEDURE BIG_INT_INT, &
                       BIG_INT_CHAR, &
                       BIG_INT_B
   END INTERFACE
   PUBLIC BIG_INT

   INTERFACE ASSIGNMENT (=)
      MODULE PROCEDURE BIG_GETS_INT, &
                       BIG_GETS_CHAR
   END INTERFACE
   PUBLIC ASSIGNMENT (=)

   INTERFACE OPERATOR (+)
      MODULE PROCEDURE BIG_PLUS_INT, &
                       INT_PLUS_BIG, &
                       BIG_PLUS_BIG
   END INTERFACE
   PUBLIC OPERATOR (+)

   INTERFACE OPERATOR (-)
      MODULE PROCEDURE BIG_MINUS_INT, &
                       BIG_MINUS_BIG
   END INTERFACE
   PUBLIC OPERATOR (-)

   INTERFACE OPERATOR (*)
      MODULE PROCEDURE BIG_TIMES_INT, &
                       INT_TIMES_BIG, &
                       BIG_TIMES_BIG
   END INTERFACE
   PUBLIC OPERATOR (*)

   INTERFACE OPERATOR (/)
      MODULE PROCEDURE BIG_DIV_INT, &
                       BIG_DIV_BIG
   END INTERFACE
   PUBLIC OPERATOR (/)

   INTERFACE OPERATOR (**)
      MODULE PROCEDURE BIG_POWER_INT
   END INTERFACE
   PUBLIC OPERATOR (**)

   INTERFACE OPERATOR (==)
      MODULE PROCEDURE BIG_EQ_INT, &
                       INT_EQ_BIG, &
                       BIG_EQ_BIG
   END INTERFACE
   PUBLIC OPERATOR (==)

   INTERFACE OPERATOR (/=)
      MODULE PROCEDURE BIG_NE_INT, &
                       INT_NE_BIG, &
                       BIG_NE_BIG
   END INTERFACE
   PUBLIC OPERATOR (/=)

   INTERFACE OPERATOR (<=)
      MODULE PROCEDURE BIG_LE_INT, &
                       INT_LE_BIG, &
                       BIG_LE_BIG
   END INTERFACE
   PUBLIC OPERATOR (<=)

   INTERFACE OPERATOR (>=)
      MODULE PROCEDURE BIG_GE_INT, &
                       INT_GE_BIG, &
                       BIG_GE_BIG
   END INTERFACE
   PUBLIC OPERATOR (>=)

   INTERFACE OPERATOR (<)
      MODULE PROCEDURE BIG_LT_INT, &
                       INT_LT_BIG, &
                       BIG_LT_BIG
   END INTERFACE
   PUBLIC OPERATOR (<)

   INTERFACE OPERATOR (>)
      MODULE PROCEDURE BIG_GT_INT, &
                       INT_GT_BIG, &
                       BIG_GT_BIG
   END INTERFACE
   PUBLIC OPERATOR (>)

   INTERFACE HUGE
      MODULE PROCEDURE HUGE_BIG
   END INTERFACE
   PUBLIC HUGE

   INTERFACE MODULO
      MODULE PROCEDURE MODULO_BIG_INT, &
                       MODULO_BIG_BIG
   END INTERFACE
   PUBLIC MODULO

   INTERFACE DIGITS
      MODULE PROCEDURE DIGITS_BIG
   END INTERFACE
   PUBLIC DIGITS

   INTERFACE RADIX
      MODULE PROCEDURE RADIX_BIG
   END INTERFACE
   PUBLIC RADIX

   INTERFACE RANGE
      MODULE PROCEDURE RANGE_BIG
   END INTERFACE
   PUBLIC RANGE

   INTERFACE SQRT
      MODULE PROCEDURE SQRT_BIG
   END INTERFACE
   PUBLIC SQRT

   INTERFACE PRINT
      MODULE PROCEDURE PRINT_BIG
   END INTERFACE
   PUBLIC PRINT

   INTERFACE PRIME
      MODULE PROCEDURE PRIME_BIG
   END INTERFACE
   PUBLIC PRIME

   INTERFACE FACTORIAL
      MODULE PROCEDURE FACTORIAL_BIG
   END INTERFACE
   PUBLIC FACTORIAL

   INTERFACE PRINT_FACTORS
      MODULE PROCEDURE PRINT_FACTORS_BIG
   END INTERFACE
   PUBLIC PRINT_FACTORS

CONTAINS

FUNCTION BIG_INT_INT (I) RESULT (B_RESULT)

   INTEGER, INTENT (IN) :: I
   TYPE (BIG_INTEGER) :: B_RESULT

   IF (I < 0) THEN
      B_RESULT = HUGE (B_RESULT)
   ELSE
      B_RESULT % DIGIT (0) = MODULO (I, BASE)
      B_RESULT % DIGIT (1) = I / BASE
      B_RESULT % DIGIT (2:) = 0
   END IF

END FUNCTION BIG_INT_INT

FUNCTION BIG_INT_CHAR (C)  RESULT (B_RESULT)

   CHARACTER (LEN = *), INTENT (IN) :: C
   TYPE (BIG_INTEGER) :: B_RESULT
   INTEGER :: TEMP_DIGIT, N

   B_RESULT % DIGIT = 0
   DO N = 1, LEN (C)
      TEMP_DIGIT = INDEX ("0123456789", C (N:N)) - 1
      IF (TEMP_DIGIT < 0) THEN
         B_RESULT = HUGE (B_RESULT)
      ELSE
         B_RESULT = B_RESULT * 10 + TEMP_DIGIT
      END IF
   END DO

END FUNCTION BIG_INT_CHAR

FUNCTION BIG_INT_B (B)  RESULT (B_RESULT)

   TYPE (BIG_INTEGER), INTENT (IN) :: B
   TYPE (BIG_INTEGER) :: B_RESULT

   B_RESULT = B

END FUNCTION BIG_INT_B

SUBROUTINE BIG_GETS_INT (B, I)

   TYPE (BIG_INTEGER), INTENT (OUT) :: B
   INTEGER, INTENT (IN) :: I

   B = BIG_INT_INT (I)

END SUBROUTINE BIG_GETS_INT

SUBROUTINE BIG_GETS_CHAR (B, C)

   TYPE (BIG_INTEGER), INTENT (OUT) :: B
   CHARACTER (LEN = *), INTENT (IN) :: C

   B = BIG_INT_CHAR (C)

END SUBROUTINE BIG_GETS_CHAR

FUNCTION BIG_PLUS_INT (B, I) RESULT (B_RESULT)

   TYPE (BIG_INTEGER), INTENT (IN) :: B
   INTEGER, INTENT (IN) :: I
   TYPE (BIG_INTEGER) :: B_RESULT

   B_RESULT = B + BIG_INT (I)

END FUNCTION BIG_PLUS_INT

FUNCTION INT_PLUS_BIG (I, B) RESULT (B_RESULT)

   INTEGER, INTENT (IN) :: I
   TYPE (BIG_INTEGER), INTENT (IN) :: B
   TYPE (BIG_INTEGER) :: B_RESULT

   B_RESULT = B + BIG_INT (I)

END FUNCTION INT_PLUS_BIG

FUNCTION BIG_PLUS_BIG (X, Y)  RESULT (B_RESULT)

   TYPE (BIG_INTEGER), INTENT (IN) :: X, Y
   TYPE (BIG_INTEGER) :: B_RESULT
   INTEGER :: CARRY, TEMP_DIGIT, N

   CARRY = 0
   DO N = 0, NR_OF_DIGITS
      TEMP_DIGIT = &
         X % DIGIT (N) + Y % DIGIT (N) + CARRY
      B_RESULT % DIGIT (N) = MODULO (TEMP_DIGIT, BASE)
      CARRY = TEMP_DIGIT / BASE
   END DO

   IF (B_RESULT % DIGIT (NR_OF_DIGITS) /= 0 .OR.  &
         CARRY /= 0) THEN
      B_RESULT = HUGE (B_RESULT)
   END IF

END FUNCTION BIG_PLUS_BIG

FUNCTION BIG_MINUS_INT (B, I)  RESULT (B_RESULT)

   TYPE (BIG_INTEGER), INTENT (IN) :: B
   INTEGER, INTENT (IN) :: I
   TYPE (BIG_INTEGER) :: B_RESULT

   B_RESULT = B - BIG_INT (I)

END FUNCTION BIG_MINUS_INT

FUNCTION BIG_MINUS_BIG (X, Y)  RESULT (B_RESULT)

   TYPE (BIG_INTEGER), INTENT (IN) :: X, Y
   TYPE (BIG_INTEGER) :: B_RESULT
   TYPE (BIG_INTEGER) :: TEMP_BIG
   INTEGER :: N

   TEMP_BIG = X
   DO N = 0, NR_OF_DIGITS - 1
      B_RESULT % DIGIT (N) = TEMP_BIG % DIGIT (N) - Y % DIGIT (N)
      IF (B_RESULT % DIGIT (N) < 0) THEN
         B_RESULT % DIGIT (N) = B_RESULT % DIGIT (N) + BASE
         TEMP_BIG % DIGIT (N + 1) = TEMP_BIG % DIGIT (N + 1) - 1
      END IF
   END DO

   IF (TEMP_BIG % DIGIT (NR_OF_DIGITS) < 0) THEN
      B_RESULT % DIGIT = 0
   ELSE
      B_RESULT % DIGIT (NR_OF_DIGITS) = 0
   END IF

END FUNCTION BIG_MINUS_BIG

FUNCTION BIG_TIMES_INT (B, I)  RESULT (B_RESULT)

   TYPE (BIG_INTEGER), INTENT (IN) :: B
   INTEGER, INTENT (IN) :: I
   TYPE (BIG_INTEGER) :: B_RESULT
   INTEGER :: I0, I1, CARRY, N, TEMP_DIGIT

   I0 = MODULO (I, BASE)
   CARRY = 0
   DO N = 0, NR_OF_DIGITS
      TEMP_DIGIT = B % DIGIT (N) * I0 + CARRY
      B_RESULT % DIGIT (N) = MODULO (TEMP_DIGIT, BASE)
      CARRY = TEMP_DIGIT / BASE
   END DO

   IF (B_RESULT % DIGIT (NR_OF_DIGITS) /= 0 .OR.  &
         CARRY /= 0) THEN
      B_RESULT = HUGE (B_RESULT)
      RETURN
   END IF

   IF (I1 >= BASE) THEN
      I1 = I / BASE
      CARRY = 0
      DO N = 1, NR_OF_DIGITS
         TEMP_DIGIT = B % DIGIT (N-1) * I1 + B_RESULT % DIGIT (N) + CARRY
         B_RESULT % DIGIT (N) = MODULO (TEMP_DIGIT, BASE)
         CARRY = TEMP_DIGIT / BASE
      END DO

      IF (B_RESULT % DIGIT (NR_OF_DIGITS) /= 0 .OR.  &
            CARRY /= 0) THEN
         B_RESULT = HUGE (B_RESULT)
         STOP
      END IF
   END IF

END FUNCTION BIG_TIMES_INT

FUNCTION INT_TIMES_BIG (I, B)  RESULT (B_RESULT)

   TYPE (BIG_INTEGER), INTENT (IN) :: B
   INTEGER, INTENT (IN) :: I
   TYPE (BIG_INTEGER) :: B_RESULT

   B_RESULT = B * I

END FUNCTION INT_TIMES_BIG

FUNCTION BIG_TIMES_BIG (X, Y)  RESULT (B_RESULT)

   TYPE (BIG_INTEGER), INTENT (IN) :: X, Y
   TYPE (BIG_INTEGER) :: B_RESULT
   TYPE (BIG_INTEGER) :: TEMP_BIG
   INTEGER :: N

   B_RESULT % DIGIT = 0
   DO N = NR_OF_DIGITS, 0, -1
      IF (X % DIGIT (N) /= 0) THEN
         TEMP_BIG = Y * (X % DIGIT (N))
         IF (TEMP_BIG == HUGE (TEMP_BIG)) RETURN
         TEMP_BIG % DIGIT = EOSHIFT (TEMP_BIG % DIGIT, -N)
         B_RESULT = B_RESULT + TEMP_BIG
         IF (B_RESULT == HUGE (B_RESULT)) RETURN
      END IF
   END DO

END FUNCTION BIG_TIMES_BIG

FUNCTION BIG_DIV_INT (B, I)  RESULT (B_RESULT)

   TYPE (BIG_INTEGER), INTENT (IN) :: B
   INTEGER, INTENT (IN) :: I
   TYPE (BIG_INTEGER) :: B_RESULT
   INTEGER :: N, TEMP_INT, REMAINDER

   REMAINDER = 0
   DO N = NR_OF_DIGITS, 0, -1
      TEMP_INT = BASE * REMAINDER + B % DIGIT (N)
      B_RESULT % DIGIT (N) = TEMP_INT / I
      REMAINDER = MODULO (TEMP_INT, I)
   END DO

END FUNCTION BIG_DIV_INT

FUNCTION BIG_DIV_BIG (X,Y)  RESULT (B_RESULT)

   TYPE (BIG_INTEGER), INTENT (IN) :: X, Y
   TYPE (BIG_INTEGER) :: B_RESULT
   TYPE (BIG_INTEGER) :: Q
   INTEGER :: HIGH, LOW, MID, B, D, M, N, K

   DO N = NR_OF_DIGITS -1, 0, -1
      IF (Y % DIGIT (N) /= 0) EXIT
   END DO
   IF (N == -1) THEN  ! Denominator = 0
      B_RESULT = HUGE (B_RESULT)
      RETURN
   ELSE IF (N <= 1) THEN
      B_RESULT = X / (BASE * Y % DIGIT (1) + Y % DIGIT (0))
      RETURN
   END IF

   DO M = NR_OF_DIGITS -1, 0, -1
      IF (X % DIGIT (M) /= 0) EXIT
   END DO
   IF (M < N) THEN
      B_RESULT = 0
      RETURN
   END IF

   D = Y % DIGIT (N)
   Q = X
   B_RESULT % DIGIT (M - N + 1 :) = 0

   DO K = M - N, 0, -1
      B = BASE * Q % DIGIT (M + 1) + Q % DIGIT (M)
      HIGH = (B + 1) / D + 1
      LOW = B / (D + 1)
      DO
         IF (LOW >= HIGH - 1) EXIT 
         MID = (HIGH + LOW) / 2
         IF (MID * Y <= X) THEN
            LOW = MID
         ELSE
            HIGH = MID
         END IF
      END DO
      B_RESULT % DIGIT (K) = LOW
      Q = Q - LOW * Y * BIG_BASE_TO_POWER (K)
      M = M - 1
   END DO

END FUNCTION BIG_DIV_BIG

RECURSIVE FUNCTION BIG_POWER_INT (B, I)  &
      RESULT (B_RESULT)

   TYPE (BIG_INTEGER), INTENT (IN) :: B
   INTEGER, INTENT (IN) :: I
   TYPE (BIG_INTEGER) :: B_RESULT
   TYPE (BIG_INTEGER) :: TEMP_BIG

   IF (I <= 0) THEN
      B_RESULT = 1
   ELSE
      TEMP_BIG = BIG_POWER_INT (B, I / 2)
      IF (MODULO (I, 2) == 0) THEN
         B_RESULT = TEMP_BIG * TEMP_BIG
      ELSE
         B_RESULT = TEMP_BIG * TEMP_BIG * B
      END IF
   END IF

END FUNCTION BIG_POWER_INT

FUNCTION BIG_EQ_INT (B, I)  RESULT (L_RESULT)

   TYPE (BIG_INTEGER), INTENT (IN) :: B
   INTEGER, INTENT (IN) :: I
   LOGICAL :: L_RESULT

   L_RESULT = B == BIG_INT (I)

END FUNCTION BIG_EQ_INT

FUNCTION INT_EQ_BIG (I, B)  RESULT (L_RESULT)

   INTEGER, INTENT (IN) :: I
   TYPE (BIG_INTEGER), INTENT (IN) :: B
   LOGICAL :: L_RESULT

   L_RESULT = B == BIG_INT (I)

END FUNCTION INT_EQ_BIG

FUNCTION BIG_EQ_BIG (X, Y)  RESULT (L_RESULT)

   TYPE (BIG_INTEGER), INTENT (IN) :: X, Y
   LOGICAL :: L_RESULT

   L_RESULT = ALL (X % DIGIT (0 : NR_OF_DIGITS - 1) ==   &
                   Y % DIGIT (0 : NR_OF_DIGITS - 1))

END FUNCTION BIG_EQ_BIG

FUNCTION BIG_NE_INT (B, I)  RESULT (L_RESULT)

   TYPE (BIG_INTEGER), INTENT (IN) :: B
   INTEGER, INTENT (IN) :: I
   LOGICAL :: L_RESULT

   L_RESULT = B /= BIG_INT (I)

END FUNCTION BIG_NE_INT

FUNCTION INT_NE_BIG (I, B)  RESULT (L_RESULT)

   INTEGER, INTENT (IN) :: I
   TYPE (BIG_INTEGER), INTENT (IN) :: B
   LOGICAL :: L_RESULT

   L_RESULT = B /= BIG_INT (I)

END FUNCTION INT_NE_BIG

FUNCTION BIG_NE_BIG (X, Y)  RESULT (L_RESULT)

   TYPE (BIG_INTEGER), INTENT (IN) :: X, Y
   LOGICAL :: L_RESULT

   L_RESULT = ANY (X % DIGIT (0 : NR_OF_DIGITS - 1) /=   &
                   Y % DIGIT (0 : NR_OF_DIGITS - 1))

END FUNCTION BIG_NE_BIG

FUNCTION BIG_LE_INT (B, I)  RESULT (L_RESULT)

   TYPE (BIG_INTEGER), INTENT (IN) :: B
   LOGICAL :: L_RESULT
   INTEGER, INTENT (IN) :: I

   L_RESULT = B <= BIG_INT (I)
 
END FUNCTION BIG_LE_INT

FUNCTION INT_LE_BIG (I, B)  RESULT (L_RESULT)

   TYPE (BIG_INTEGER), INTENT (IN) :: B
   LOGICAL :: L_RESULT
   INTEGER, INTENT (IN) :: I

   L_RESULT = B <= BIG_INT (I)
 
END FUNCTION INT_LE_BIG

FUNCTION BIG_LE_BIG (X, Y)  RESULT (L_RESULT)

   TYPE (BIG_INTEGER), INTENT (IN) :: X, Y
   LOGICAL :: L_RESULT
   INTEGER :: N

   L_RESULT = .TRUE.
   DO N = NR_OF_DIGITS, 0, -1
      IF (X % DIGIT (N) /= Y % DIGIT (N)) THEN
         L_RESULT = (X % DIGIT (N) < Y % DIGIT (N))
         EXIT
      END IF
   END DO

END FUNCTION BIG_LE_BIG

FUNCTION BIG_LT_INT (B, I)  RESULT (L_RESULT)

   TYPE (BIG_INTEGER), INTENT (IN) :: B
   LOGICAL :: L_RESULT
   INTEGER, INTENT (IN) :: I

   L_RESULT = B < BIG_INT (I)
 
END FUNCTION BIG_LT_INT

FUNCTION INT_LT_BIG (I, B)  RESULT (L_RESULT)

   TYPE (BIG_INTEGER), INTENT (IN) :: B
   LOGICAL :: L_RESULT
   INTEGER, INTENT (IN) :: I

   L_RESULT = BIG_INT (I) < B
 
END FUNCTION INT_LT_BIG

FUNCTION BIG_LT_BIG (X, Y)  RESULT (L_RESULT)

   TYPE (BIG_INTEGER), INTENT (IN) :: X, Y
   LOGICAL :: L_RESULT
   INTEGER :: N

   L_RESULT = .FALSE.
   DO N = NR_OF_DIGITS, 0, -1
      IF (X % DIGIT (N) /= Y % DIGIT (N)) THEN
         L_RESULT = (X % DIGIT (N) < Y % DIGIT (N))
         EXIT
      END IF
   END DO

END FUNCTION BIG_LT_BIG

FUNCTION BIG_GE_INT (B, I)  RESULT (L_RESULT)

   TYPE (BIG_INTEGER), INTENT (IN) :: B
   LOGICAL :: L_RESULT
   INTEGER, INTENT (IN) :: I

   L_RESULT = B >= BIG_INT (I)
 
END FUNCTION BIG_GE_INT

FUNCTION INT_GE_BIG (I, B)  RESULT (L_RESULT)

   TYPE (BIG_INTEGER), INTENT (IN) :: B
   LOGICAL :: L_RESULT
   INTEGER, INTENT (IN) :: I

   L_RESULT = B >= BIG_INT (I)
 
END FUNCTION INT_GE_BIG

FUNCTION BIG_GE_BIG (X, Y)  RESULT (L_RESULT)

   TYPE (BIG_INTEGER), INTENT (IN) :: X, Y
   LOGICAL :: L_RESULT
   INTEGER :: N

   L_RESULT = .TRUE.
   DO N = NR_OF_DIGITS, 0, -1
      IF (X % DIGIT (N) /= Y % DIGIT (N)) THEN
         L_RESULT = (X % DIGIT (N) > Y % DIGIT (N))
         EXIT
      END IF
   END DO

END FUNCTION BIG_GE_BIG

FUNCTION BIG_GT_INT (B, I)  RESULT (L_RESULT)

   TYPE (BIG_INTEGER), INTENT (IN) :: B
   LOGICAL :: L_RESULT
   INTEGER, INTENT (IN) :: I

   L_RESULT = B > BIG_INT (I)
 
END FUNCTION BIG_GT_INT

FUNCTION INT_GT_BIG (I, B)  RESULT (L_RESULT)

   TYPE (BIG_INTEGER), INTENT (IN) :: B
   LOGICAL :: L_RESULT
   INTEGER, INTENT (IN) :: I

   L_RESULT = BIG_INT (I) > B
 
END FUNCTION INT_GT_BIG

FUNCTION BIG_GT_BIG (X, Y)  RESULT (L_RESULT)

   TYPE (BIG_INTEGER), INTENT (IN) :: X, Y
   LOGICAL :: L_RESULT
   INTEGER :: N

   L_RESULT = .FALSE.
   DO N = NR_OF_DIGITS, 0, -1
      IF (X % DIGIT (N) /= Y % DIGIT (N)) THEN
         L_RESULT = (X % DIGIT (N) > Y % DIGIT (N))
         EXIT
      END IF
   END DO

END FUNCTION BIG_GT_BIG

FUNCTION HUGE_BIG (B)  RESULT (B_RESULT)

   TYPE (BIG_INTEGER), INTENT (IN) :: B
   TYPE (BIG_INTEGER) :: B_RESULT

   B_RESULT % DIGIT = BASE - 1
   B_RESULT % DIGIT (NR_OF_DIGITS) = 0

END FUNCTION HUGE_BIG

FUNCTION MODULO_BIG_INT (B, I)  RESULT (B_RESULT)

   TYPE (BIG_INTEGER), INTENT (IN) :: B
   INTEGER, INTENT (IN) :: I
   INTEGER :: B_RESULT
   INTEGER :: N, REMAINDER

   REMAINDER = 0
   DO N = NR_OF_DIGITS, 0, -1
      REMAINDER = MODULO (BASE * REMAINDER + B % DIGIT (N), I)
   END DO

   B_RESULT = REMAINDER

END FUNCTION MODULO_BIG_INT

FUNCTION MODULO_BIG_BIG (A, P)  RESULT (B_RESULT)

   TYPE (BIG_INTEGER), INTENT (IN) :: A, P
   TYPE (BIG_INTEGER) :: B_RESULT

!  B_RESULT = A - A / P * P
   TYPE (BIG_INTEGER) :: Q
   INTEGER :: HIGH, LOW, MID, B, D, M, N, K

   DO N = NR_OF_DIGITS -1, 0, -1
      IF (P % DIGIT (N) /= 0) EXIT
   END DO
   IF (N == -1) THEN  ! Denominator = 0
      B_RESULT = HUGE (B_RESULT)
      RETURN
   ELSE IF (N <= 1) THEN
      B_RESULT = MODULO (A, (BASE * P % DIGIT (1) + P % DIGIT (0)))
      RETURN
   END IF

   DO M = NR_OF_DIGITS -1, 0, -1
      IF (A % DIGIT (M) /= 0) EXIT
   END DO
   IF (M < N) THEN
      B_RESULT = P
      RETURN
   END IF

   D = P % DIGIT (N)
   Q = A
   B_RESULT % DIGIT (M - N + 1 :) = 0

   DO K = M - N, 0, -1
      B = BASE * Q % DIGIT (M + 1) + Q % DIGIT (M)
      HIGH = (B + 1) / D + 1
      LOW = B / (D + 1)
      DO
         IF (LOW >= HIGH - 1) EXIT 
         MID = (HIGH + LOW) / 2
         IF (MID * P <= A) THEN
            LOW = MID
         ELSE
            HIGH = MID
         END IF
      END DO
      Q = Q - LOW * P * BIG_BASE_TO_POWER (K)
      M = M - 1
   END DO

   B_RESULT = Q

END FUNCTION MODULO_BIG_BIG

FUNCTION DIGITS_BIG (B)  RESULT (DIGITS_RESULT)

   TYPE (BIG_INTEGER), INTENT (IN) :: B
   INTEGER :: DIGITS_RESULT

   DIGITS_RESULT = NR_OF_DIGITS

END FUNCTION DIGITS_BIG

FUNCTION RADIX_BIG (B)  RESULT (RADIX_RESULT)

   TYPE (BIG_INTEGER), INTENT (IN) :: B
   INTEGER :: RADIX_RESULT

   RADIX_RESULT = BASE

END FUNCTION RADIX_BIG

FUNCTION RANGE_BIG (B)  RESULT (RANGE_RESULT)

   TYPE (BIG_INTEGER), INTENT (IN) :: B
   INTEGER :: RANGE_RESULT

   RANGE_RESULT = D * NR_OF_DIGITS - 1

END FUNCTION RANGE_BIG

FUNCTION SQRT_BIG (X)  RESULT (B_RESULT)

   TYPE (BIG_INTEGER), INTENT (IN) :: X
   TYPE (BIG_INTEGER) :: B_RESULT
   TYPE (BIG_INTEGER) :: OLD_SQRT_BIG, NEW_SQRT_BIG
   INTEGER :: N, I

   N = -1
   DO I = NR_OF_DIGITS, 0, -1
      IF (X % DIGIT (I) /= 0) THEN
         N = I
         EXIT
      END IF
   END DO

   IF (N == -1) THEN
      B_RESULT = 0
   ELSE IF (N == 0) THEN
      B_RESULT = INT (SQRT (REAL (X % DIGIT (0))))
   ELSE
      OLD_SQRT_BIG = 0
      IF (MODULO (N, 2) == 0) THEN
         OLD_SQRT_BIG % DIGIT (N / 2) = INT (SQRT (REAL (X % DIGIT (N))))
      ELSE
         OLD_SQRT_BIG % DIGIT ((N - 1) / 2) =  &
               INT (SQRT (REAL (BASE * X % DIGIT (N) + X % DIGIT (N-1))))
      END IF

      DO
         NEW_SQRT_BIG = (OLD_SQRT_BIG + X / OLD_SQRT_BIG) / 2
         IF (NEW_SQRT_BIG == OLD_SQRT_BIG .OR.  &
             NEW_SQRT_BIG == OLD_SQRT_BIG + 1 .OR.  &
             NEW_SQRT_BIG == 0) THEN
            EXIT
         ELSE
            OLD_SQRT_BIG = NEW_SQRT_BIG
         END IF
      END DO
      B_RESULT = OLD_SQRT_BIG
   END IF

END FUNCTION SQRT_BIG

FUNCTION BIG_BASE_TO_POWER (N)  RESULT (B_RESULT)

   INTEGER, INTENT (IN) :: N
   TYPE (BIG_INTEGER) :: B_RESULT

   IF (N < 0) THEN
      B_RESULT = 0
   ELSE IF (N >= NR_OF_DIGITS) THEN
      B_RESULT = HUGE (B_RESULT)
   ELSE
      B_RESULT % DIGIT = 0
      B_RESULT % DIGIT (N) = 1
   END IF

END FUNCTION BIG_BASE_TO_POWER

FUNCTION FACTORIAL_BIG (X)  RESULT (B_RESULT)

   INTEGER, INTENT (IN) :: X
   TYPE (BIG_INTEGER) :: B_RESULT
   TYPE (BIG_INTEGER) :: H
   INTEGER :: M

   H = HUGE (H)
   B_RESULT = 1
   M = 1

   DO
      IF (M > X) EXIT
      IF (B_RESULT == H) RETURN
      B_RESULT = B_RESULT * M
      M = M + 1
   END DO

END FUNCTION FACTORIAL_BIG

FUNCTION PRIME_BIG (X)  RESULT (L_RESULT)

   TYPE (BIG_INTEGER), INTENT (IN) :: X
   LOGICAL :: L_RESULT
   TYPE (BIG_INTEGER) :: S, DIV

   IF (X <= 1) THEN
      L_RESULT = .FALSE.
   ELSE IF (X == 2) THEN
      L_RESULT = .TRUE.
   ELSE IF (MODULO (X, 2) == 0) THEN
         L_RESULT = .FALSE.
   ELSE
      L_RESULT = .TRUE.
      S = SQRT (X)
      DIV = 3
      DO WHILE (DIV <= S)
         IF (MODULO (X, DIV) == 0) THEN
            L_RESULT = .FALSE.
            EXIT
         END IF
         DIV = DIV + 2
      END DO
   END IF

END FUNCTION PRIME_BIG

SUBROUTINE PRINT_BIG (X)

   TYPE (BIG_INTEGER), INTENT (IN) :: X
   INTEGER :: N, P
   CHARACTER (LEN = 100) :: CHAR_D, CHAR_X, BIG_FORMAT

   DO N = NR_OF_DIGITS, 0, -1
      IF (X % DIGIT (N) /= 0) EXIT
   END DO

   IF (N == -1) THEN
      WRITE (*, "(A)", ADVANCE = "NO") "0"
   ELSE
      WRITE (CHAR_X, *) X % DIGIT (N)
      P = VERIFY (CHAR_X, " ")
      CHAR_D = CHAR_X (P :)
      WRITE (*, "(A)", ADVANCE = "NO") TRIM (CHAR_D)
      WRITE (CHAR_D, *) D
      P = VERIFY (CHAR_D, " ")
      CHAR_D = CHAR_D (P :)
      WRITE (BIG_FORMAT, *) NR_OF_DIGITS
      P = VERIFY (BIG_FORMAT, " ")
      BIG_FORMAT = BIG_FORMAT (P :)
      BIG_FORMAT = "(" // TRIM (BIG_FORMAT) // "I" //  &
                    TRIM (CHAR_D) // "." // TRIM (CHAR_D) // ")"
      WRITE (*, BIG_FORMAT, ADVANCE = "NO") X % DIGIT (N-1 : 0 : -1)
   END IF

END SUBROUTINE PRINT_BIG

SUBROUTINE PRINT_FACTORS_BIG (X)

   TYPE (BIG_INTEGER), INTENT (IN) :: X
   TYPE (BIG_INTEGER) :: TEMP_X, F, SQRT_X

   TEMP_X = X
   F = 2
   SQRT_X = SQRT (X)
   DO
     IF (MODULO (TEMP_X, F) == 0) THEN
        WRITE (*, "(A)", ADVANCE="NO") "2 "
        TEMP_X = TEMP_X / 2
     ELSE
        EXIT
     END IF
   END DO
   F = 3
   DO
      IF (TEMP_X == 1) EXIT
      IF (SQRT_X < F) THEN
         CALL PRINT_BIG (TEMP_X)
         WRITE (*, "(A)", ADVANCE="NO") " "
         EXIT
      END IF
      DO
         IF (MODULO (TEMP_X, F) == 0) THEN
            CALL PRINT_BIG (F)
            WRITE (*, "(A)", ADVANCE="NO") " "
            TEMP_X = TEMP_X / F
            SQRT_X = SQRT (TEMP_X)
         ELSE
            EXIT
         END IF
      END DO
      F = F + 2
   END DO
   PRINT *

END SUBROUTINE PRINT_FACTORS_BIG

END MODULE BIG_INTEGERS
