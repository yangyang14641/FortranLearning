PROGRAM DERNOI
C
C  Purpose:
C    To examine the effects of noise on the calculation of the numerical
C    derivative of a function.  This routine generates two input data 
C    sets, one a pure sinusoid and the other corrupted by a uniform random
C    noise whose peak amplitude is 2% if the peak amplitude of the 
C    sinusoid.  It takes the derivative of both data sets, and compares
C    the numerical derivative with the known correct answer for a sine
C    function (df/dx = cox(x)).
C
C  Record of revisions:
C      Date       Programmer          Description of change
C      ====       ==========          =====================
C    12/23/93    S. J. Chapman        Original code
C
C  List of parameters:
C     NSAMP  -- Number of samples in function
C     DX     -- Spacing of samples in function
C
C  List of local variables:
C     CDERIV -- Analytically calculated derivative df/dx = cos(x)
C     ERROR  -- Error flag from subroutine DERV
C     I      -- DO loop variable.
C     MAXER1 -- Maximum error in calculated derivative of pure sine
C     MAXER2 -- Maximum error in calculated derivative of sine + noise
C     NDERV1 -- Derivative of pure sine from subroutine DERV
C     NDERV2 -- Derivative of noisy sine from subroutine DERV
C     SINE1  -- f(x) = sin(x)
C     SINE2  -- f(x) = sin(x) + 2% uniform random noise
C     VALUE  -- Random noise value
C
      IMPLICIT NONE
C
C     Parameters.
C
      INTEGER     NSAMP
      REAL        DX
      PARAMETER ( NSAMP = 100  )
      PARAMETER ( DX    = 0.05 )
C
C     Local variables.
C 
      REAL    SINE1(NSAMP), SINE2(NSAMP), CDERIV(NSAMP-1)
      REAL    NDERV1(NSAMP-1), NDERV2(NSAMP-1), MAXER1, MAXER2, VALUE
      INTEGER I, ERROR
C
C     Calculate f(x) = sin(x)
C
      DO 10 I = 1, NSAMP
         SINE1(I) = SIN(REAL(I-1)*DX)
   10 CONTINUE
C
C     Calculate f(x) = sin(x) + 2% noise.  Scale noise to a peak
C     amplitude of +/- 0.02.
C
      DO 20 I = 1, NSAMP
         CALL RANDOM0 ( VALUE ) 
         VALUE = 0.04 * VALUE - 0.02
         SINE2(I) = SINE1(I) + VALUE
   20 CONTINUE

      OPEN (8,FILE = 'x')
      DO I = 1, NSAMP 
         WRITE (8,'(1X,F12.6,4X,F12.6)') REAL(I-1)*DX, SINE2(I)
      END DO

      END
