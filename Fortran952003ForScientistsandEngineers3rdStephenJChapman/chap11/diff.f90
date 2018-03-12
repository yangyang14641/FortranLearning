PROGRAM diff
!
!  Purpose: 
!    To test the effects of finite precision by differentiating
!    a function with 10 different step sizes, with both single
!    precision and double precision.  The test will be based on 
!    the function F(X) = 1./X.
! 
!  Record of revisions:
!       Date       Programmer          Description of change
!       ====       ==========          =====================
!     11/27/95    S. J. Chapman        Original code
!
IMPLICIT NONE

! Data dictionary: declare constants
INTEGER, PARAMETER :: SGL = SELECTED_REAL_KIND(p=6,r=37)
INTEGER, PARAMETER :: DBL = SELECTED_REAL_KIND(p=13)

! List of local variables:
REAL(KIND=DBL) :: ans             ! True (analytic) answer
REAL(KIND=DBL) :: d_ans           ! Double precision answer
REAL(KIND=DBL) :: d_error         ! Double precision percent error
REAL(KIND=DBL) :: d_fx            ! Double precision F(x)
REAL(KIND=DBL) :: d_fxdx          ! Double precision F(x+dx)
REAL(KIND=DBL) :: d_dx            ! Step size
REAL(KIND=DBL) :: d_x = 0.15_DBL  ! Location to evaluate dF(x)/dx
INTEGER :: i                      ! Index variable
REAL(KIND=SGL) :: s_ans           ! Single precision answer
REAL(KIND=SGL) :: s_error         ! Single precision percent error
REAL(KIND=SGL) :: s_fx            ! Single precision F(x)
REAL(KIND=SGL) :: s_fxdx          ! Single precision F(x+dx)
REAL(KIND=SGL) :: s_dx            ! Step size
REAL(KIND=SGL) :: s_x = 0.15_SGL  ! Location to evaluate dF(x)/dx

! Print headings.
WRITE (*,1)
1 FORMAT (1X,'    DX       TRUE ANS     SP ANS          DP ANS  ', &
             '         SP ERR   DP ERR  ')
 
! Calculate analytic solution at x=0.15.
ans = - ( 1.0_DBL / d_x**2 )
    
! Calculate answer from definition of differentiation
step_size: DO i = 1, 10 
 
   ! Get delta x.
   s_dx = 1.0 / 10.0**i
   d_dx = 1.0_DBL / 10.0_DBL**i
 
   ! Calculate single precision answer.
   s_fxdx = 1. / ( s_x + s_dx )   
   s_fx   = 1. / s_x
   s_ans  = ( s_fxdx - s_fx ) / s_dx
 
   ! Calculate single precision error, in percent.
   s_error = ( s_ans - REAL(ans) ) / REAL(ans) * 100.
 
   ! Calculate double precision answer.
   d_fxdx = 1.0_DBL / ( d_x + d_dx )   
   d_fx   = 1.0_DBL / d_x 
   d_ans  = ( d_fxdx - d_fx ) / d_dx

   ! Calculate double precision error, in percent.
   d_error = ( d_ans - ans ) / ans * 100.
 
   ! Tell user.
   WRITE (*,100) d_dx, ans, s_ans, d_ans, s_error, d_error
   100 FORMAT (1X, ES10.3, F12.7, F12.7, ES22.14, F9.3, F9.3)
 
END DO step_size
 
END PROGRAM diff
