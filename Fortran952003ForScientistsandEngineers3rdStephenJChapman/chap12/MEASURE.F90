PROGRAM measurements
!
! Purpose:
!   To generate measurements for the radar tracker simulation.
!
REAL, PARAMETER :: rad_2_deg = 180./3.141592
REAL :: x0  = -5000         ! m
REAL :: y0  = +20000        ! m
REAL :: vx0 = +141.4        ! m/s
REAL :: vy0 = -141.4        ! m/s
REAL :: vx20 = 0.           ! m/s
REAL :: vy20 = -200.        ! m/s
REAL :: dt = 5              ! s
REAL :: alpha = 0.7
REAL :: beta = 0.38
REAL :: dummy
REAL,DIMENSION(0:40) :: x, y, vx, vy, r, theta, t
REAL,DIMENSION(0:40) :: xm, ym, xs, ys, vxs, vys, xp, yp

!  Calculate positions for first part of straight flight.
DO i = 0, 20
   x(i)  = x0 + vx0 * (i * dt)
   y(i)  = y0 + vy0 * (i * dt)
   vx(i) = vx0
   vy(i) = vy0
   t(i)  = i * dt
   r(i)  = SQRT(x(i)**2+y(i)**2)
END DO

! Now turn corner...
DO i = 21, 40
   x(i)  = x(20) + vx20 * ((i-20) * dt)
   y(i)  = y(20) + vy20 * ((i-20) * dt)
   vx(i) = vx20
   vy(i) = vy20
   t(i)  = i * dt
END DO

!  Calculate range and angles.
DO i = 0, 40
   r(i) = SQRT(x(i)**2+y(i)**2)
   theta(i) = 90 - rad_2_deg*atan2(y(i),x(i))
   IF ( theta(i) < 0. ) theta(i) = theta(i) + 360.
   WRITE (*,100) t(i), x(i), y(i), r(i), theta(i)
END DO

! Open file and save uncorrupted values.
OPEN (8,FILE='track1.dat',STATUS='REPLACE')
DO i = 0, 40
   !WRITE (8,100) t(i), x(i), y(i), r(i), theta(i)
   WRITE (8,120) t(i), r(i), theta(i)
   120 FORMAT (1X,F6.2,3X,F8.0,3X,F9.1)
END DO
CLOSE (8)

! Corrupt measurements by noise.  Corrupt range by Gaussian
! noise with a standard deviation of 200 m, and corrupt angle
! by Gaussian noise with a standard deviation of 1.1 degrees.
!
DO i = 1, 10
   dummy = random_g()
END DO

DO i = 0, 40
    r(i) = r(i) + 200. * random_g()
    theta(i) = theta(i) + 1.1 * random_g()
    IF ( theta(i) < 0. ) theta(i) = theta(i) + 360.
    IF ( theta(i) > 360. ) theta(i) = theta(i) - 360.
END DO

!  Display range and angles.
WRITE (*,*) ' '
DO i = 0, 40
   WRITE (*,100) t(i), x(i), y(i), r(i), theta(i)
   100 FORMAT (1X,F6.2,2(3X,F10.2),3X,F8.0,3X,F9.1)
END DO   

! Open file and save corrupted values.
OPEN (8,FILE='track2.dat',STATUS='REPLACE')
DO i = 0, 40
   !WRITE (8,100) t(i), x(i), y(i), r(i), theta(i)
   WRITE (8,120) t(i), r(i), theta(i)
END DO
CLOSE (8)

! Open file and save velocities.
OPEN (8,FILE='vel.dat',STATUS='REPLACE')
DO i = 0, 39
   WRITE (8,100) (t(i)+t(i+1))/2., vx(i), vy(i)
END DO
CLOSE (8)

! Now execute filter.
xm(0) = r(0) * sin(theta(0) / rad_2_deg)
ym(0) = r(0) * cos(theta(0) / rad_2_deg)
xs(0) = xm(0)
ys(0) = ym(0)
vxs(0) = 0.
vys(0) = 0.

DO i = 1, 40
   xm(i) = r(i) * sin(theta(i) / rad_2_deg)
   ym(i) = r(i) * cos(theta(i) / rad_2_deg)

   xp(i) = xs(i-1) + vxs(i-1) * 5
   yp(i) = ys(i-1) + vys(i-1) * 5

   xs(i) = xp(i) + alpha * ( xm(i) - xp(i) )
   ys(i) = yp(i) + alpha * ( ym(i) - yp(i) )

   vxs(i) = vxs(i-1) + (beta/5) * ( xm(i) - xp(i) )
   vys(i) = vys(i-1) + (beta/5) * ( ym(i) - yp(i) )

END DO

! Open file and save filtered positions.
OPEN (8,FILE='filtp.dat',STATUS='REPLACE')
DO i = 0, 40
   WRITE (8,110) t(i), xm(i), xp(i), xs(i), ym(i), yp(i), ys(i)
   110 FORMAT (1X,F6.2,6(2X,F9.1)) 
END DO
CLOSE (8)


! Open file and save filtered positions.
OPEN (8,FILE='filtv.dat',STATUS='REPLACE')
DO i = 0, 40
   WRITE (8,110) t(i), vx(i), vxs(i), vy(i), vys(i)
END DO
CLOSE (8)


DO i = 1, 40
   write (*,110) t(i), vx(i), vxs(i), vy(i), vys(i)   
END DO




END PROGRAM

REAL FUNCTION random_g( )
!
!  Purpose:
!    Function to generate a gaussian normal distribution with
!    a mean of 0.0 and a standard deviation of 1.0.  It is 
!    based on the Box-Muller method as documented in Chapter 7
!    of "Numerical Recipes".
!
!  Record of revisions:
!      Date       Programmer          Description of change
!      ====       ==========          =====================
!    03/12/96    S. J. Chapman        Original code
!
IMPLICIT NONE

! Declare local variables.
LOGICAL,SAVE :: available = .FALSE. ! Value available?
REAL :: r                 ! SQRT(ran(1)**2+ran(2)**2)
REAL, DIMENSION(2) :: ran ! Unif random vars [0,1)
REAL, SAVE :: saved       ! Saved value
REAL, DIMENSION(2) :: v   ! Unif random vars [-1,1)
REAL, DIMENSION(2) :: y   ! Gaussian random vars

! Check to see if a saved value is available.
IF ( .NOT. available ) THEN
 
   ! No variable is available, so we must create new ones.
   ! Get 2 uniform random variables in the range [0.,1.) such
   ! that the square root of the sum of their squares < 1.  
   ! Keep trying until we come up with such a combination.
   DO 
      CALL random_number( ran )
      v = 2. * ran - 1.
      r = SUM( v**2 )
      IF ( r < 1. ) EXIT
   END DO
   
   ! Calculate the two Gaussian random from the uniform 
   ! variables by Box-Muller method as: 
   !   y = SQRT( -2.*LOG(r) / r ) * v
   y = SQRT( -2. * LOG(r) / r ) * v
   
   ! Return one value and save the other for next time.
   random_g = y(1)
   saved    = y(2)
   available = .TRUE.

ELSE ! A saved value was available. 

   random_g  = saved
   available = .FALSE.

END IF

END FUNCTION random_g

