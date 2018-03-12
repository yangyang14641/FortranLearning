PROGRAM power
!
!  Purpose: 
!    To calculate the current, real, reactive, and apparent power,
!    and the power factor supplied to a load. 
!
!  Record of revisions:
!      Date       Programmer          Description of change
!      ====       ==========          =====================
!    11/03/06    S. J. Chapman        Original code 
!
     
IMPLICIT NONE
 
! Data dictionary: declare constants  
REAL,PARAMETER :: DEG_2_RAD = 0.01745329 ! Deg to radians factor

! Data dictionary: declare variable types, definitions, & units  
REAL :: amps            ! Current in the load (A)
REAL :: p               ! Real power of load (W)
REAL :: pf              ! Power factor of load (no units)
REAL :: q               ! Reactive power of the load (VAR)
REAL :: s               ! Apparent power of the load (VA)
REAL :: theta           ! Impedance angle of the load (deg)
REAL :: volts           ! Rms voltage of the power source (V)
REAL :: z               ! Magnitude of the load impedance (ohms)

! Prompt the user for the rms voltage.
WRITE (*,*) 'Enter the rms voltage of the source: '
READ  (*,*) volts 

! Prompt the user for the magnitude and angle of the impedance.
WRITE (*,*) 'Enter the magnitude and angle of the impedance '
WRITE (*,*) 'in ohms and degrees: '
READ  (*,*) z, theta   

! Perform calculations
amps = volts / z                           ! Rms current
p = volts * amps * cos (theta * DEG_2_RAD) ! Real power
q = volts * amps * sin (theta * DEG_2_RAD) ! Reactive power
s = volts * amps                           ! Apparent power
pf = cos ( theta * DEG_2_RAD)              ! Power factor
 
! Write out the results.
WRITE (*,*) 'Voltage        = ', volts, ' volts'
WRITE (*,*) 'Impedance      = ', z, ' ohms at ', theta,' degrees'
WRITE (*,*) 'Current        = ', amps, ' amps'
WRITE (*,*) 'Real Power     = ', p, ' watts'
WRITE (*,*) 'Reactive Power = ', q, ' VAR'
WRITE (*,*) 'Apparent Power = ', s, ' VA'
WRITE (*,*) 'Power Factor   = ', pf
 
! Finish up.
END PROGRAM power
