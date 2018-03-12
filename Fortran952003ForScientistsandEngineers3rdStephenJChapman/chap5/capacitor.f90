PROGRAM capacitor
!
!  Purpose:
!    To calculate the behavior of a capacitor as follows:
!    1.  If capacitance and voltage are known, calculate 
!        charge, number of electrons, and energy stored.
!    2.  If charge and voltage are known, calculate capa-
!        citance, number of electrons, and energy stored. 
!
!  Record of revisions:
!       Date       Programmer          Description of change
!       ====       ==========          =====================
!     11/18/06    S. J. Chapman        Original code
!
IMPLICIT NONE

! Data dictionary: declare constants
REAL, PARAMETER :: ELECTRONS_PER_COULOMB = 6.241461E18 

! Data dictionary: declare variable types, definitions, & units
REAL :: c         ! Capacitance of the capacitor (farads).
REAL :: charge    ! Charge on the capacitor (coulombs).
REAL :: electrons ! Number of electrons on the plates of the capacitor
REAL :: energy    ! Energy stored in the electric field (joules)
INTEGER :: type   ! Type of input data available for the calculation:
                  !  1:  C and V
                  !  2:  CHARGE and V
REAL :: v         ! Voltage on the capacitor (volts).

! Prompt user for the type of input data available.
WRITE (*, 100)
100 FORMAT (' This program calculates information about a ' &
            'capacitor.',/, ' Please specify the type of information',&
            ' available from the following list:',/,&
            '   1 -- capacitance and voltage ',/,&
            '   2 -- charge and voltage ',//,&
            ' Select options 1 or 2: ')

! Get response and validate it.
DO
   READ (*,*) type
   IF ( (type == 1) .OR. (type == 2) ) EXIT
   WRITE (*,110) type
   110 FORMAT (' Invalid response: ', I6, '.  Please enter 1 or 2:')
END DO

! Get additional data based upon the type of calculation.
input: IF ( type == 1 ) THEN
    
   ! Get capacitance.   
   WRITE (*,' Enter capacitance in farads: ')    
   READ (*,*) c
   
   ! Get voltage.
   WRITE (*,' Enter voltage in volts: ')                   
   READ (*,*) v
 
ELSE 
 
   ! Get charge
   WRITE (*,' Enter charge in coulombs: ')                 
   READ (*,*) charge

   ! Get voltage.
   WRITE (*,' Enter voltage in volts: ')                   
   READ (*,*) v
 
END IF input

! Calculate the unknown quantities. 
calculate: IF ( type == 1 ) THEN
   charge = c * v                            ! Charge
ELSE 
   c = charge / v                            ! Capacitance
END IF calculate
electrons = charge * ELECTRONS_PER_COULOMB   ! Electrons
energy = 0.5 * c * v**2                      ! Energy
 
! Write out answers.
WRITE (*,120) v, c, charge, electrons, energy 
120 FORMAT (' For this capacitor: ',/, &
            '   Voltage             = ', F10.2, ' V',/, &
            '   Capacitance         = ', ES10.3, ' F',/, &
            '   Total charge        = ', ES10.3, ' C',/, &
            '   Number of electrons = ', ES10.3,/, &
            '   Total energy        = ', F10.4, ' joules' )
 
END PROGRAM capacitor
