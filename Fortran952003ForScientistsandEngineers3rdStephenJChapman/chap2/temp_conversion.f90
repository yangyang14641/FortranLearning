PROGRAM temp_conversion
 
!  Purpose: 
!    To convert an input temperature from degrees Fahrenheit to
!    an output temperature in kelvins. 
!
!  Record of revisions:
!      Date       Programmer          Description of change
!      ====       ==========          =====================
!    11/03/06 -- S. J. Chapman        Original code 
!

IMPLICIT NONE        ! Force explicit declaration of variables

! Data dictionary: declare variable types, definitions, & units  
REAL :: temp_f       ! Temperature in degrees Fahrenheit
REAL :: temp_k       ! Temperature in kelvins

! Prompt the user for the input temperature.
WRITE (*,*) 'Enter the temperature in degrees Fahrenheit: '
READ  (*,*) temp_f  
 
! Convert to kelvins.
temp_k = (5. / 9.) * (temp_f - 32.) + 273.15 
 
! Write out the result.
WRITE (*,*) temp_f, ' degrees Fahrenheit = ', temp_k, ' kelvins'
 
! Finish up.
END PROGRAM temp_conversion
