PROGRAM c14_date
!
!  Purpose:
!    To calculate the age of an organic sample from the percentage 
!    of the original carbon 14 remaining in the sample.
!
!  Record of revisions:
!      Date       Programmer          Description of change
!      ====       ==========          =====================
!    11/03/06    S. J. Chapman        Original code 
!
IMPLICIT NONE
 
! Data dictionary: declare constants  
REAL,PARAMETER :: LAMDA = 0.00012097 ! The radioactive decay 
                                     ! constant of carbon 14,
                                     ! in units of 1/years.

! Data dictionary: declare variable types, definitions, & units  
REAL :: age      ! The age of the sample (years)
REAL :: percent  ! The percentage of carbon 14 remaining at the time
                 ! of the measurement (%)
REAL :: ratio    ! The ratio of the carbon 14 remaining at the time
                 ! of the measurement to the original amount of 
                 ! carbon 14 (no units)
 
! Prompt the user for the percentage of C-14 remaining.
WRITE (*,*) 'Enter the percentage of carbon 14 remaining:'
READ  (*,*) percent
 
! Echo the user's input value.
WRITE (*,*) 'The remaining carbon 14 = ', percent, ' %.'

! Perform calculations
ratio = percent / 100.             ! Convert to fractional ratio
age = (-1.0 / LAMDA) * log(ratio)  ! Get age in years

! Tell the user about the age of the sample.
WRITE (*,*) 'The age of the sample is  ', age, ' years.'
 
! Finish up.
END PROGRAM c14_date

