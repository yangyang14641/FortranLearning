PROGRAM test_selected_kinds
!
!  Purpose: 
!    To determine selected real kinds.
! 
IMPLICIT NONE
INTEGER :: kind_number     ! Resulting kind number

! Display kind numbers
kind_number = SELECTED_REAL_KIND(p=6,r=37)
WRITE (*,*) 'SELECTED_REAL_KIND(p=6,r=37) = ', kind_number
kind_number = SELECTED_REAL_KIND(p=12) 
WRITE (*,*) 'SELECTED_REAL_KIND(p=12) = ',   kind_number
kind_number = SELECTED_REAL_KIND(r=100) 
WRITE (*,*) 'SELECTED_REAL_KIND(r=100) = ',  kind_number
kind_number = SELECTED_REAL_KIND(13,200) 
WRITE (*,*) 'SELECTED_REAL_KIND(13,200) = ', kind_number
kind_number = SELECTED_REAL_KIND(13) 
WRITE (*,*) 'SELECTED_REAL_KIND(13) = ',     kind_number
kind_number = SELECTED_REAL_KIND(p=17) 
WRITE (*,*) 'SELECTED_REAL_KIND(p=17) = ',   kind_number
 
END PROGRAM test_selected_kinds
