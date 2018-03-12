PROGRAM test_selected_int_kinds
!
!  Purpose: 
!    To determine selected integer kinds.
! 
IMPLICIT NONE
INTEGER :: kind_number     ! Resulting kind number

! Display kind numbers
kind_number = SELECTED_INT_KIND(1)
WRITE (*,*) 'SELECTED_INT_KIND(1) = ', kind_number
kind_number = SELECTED_INT_KIND(2) 
WRITE (*,*) 'SELECTED_INT_KIND(2) = ',   kind_number
kind_number = SELECTED_INT_KIND(3) 
WRITE (*,*) 'SELECTED_INT_KIND(3) = ',  kind_number
kind_number = SELECTED_INT_KIND(4) 
WRITE (*,*) 'SELECTED_INT_KIND(4) = ', kind_number
kind_number = SELECTED_INT_KIND(5) 
WRITE (*,*) 'SELECTED_INT_KIND(5) = ',     kind_number
kind_number = SELECTED_INT_KIND(6) 
WRITE (*,*) 'SELECTED_INT_KIND(6) = ',   kind_number
kind_number = SELECTED_INT_KIND(7) 
WRITE (*,*) 'SELECTED_INT_KIND(7) = ',   kind_number
kind_number = SELECTED_INT_KIND(8) 
WRITE (*,*) 'SELECTED_INT_KIND(8) = ',   kind_number
kind_number = SELECTED_INT_KIND(9) 
WRITE (*,*) 'SELECTED_INT_KIND(9) = ',   kind_number
kind_number = SELECTED_INT_KIND(10) 
WRITE (*,*) 'SELECTED_INT_KIND(10) = ',   kind_number
kind_number = SELECTED_INT_KIND(11) 
WRITE (*,*) 'SELECTED_INT_KIND(11) = ',   kind_number
kind_number = SELECTED_INT_KIND(12) 
WRITE (*,*) 'SELECTED_INT_KIND(12) = ',   kind_number
kind_number = SELECTED_INT_KIND(13) 
WRITE (*,*) 'SELECTED_INT_KIND(13) = ',   kind_number
kind_number = SELECTED_INT_KIND(14) 
WRITE (*,*) 'SELECTED_INT_KIND(14) = ',   kind_number
kind_number = SELECTED_INT_KIND(15) 
WRITE (*,*) 'SELECTED_INT_KIND(15) = ',   kind_number
kind_number = SELECTED_INT_KIND(16) 
WRITE (*,*) 'SELECTED_INT_KIND(16) = ',   kind_number
kind_number = SELECTED_INT_KIND(17) 
WRITE (*,*) 'SELECTED_INT_KIND(17) = ',   kind_number
kind_number = SELECTED_INT_KIND(18) 
WRITE (*,*) 'SELECTED_INT_KIND(18) = ',   kind_number
kind_number = SELECTED_INT_KIND(19) 
WRITE (*,*) 'SELECTED_INT_KIND(19) = ',   kind_number
kind_number = SELECTED_INT_KIND(20) 
WRITE (*,*) 'SELECTED_INT_KIND(20) = ',   kind_number
 
END PROGRAM test_selected_int_kinds
