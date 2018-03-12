MODULE hourly_employee_class
!
!   This module implements an hourly employee class.  
!
!  Record of revisions:
!      Date       Programmer          Description of change
!      ====       ==========          =====================
!    12/29/06    S. J. Chapman        Original code
!
USE employee_class                  ! USE parent class
IMPLICIT NONE

! Type definition
TYPE,PUBLIC,EXTENDS(employee) :: hourly_employee  

   ! Additional instance variables. 
   PRIVATE
   REAL :: rate = 0                ! Hourly rate

CONTAINS

   ! Bound procedures
   PROCEDURE,PUBLIC :: set_pay_rate => set_pay_rate_sub
   PROCEDURE,PUBLIC :: calc_pay => calc_pay_fn
   
END TYPE hourly_employee

! Restrict access to the actual procedure names
PRIVATE :: calc_pay_fn, set_pay_rate_sub

! Now add methods
CONTAINS

   SUBROUTINE set_pay_rate_sub(this, rate)
   ! 
   ! Subroutine to initialize the pay rate of the hourly 
   ! employee.  This is a new method.
   !
   IMPLICIT NONE
   
   ! Declare calling arguments
   CLASS(hourly_employee) :: this        ! Hourly employee object
   REAL,INTENT(IN) :: rate               ! Pay rate ($/hr)

   ! Save data in this object.
   this%rate = rate
                   
   END SUBROUTINE set_pay_rate_sub


   REAL FUNCTION calc_pay_fn(this,hours)
   ! 
   ! Function to calculate the hourly employee pay.  This
   ! function overrides the one in the parent class.
   !
   IMPLICIT NONE
   
   ! Declare calling arguments
   CLASS(hourly_employee) :: this        ! Hourly employee object
   REAL,INTENT(IN) :: hours              ! Hours worked

   ! Return pay
   this%pay = hours * this%rate
   calc_pay_fn = this%pay
                   
   END FUNCTION calc_pay_fn

END MODULE hourly_employee_class
