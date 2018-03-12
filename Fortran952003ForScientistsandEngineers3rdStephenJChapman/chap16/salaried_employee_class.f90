MODULE salaried_employee_class
!
!   This module implements a salaried employee class.  
!
!  Record of revisions:
!      Date       Programmer          Description of change
!      ====       ==========          =====================
!    12/29/06    S. J. Chapman        Original code
!
USE employee_class                  ! USE parent class
IMPLICIT NONE

! Type definition
TYPE,PUBLIC,EXTENDS(employee) :: salaried_employee  

   ! Additional instance variables. 
   PRIVATE
   REAL :: salary = 0                 ! Monthly salary

CONTAINS

   ! Bound procedures
   PROCEDURE,PUBLIC :: set_salary => set_salary_sub
   PROCEDURE,PUBLIC :: calc_pay => calc_pay_fn
   
END TYPE salaried_employee

! Restrict access to the actual procedure names
PRIVATE :: calc_pay_fn, set_salary_sub

! Now add methods
CONTAINS

   SUBROUTINE set_salary_sub(this, salary)
   ! 
   ! Subroutine to initialize the salary of the salaried 
   ! employee.  This is a new method.
   !
   IMPLICIT NONE
   
   ! Declare calling arguments
   CLASS(salaried_employee) :: this      ! Salaried employee object
   REAL,INTENT(IN) :: salary             ! Salary

   ! Save data in this object.
   this%pay    = salary
   this%salary = salary
                   
   END SUBROUTINE set_salary_sub


   REAL FUNCTION calc_pay_fn(this,hours)
   ! 
   ! Function to calculate the salaried employee pay.  This
   ! function overrides the one in the parent class.
   !
   IMPLICIT NONE
   
   ! Declare calling arguments
   CLASS(salaried_employee) :: this      ! Salaried employee object
   REAL,INTENT(IN) :: hours              ! Hours worked

   ! Return pay
   calc_pay_fn = this%salary
                   
   END FUNCTION calc_pay_fn

END MODULE salaried_employee_class
