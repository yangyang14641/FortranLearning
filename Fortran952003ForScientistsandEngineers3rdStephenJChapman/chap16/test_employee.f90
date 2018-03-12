PROGRAM test_employee
!
!  This program tests the employee class and its subclasses.
!
!  Record of revisions:
!      Date       Programmer          Description of change
!      ====       ==========          =====================
!    12/29/06    S. J. Chapman        Original code
!
USE hourly_employee_class        ! Import hourly employee class
USE salaried_employee_class      ! Import salaried employee class
IMPLICIT NONE

! Declare variables
CLASS(employee),POINTER :: emp1, emp2         ! Employees
TYPE(salaried_employee),POINTER :: sal_emp    ! Salaried employee
TYPE(hourly_employee),POINTER :: hourly_emp   ! Hourly employee
INTEGER :: istat                              ! Allocate status

! Create an object of type "salaried_employee" 
ALLOCATE( sal_emp, STAT=istat )

! Initialize the data in this object
CALL sal_emp%set_employee('John','Jones','111-11-1111');
CALL sal_emp%set_salary(3000.00);

! Create an object of type "hourly_employee" 
ALLOCATE( hourly_emp, STAT=istat )

! Initialize the data in this object
CALL hourly_emp%set_employee('Jane','Jones','222-22-2222');
CALL hourly_emp%set_pay_rate(12.50);

! Now create pointers to "employees".
emp1 => sal_emp
emp2 => hourly_emp

! Calculate pay using subclass pointers
WRITE (*,'(A)') 'Pay using subclass pointers:'
WRITE (*,'(A,F6.1)') 'Emp 1 Pay = ', sal_emp%calc_pay(160.)
WRITE (*,'(A,F6.1)') 'Emp 2 Pay = ', hourly_emp%calc_pay(160.)

! Calculate pay using superclass pointers
WRITE (*,'(A)') 'Pay using superclass pointers:'
WRITE (*,'(A,F6.1)') 'Emp 1 Pay = ', emp1%calc_pay(160.)
WRITE (*,'(A,F6.1)') 'Emp 2 Pay = ', emp2%calc_pay(160.)

! List employee information using superclass pointers
WRITE (*,*) 'Employee information:'
WRITE (*,*) 'Emp 1 Name / SSN = ', TRIM(emp1%get_first_name()) // &
            ' ' // TRIM(emp1%get_last_name()) // ' ', &
            TRIM(emp1%get_ssn())
WRITE (*,*) 'Emp 2 Name / SSN = ', TRIM(emp2%get_first_name()) // &
            ' ' // TRIM(emp2%get_last_name()) // ' ', &
            TRIM(emp2%get_ssn())

END PROGRAM test_employee
