!-------------------------------------------------------------------
! Program Labels
!-------------------------------------------------------------------
! Program name: tryPreprocess.f90
! Try preprocess in c language format
! Compiler command: ifort -cpp tryPreprocess.f90 -o tryPreprocess
! or ifort -fpp tryPreprocess.f90 -o tryPreprocess
! -cpp / -fpp  means C Pre-processor / Fortran Pre Processor
!-------------------------------------------------------------------


!-------------------------------------------------------------------
! Define macros 
!-------------------------------------------------------------------
#define FLOAT REAL(KIND=8)     ! define 8-byte double-precision float
!#define FLOAT real(kind=4)    ! define 4-byte single-precision float
!-------------------------------------------------------------------


!-------------------------------------------------------------------
! Program main
!-------------------------------------------------------------------
PROGRAM main
IMPLICIT NONE

FLOAT i          ! define a float of single-precision or double-precision

i = 3.14159265358979323846D0

WRITE(*,200)  i
200 FORMAT(1x,'i = ',ES30.20)

WRITE(*,300)  
300 FORMAT(1x,'i = 3.14159265358979323846D0')

END PROGRAM main
!--------------------------------------------------------------------