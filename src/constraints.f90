!$Header: /usr/local/ollincvs/Codes/OllinToy/src/constraints.f90,v 1.3 2024/09/06 20:52:10 malcubi Exp $

  subroutine constraints

! *************************************
! ***   EVALUATION OF CONSTRAINTS   ***
! *************************************

! This routine evaluates the derivative constraints.

! Include modules.

  use param
  use arrays
  use arrayflags

! Extra variables.

  implicit none

  integer i

  real(8) idx
  real(8) zero,half,one


! *******************
! ***   NUMBERS   ***
! *******************

  zero  = 0.0D0
  half  = 0.5D0
  one   = 1.0D0

  idx = one/dx


! ***********************
! ***   CONSTRAINTS   ***
! ***********************

! Calpha.

  if (flag_Calpha) then

     do i=1,Nx-1
        Calpha(i) = Dalpha(i) - half*idx*(alpha(i+1) - alpha(i-1))/alpha(i)
     end do

     if (boundtype=="flat") then
        Calpha(0)  = Calpha(1)
        Calpha(Nx) = Calpha(Nx-1)
     else if (boundtype=="periodic") then
        Calpha(0)  = Calpha(Nx-1)
        Calpha(Nx) = Calpha(1)
     end if

  end if

! Cg.

  if (flag_Cg) then

     do i=1,Nx-1
        Cg(i) = Dg(i) - half*idx*(g(i+1) - g(i-1))/g(i)
     end do

     Cg(0)  = Cg(1)
     Cg(Nx) = Cg(Nx-1)

  end if

! Csigma.

  if (flag_Csigma) then

     if (shift == "harmonic") then

        do i=1,Nx-1
           Csigma(i) = Dsigma(i) - half*idx*(sigma(i+1) - sigma(i-1))
        end do

        Csigma(0)  = Csigma(1)
        Csigma(Nx) = Csigma(Nx-1)

     else

        Csigma = 0.0D0

     end if

  end if


! ***************
! ***   END   ***
! ***************

  end subroutine constraints
