! $Header: /usr/local/ollincvs/Codes/OllinToy/src/eigenfields.f90,v 1.9 2005/04/27 19:15:40 malcubi Exp $

  subroutine eigenfields

! *************************************
! ***   EVALUATION OF EIGENFIELDS   ***
! *************************************

! This subroutine evaluates the different eigenfields.

! Include modules.

  use param
  use arrays

! Extra variables.

  implicit none


! *******************************
! ***   SLICING EIGENFIELDS   ***
! *******************************

! Eigenspeeds:
!
! lambda0  = - beta
! lambdafp = - beta + alpha sqrt(f/g)
! lambdafm = - beta - alpha sqrt(f/g)
! lambdahp = - beta + alpha sqrt(h/g)
! lambdahm = - beta - alpha sqrt(h/g)

  lambda0  = 0.0D0
  lambdafp = + alpha*dsqrt(f/g)
  lambdafm = - alpha*dsqrt(f/g)

  if (shift /= "none") then
     lambda0  = lambda0  - beta
     lambdafp = lambdafp - beta
     lambdafm = lambdafm - beta
  end if

  if (shift == "harmonic") then
     lambdahp = + alpha*dsqrt(h/g) - beta
     lambdahm = - alpha*dsqrt(h/g) - beta
  end if

! Eigenfield with zero speed.
!
! W0  =  Dalpha / f - Dg / 2

  W0 = Dalpha/f - 0.5D0*Dg

! Eigenfields associated with (Dalpha,Ktilde):
!
! Wfp  =  Ktilde + Dalpha / sqrt(f)
! Wfm  =  Ktilde - Dalpha / sqrt(f)

  Wfp = Ktilde + Dalpha/dsqrt(f)
  Wfm = Ktilde - Dalpha/dsqrt(f)

! Shift eigenfields:
!
! Whp  =  ( 1 / sqrt(g h) + sigma ) Ktilde  +  Dg / 2 sqrt(g)  -  Dsigma / sqrt(h)
! Whm  =  ( 1 / sqrt(g h) - sigma ) Ktilde  -  Dg / 2 sqrt(g)  -  Dsigma / sqrt(h)

  if (shift == "harmonic") then
     Whp = (1.0D0/dsqrt(g*h) + sigma)*Ktilde + 0.5D0*Dg/dsqrt(g) - Dsigma/dsqrt(h)
     Whm = (1.0D0/dsqrt(g*h) - sigma)*Ktilde - 0.5D0*Dg/dsqrt(g) - Dsigma/dsqrt(h)
  end if


! ***************
! ***   END   ***
! ***************

  end subroutine eigenfields
