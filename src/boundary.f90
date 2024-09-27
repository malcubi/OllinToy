!$Header: /usr/local/ollincvs/Codes/OllinToy/src/boundary.f90,v 1.4 2005/03/17 01:10:49 malcubi Exp $

  subroutine boundary

! *****************************
! ***  BOUNDARY CONDITIONS  ***
! *****************************

! Here we impose bounday conditions.  I only impose here
! boundary conditions not related to eigenfield information
! like "flat" or "periodic", since other boundary conditions
! are imposed directly through the sources routine.

! Include modules.

  use param
  use arrays

! Extra variables.

  implicit none


! ****************
! ***   FLAT   ***
! ****************

! This is a very simple (and not particularly good) boundary
! condition that simply copies values to the boundaries from
! one point in.

  if (boundtype=="flat") then

!    Left boundary.

     Dalpha(0) = Dalpha(1)
     Dg(0)     = Dg(1)
     Ktilde(0) = Ktilde(1)

     Wfp(0) = Wfp(1)
     Wfm(0) = Wfm(1)
     W0(0)  = W0(1)

!    Right boundary.

     Dalpha(Nx) = Dalpha(Nx-1)
     Dg(Nx)     = Dg(Nx-1)
     Ktilde(Nx) = Ktilde(Nx-1)

     Wfp(Nx) = Wfp(Nx-1)
     Wfm(Nx) = Wfm(Nx-1)
     W0(Nx)  = W0(Nx-1)


! ********************
! ***   PERIODIC   ***
! ********************

! Periodic boundary conditions are implemented by making the
! last point on one side equal to the second point on the other
! side for all variables.

  else if (boundtype=="periodic") then

!    Left boundary.

     alpha(0) = alpha(Nx-1)
     g(0)     = g(Nx-1)

     Dalpha(0) = Dalpha(Nx-1)
     Dg(0)     = Dg(Nx-1)
     Ktilde(0) = Ktilde(Nx-1)

     Wfp(0) = Wfp(Nx-1)
     Wfm(0) = Wfm(Nx-1)
     W0(0)  = W0(Nx-1)

!    Right boundary.

     alpha(Nx) = alpha(1)
     g(Nx)     = g(1)

     Dalpha(Nx) = Dalpha(1)
     Dg(Nx)     = Dg(1)
     Ktilde(Nx) = Ktilde(1)

     Wfp(Nx) = Wfp(1)
     Wfm(Nx) = Wfm(1)
     W0(Nx)  = W0(1)

  end if


! ***************
! ***   END   ***
! ***************

  end subroutine boundary
