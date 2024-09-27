!$Header: /usr/local/ollincvs/Codes/OllinToy/src/sources.f90,v 1.28 2005/06/10 18:38:53 malcubi Exp $

  subroutine sources

! ******************************************
! ***  SOURCES FOR EVOLUTION EQUATIONS   ***
! ******************************************

! This routine calculates the sources for the evolution equations.
! There are several different methods coded for the spatial
! derivatives:
!
! 1) "center"     This uses simple centered differences for the
!                 spatial derivatives.  It is easy to implement
!                 and stable. It is also in fact conservative,
!                 as one can always write:
!
!                 source = ( F      -  F      ) / dx
!                             i+1/2     i-1/2
!
!                 where:
!
!                 F      = ( F  + F    ) / 2
!                  i+1/2      i    i+1
!
!                 which implies:
!
!                 source = ( F    - F   ) / 2 dx
!                             i+1    i-1
!
! 2) "upwind"     Upwinded differenced in eigenfields.  This is
!                 also conservative, but is only first order.
!
! 3) "minmod"     Minmod slope limiter (see LeVeque)
!
! 4) "vanleer"    van Leer slope limiter (see LeVeque)
!
! Slope limiters are conservative and second order accurate except at
! extrema where they become only first order accurate.  They are much
! better behaved for large gradients that the "center" method.
!
! In all cases, I construct the sources for the eigenfields first,
! and from those I reconstruct the sources for (Dalpha,Dg,Ktilde).
! The sources for the eigenfields are:
!
!       sWfp = - d  ( lambdafp Wfp )
!                 x
!
!       sWfm = - d  ( lambdafm Wfm )
!                 x
!                                     2
!       sW0  = - d  ( lambda0 Wf0) - d  beta
!                 x                   x
!
! Notice that the source for W0 has an extra contribution from
! derivatives of the shift.  Also, when there is no shift lambda0=0,
! so W0 does not evolve.
!
! The routine also calculates the sources for the characteristic
! surfaces.  These surfaces are represented by scalar functions
! (charp,charm) that remain are constant along the corresponding
! characteristics, or in other words, the level sets of these
! functions track individual characteristic lines.  As the functions
! are constant along characteristics, their sources are very simple:
!
!       scharp =  - lambdafp d ( charp )
!                            x
!
!       scharm =  - lambdafm d ( charm )
!                            x
!
! Notice one important point about notation:  The fluxes are supposed
! to be evaluated between grid points, but since I can't use half-integer
! arguments on an array, I take F(i) to mean the flux evaluated at i+1/2.
! Also, since the fluxes in principle extend half a grid point beyond
! the grid in both directions, they are arrays that go from -1 to Nx.

! Include modules.

  use param
  use arrays

! Extra variables.

  implicit none

  integer i

  real(8) slope1,slope2
  real(8) slopecorrect
  real(8) idx,halfidx
  real(8) zero,half,one,two

  real(8), dimension (-1:Nx) :: flux,flux0,fluxfp,fluxfm,fluxhp,fluxhm

! The variables defined above are:
!
! idx           1/dx
! halfidx       1/(2 dx)
!
! slope1        Slope of flux to the right of grid point under consideration.
! slope2        Slope of flux to the left of grid point under consideration.
!
! flux          Array for fluxes in evolution of eigenfields.


! *******************
! ***   NUMBERS   ***
! *******************

  zero  = 0.0D0
  half  = 0.5D0
  one   = 1.0D0
  two   = 2.0D0

  idx = one/dx
  halfidx = half*idx


! ***********************************************
! ***   SOURCES FOR LAPSE, METRIC AND SHIFT   ***
! ***********************************************

! The source for the lapse is:
!
!                    2
! d alpha  =  - alpha  f(alpha) Ktilde / sqrt(g)  +  alpha beta Dalpha
!  t                            

  salpha = - alpha**2*f*Ktilde/dsqrt(g)

  if (shift /= "none") then
     salpha = salpha + alpha*beta*Dalpha
  end if

! The source for the metric is:
!
! d g  =  - 2 alpha sqrt(g) Ktilde  +  g beta Dg  +  2 g Dbeta 
!  t

  sg = - two*alpha*dsqrt(g)*Ktilde

  if (shift /= "none") then
     sg = sg + g*(beta*Dg + two*Dbeta)
  end if

! The source for the rescaled shift is:
!
! d sigma  =  alpha [ - Dalpha/g + sigma*Dsigma + h ( Dg / 2g + sigma Ktilde / sqrt(g) )
!  t

  if (shift == "harmonic") then
     ssigma = alpha*(sigma*Dsigma - Dalpha/g + h*(half*Dg/g + sigma*Ktilde/dsqrt(g)))
  end if


! ***************************************
! ***   SIMPLE CENTERED DIFFERENCES   ***
! ***************************************

! Simple centered differences.

  if (method=="center") then

!    Sources at interior points for (Wfp,Wfm).

     do i=1,Nx-1
        sWfp(i) = - (lambdafp(i+1)*Wfp(i+1) - lambdafp(i-1)*Wfp(i-1))*halfidx
        sWfm(i) = - (lambdafm(i+1)*Wfm(i+1) - lambdafm(i-1)*Wfm(i-1))*halfidx
     end do

!    Sources at interior points for W0.

     if (shift == "none") then
        sW0 = zero
     else
        do i=1,Nx-1
           sW0(i) = - ((lambda0(i+1)*W0(i+1) - lambda0(i-1)*W0(i-1)) &
                    + (Dbeta(i+1) - Dbeta(i-1)))*halfidx
        end do
     end if

!    Sources at interior points for (Whp,Whm).

     if (shift == "harmonic") then
        do i=1,Nx-1
           sWhp(i) = - (lambdahp(i+1)*Whp(i+1) - lambdahp(i-1)*Whp(i-1))*halfidx
           sWhm(i) = - (lambdahm(i+1)*Whm(i+1) - lambdahm(i-1)*Whm(i-1))*halfidx
        end do
     end if

!    Sources at left boundary. For outgoing eigenfields we use
!    second order one-sided differences.  Sources for incoming
!    fields are done later.

     if ((lambda0(0) < zero).and.(shift /= "none")) then
        sW0(0) = - (2.0D0*(lambda0(1)*W0(1) - lambda0(0)*W0(0)) &
                 -  0.5D0*(lambda0(2)*W0(2) - lambda0(0)*W0(0)))*idx &
                 - (2.0D0*(Dbeta(1) - Dbeta(0)) &
                 -  0.5D0*(Dbeta(2) - Dbeta(0)))*idx
     end if

     if (lambdafp(0) < zero) then
        sWfp(0) = - (2.0D0*(lambdafp(1)*Wfp(1) - lambdafp(0)*Wfp(0)) &
                   - 0.5D0*(lambdafp(2)*Wfp(2) - lambdafp(0)*Wfp(0)))*idx
     end if

     if (lambdafm(0) < zero) then
        sWfm(0) = - (2.0D0*(lambdafm(1)*Wfm(1) - lambdafm(0)*Wfm(0)) &
                   - 0.5D0*(lambdafm(2)*Wfm(2) - lambdafm(0)*Wfm(0)))*idx
     end if

     if (shift == "harmonic") then

        if (lambdahp(0) < zero) then
           sWhp(0) = - (2.0D0*(lambdahp(1)*Whp(1) - lambdahp(0)*Whp(0)) &
                      - 0.5D0*(lambdahp(2)*Whp(2) - lambdahp(0)*Whp(0)))*idx
        end if

        if (lambdahm(0) < zero) then
           sWhm(0) = - (2.0D0*(lambdahm(1)*Whm(1) - lambdahm(0)*Whm(0)) &
                      - 0.5D0*(lambdahm(2)*Whm(2) - lambdahm(0)*Whm(0)))*idx
        end if

     end if

!    Sources at right boundary. For outgoing eigenfields we use
!    second order one-sided differences.  Sources for incoming
!    fields are done later.

     if ((lambda0(Nx) > zero).and.(shift /= "none")) then
        sW0(Nx) = - (2.0D0*(lambda0(Nx)*W0(Nx) - lambda0(Nx-1)*W0(Nx-1)) &
                  -  0.5D0*(lambda0(Nx)*W0(Nx) - lambda0(Nx-2)*W0(Nx-2)))*idx &
                  - (2.0D0*(Dbeta(Nx) - Dbeta(Nx-1)) &
                  -  0.5D0*(Dbeta(Nx) - Dbeta(Nx-2)))*idx
     end if

     if (lambdafp(Nx) > zero) then
        sWfp(Nx) = - (2.0D0*(lambdafp(Nx)*Wfp(Nx) - lambdafp(Nx-1)*Wfp(Nx-1)) &
                    - 0.5D0*(lambdafp(Nx)*Wfp(Nx) - lambdafp(Nx-2)*Wfp(Nx-2)))*idx
     end if

     if (lambdafm(Nx) > zero) then
        sWfm(Nx) = - (2.0D0*(lambdafm(Nx)*Wfm(Nx) - lambdafm(Nx-1)*Wfm(Nx-1)) &
                    - 0.5D0*(lambdafm(Nx)*Wfm(Nx) - lambdafm(Nx-2)*Wfm(Nx-2)))*idx
     end if

     if (shift == "harmonic") then

        if (lambdahp(Nx) > zero) then
           sWhp(Nx) = - (2.0D0*(lambdahp(Nx)*Whp(Nx) - lambdahp(Nx-1)*Whp(Nx-1)) &
                       - 0.5D0*(lambdahp(Nx)*Whp(Nx) - lambdahp(Nx-2)*Whp(Nx-2)))*idx
        end if

        if (lambdahm(Nx) > zero) then
           sWhm(Nx) = - (2.0D0*(lambdahm(Nx)*Whm(Nx) - lambdahm(Nx-1)*Whm(Nx-1)) &
                       - 0.5D0*(lambdahm(Nx)*Whm(Nx) - lambdahm(Nx-2)*Whm(Nx-2)))*idx
        end if

     end if

!    Characteristic surfaces.

     if (trackchar) then

        do i=1,Nx-1
           schar0(i) = - lambda0(i)*(char0(i+1) - char0(i-1))*halfidx
           scharp(i) = - lambdafp(i)*(charp(i+1) - charp(i-1))*halfidx
           scharm(i) = - lambdafm(i)*(charm(i+1) - charm(i-1))*halfidx
        end do

!       The boundaries are just copied.  This is OK, as these
!       quantities do not feed back into the evolution.

        schar0(0) = schar0(1)
        scharp(0) = scharp(1)
        scharm(0) = scharm(1)

        schar0(Nx) = schar0(Nx-1)
        scharp(Nx) = scharp(Nx-1)
        scharm(Nx) = scharm(Nx-1)

     end if


! ******************
! ***   UPWIND   ***
! ******************

! Upwinded differences (first order).

  else if (method=="upwind") then

!    Sources at interior points for (Wfp,Wfm)

     do i=1,Nx-1

        if (lambdafp(i) > zero) then
           sWfp(i) = - (lambdafp(i  )*Wfp(i  ) - lambdafp(i-1)*Wfp(i-1))*idx
        else
           sWfp(i) = - (lambdafp(i+1)*Wfp(i+1) - lambdafp(i  )*Wfp(i  ))*idx
        end if

        if (lambdafm(i) > zero) then
           sWfm(i) = - (lambdafm(i  )*Wfm(i  ) - lambdafm(i-1)*Wfm(i-1))*idx
        else
           sWfm(i) = - (lambdafm(i+1)*Wfm(i+1) - lambdafm(i  )*Wfm(i  ))*idx
        end if

     end do

!    Sources at interior points for W0.

     if (shift == "none") then
        sW0 = zero
     else
        do i=1,Nx-1
           if (lambda0(i) > zero) then
              sW0(i) = - (lambda0(i  )*W0(i  ) - lambda0(i-1)*W0(i-1))*idx
           else
              sW0(i) = - (lambda0(i+1)*W0(i+1) - lambda0(i  )*W0(i  ))*idx
           end if
           sW0(i) = sW0(i) - (Dbeta(i+1) - Dbeta(i-1))*halfidx
        end do
     end if

!    Sources at interior points for (Whp,Whm).

     if (shift == "harmonic") then
        do i=1,Nx-1

           if (lambdahp(i) > zero) then
              sWhp(i) = - (lambdahp(i  )*Whp(i  ) - lambdahp(i-1)*Whp(i-1))*idx
           else
              sWhp(i) = - (lambdahp(i+1)*Whp(i+1) - lambdahp(i  )*Whp(i  ))*idx
           end if

           if (lambdahm(i) > zero) then
              sWhm(i) = - (lambdahm(i  )*Whm(i  ) - lambdahm(i-1)*Whm(i-1))*idx
           else
              sWhm(i) = - (lambdahm(i+1)*Whm(i+1) - lambdahm(i  )*Whm(i  ))*idx
           end if

        end do
     end if

!    Sources at left boundary. For outgoing eigenfields we use
!    first order one-sided differences.  Sources for incoming
!    fields are done later.

     if ((lambda0(0) < zero).and.(shift /= "none")) then
        sW0(0) = - (lambda0(1)*W0(1) - lambda0(0)*W0(0))*idx &
                 - (Dbeta(1) - Dbeta(0))*idx
     end if

     if (lambdafp(0) < zero) then
        sWfp(0) = - (lambdafp(1)*Wfp(1) - lambdafp(0)*Wfp(0))*idx
     end if

     if (lambdafm(0) < zero) then
        sWfm(0) = - (lambdafm(1)*Wfm(1) - lambdafm(0)*Wfm(0))*idx
     end if

     if (shift == "harmonic") then

        if (lambdahp(0) < zero) then
           sWhp(0) = - (lambdahp(1)*Whp(1) - lambdahp(0)*Whp(0))*idx
        end if

        if (lambdahm(0) < zero) then
           sWhm(0) = - (lambdahm(1)*Whm(1) - lambdahm(0)*Whm(0))*idx
        end if

     end if

!    Sources at right boundary. For outgoing eigenfields we use
!    first order one-sided differences.  Sources for incoming
!    fields are done later.

     if ((lambda0(Nx) > zero).and.(shift /= "none")) then
        sW0(Nx) = - (lambda0(Nx)*W0(Nx) - lambda0(Nx-1)*W0(Nx-1))*idx &
                  - (Dbeta(Nx) - Dbeta(Nx-1))*idx
     end if

     if (lambdafp(Nx) > zero) then
        sWfp(Nx) = - (lambdafp(Nx)*Wfp(Nx) - lambdafp(Nx-1)*Wfp(Nx-1))*idx
     end if

     if (lambdafm(Nx) > zero) then
        sWfm(Nx) = - (lambdafm(Nx)*Wfm(Nx) - lambdafm(Nx-1)*Wfm(Nx-1))*idx
     end if

     if (shift == "harmonic") then

        if (lambdahp(Nx) > zero) then
           sWhp(Nx) = - (lambdahp(Nx)*Whp(Nx) - lambdahp(Nx-1)*Whp(Nx-1))*idx
        end if

        if (lambdahm(Nx) > zero) then
           sWhm(Nx) = - (lambdahm(Nx)*Whm(Nx) - lambdahm(Nx-1)*Whm(Nx-1))*idx
        end if

     end if

!    Characteristic surfaces.

     if (trackchar) then

        do i=1,Nx-1

           if (lambda0(i)>=zero) then
              schar0(i) = - lambda0(i)*(char0(i  ) - char0(i-1))*idx
           else
              schar0(i) = - lambda0(i)*(char0(i+1) - char0(i  ))*idx
           end if

           scharp(i) = - lambdafp(i)*(charp(i  ) - charp(i-1))*idx
           scharm(i) = - lambdafm(i)*(charm(i+1) - charm(i  ))*idx

        end do

!       The boundaries are just copied.  This is OK, as these
!       quantities do not feed back into the evolution.

        schar0(0) = schar0(1)
        scharp(0) = scharp(1)
        scharm(0) = scharm(1)

        schar0(Nx) = schar0(Nx-1)
        scharp(Nx) = scharp(Nx-1)
        scharm(Nx) = scharm(Nx-1)

     end if


! **************************
! ***   SLOPE LIMITERS   ***
! **************************

! Here we use a slope limiter method that is second order
! except at extrema of the functions.  We can use either
! minmod or vanleer.

  else if ((method=="minmod").or.(method=="vanleer")) then

!    Find fluxes on last two points on left boundary. For outgoing
!    fields we use the slope limiter, except for the fact that at the
!    very last point we only have one slope. For incoming fields we
!    average to obtain the flux at 1/2 and copy that to -1/2
!    to make sure the source vanishes at the boundary (this is not
!    important, as boundaries for incoming fields are done again later).

     if (lambda0(0) > zero) then
        flux0(0 ) = half*(lambda0(0)*W0(0) + lambda0(1)*W0(1))
        if (shift /= "none") then
           flux0( 0) = flux0( 0) + half*(Dbeta(0) + Dbeta(1))
        end if
        flux0(-1) = flux0(0)
     else
        slope1 = lambda0(1)*W0(1) - lambda0(0)*W0(0)
        slope2 = lambda0(2)*W0(2) - lambda0(1)*W0(1)
        flux0( 0) = lambda0(1)*W0(1) - half*slopecorrect(slope1,slope2,method)
        flux0(-1) = lambda0(0)*W0(0) - half*slope1
        if (shift /= "none") then
           flux0( 0) = flux0( 0) + half*(Dbeta(0) + Dbeta(1))
           flux0(-1) = flux0(-1) + half*(3.0D0*Dbeta(0) - Dbeta(1))
        end if
     end if

     if (lambdafp(0) > zero) then
        fluxfp(0 ) = half*(lambdafp(0)*Wfp(0) + lambdafp(1)*Wfp(1))
        fluxfp(-1) = fluxfp(0)
     else
        slope1 = lambdafp(1)*Wfp(1) - lambdafp(0)*Wfp(0)
        slope2 = lambdafp(2)*Wfp(2) - lambdafp(1)*Wfp(1)
        fluxfp( 0) = lambdafp(1)*Wfp(1) - half*slopecorrect(slope1,slope2,method)
        fluxfp(-1) = lambdafp(0)*Wfp(0) - half*slope1
     end if

     if (lambdafm(0) > zero) then
        fluxfm(0 ) = half*(lambdafm(0)*Wfm(0) + lambdafm(1)*Wfm(1))
        fluxfm(-1) = fluxfm(0)
     else
        slope1 = lambdafm(1)*Wfm(1) - lambdafm(0)*Wfm(0)
        slope2 = lambdafm(2)*Wfm(2) - lambdafm(1)*Wfm(1)
        fluxfm( 0) = lambdafm(1)*Wfm(1) - half*slopecorrect(slope1,slope2,method)
        fluxfm(-1) = lambdafm(0)*Wfm(0) - half*slope1
     end if

     if (shift == "harmonic") then

        if (lambdahp(0) > zero) then
           fluxhp(0 ) = half*(lambdahp(0)*Whp(0) + lambdahp(1)*Whp(1))
           fluxhp(-1) = fluxhp(0)
        else
           slope1 = lambdahp(1)*Whp(1) - lambdahp(0)*Whp(0)
           slope2 = lambdahp(2)*Whp(2) - lambdahp(1)*Whp(1)
           fluxhp( 0) = lambdahp(1)*Whp(1) - half*slopecorrect(slope1,slope2,method)
           fluxhp(-1) = lambdahp(0)*Whp(0) - half*slope1
        end if

        if (lambdahm(0) > zero) then
           fluxhm(0 ) = half*(lambdahm(0)*Whm(0) + lambdahm(1)*Whm(1))
           fluxhm(-1) = fluxhm(0)
        else
           slope1 = lambdahm(1)*Whm(1) - lambdahm(0)*Whm(0)
           slope2 = lambdahm(2)*Whm(2) - lambdahm(1)*Whm(1)
           fluxhm( 0) = lambdahm(1)*Whm(1) - half*slopecorrect(slope1,slope2,method)
           fluxhm(-1) = lambdahm(0)*Whm(0) - half*slope1
        end if

     end if

!    Find fluxes on last two points on right boundary.  For outgoing
!    fields we use the slope limiter, except for the fact that at the
!    very last point we only have one slope. For incoming fields we
!    average to obtain the flux at Nx-1/2 and copy that to Nx+1/2
!    to make sure the source vanishes at the boundary (this is not
!    important, as boundaries for incoming fields are done again later).

     if (lambda0(Nx) > zero) then
        slope1 = lambda0(Nx  )*W0(Nx  ) - lambda0(Nx-1)*W0(Nx-1)
        slope2 = lambda0(Nx-1)*W0(Nx-1) - lambda0(Nx-2)*W0(Nx-2)
        flux0(Nx  ) = lambda0(Nx  )*W0(Nx  ) + half*slope1
        flux0(Nx-1) = lambda0(Nx-1)*W0(Nx-1) + half*slopecorrect(slope1,slope2,method)
        if (shift /= "none") then
           flux0(Nx-1) = flux0(Nx-1) + half*(Dbeta(Nx-1) + Dbeta(Nx))
           flux0(Nx  ) = flux0(Nx  ) + half*(3.0D0*Dbeta(Nx) - Dbeta(Nx-1))
        end if
     else
        flux0(Nx-1) = half*(lambda0(Nx-1)*W0(Nx-1) + lambda0(Nx)*W0(Nx))
        if (shift /= "none") then
           flux0(Nx-1) = flux0(Nx-1) + half*(Dbeta(Nx-1) + Dbeta(Nx))
        end if
        flux0(Nx  ) = flux0(Nx-1)
     end if

     if (lambdafp(Nx) > zero) then
        slope1 = lambdafp(Nx  )*Wfp(Nx  ) - lambdafp(Nx-1)*Wfp(Nx-1)
        slope2 = lambdafp(Nx-1)*Wfp(Nx-1) - lambdafp(Nx-2)*Wfp(Nx-2)
        fluxfp(Nx  ) = lambdafp(Nx  )*Wfp(Nx  ) + half*slope1
        fluxfp(Nx-1) = lambdafp(Nx-1)*Wfp(Nx-1) + half*slopecorrect(slope1,slope2,method)
     else
        fluxfp(Nx-1) = half*(lambdafp(Nx-1)*Wfp(Nx-1) + lambdafp(Nx)*Wfp(Nx))
        fluxfp(Nx  ) = fluxfp(Nx-1)
     end if

     if (lambdafm(Nx) > zero) then
        slope1 = lambdafm(Nx  )*Wfm(Nx  ) - lambdafm(Nx-1)*Wfm(Nx-1)
        slope2 = lambdafm(Nx-1)*Wfm(Nx-1) - lambdafm(Nx-2)*Wfm(Nx-2)
        fluxfm(Nx  ) = lambdafm(Nx  )*Wfm(Nx  ) + half*slope1
        fluxfm(Nx-1) = lambdafm(Nx-1)*Wfm(Nx-1) + half*slopecorrect(slope1,slope2,method)
     else
        fluxfm(Nx-1) = half*(lambdafm(Nx-1)*Wfm(Nx-1) + lambdafm(Nx)*Wfm(Nx))
        fluxfm(Nx  ) = fluxfm(Nx-1)
     end if

     if (shift == "harmonic") then

        if (lambdahp(Nx) > zero) then
           slope1 = lambdahp(Nx  )*Whp(Nx  ) - lambdahp(Nx-1)*Whp(Nx-1)
           slope2 = lambdahp(Nx-1)*Whp(Nx-1) - lambdahp(Nx-2)*Whp(Nx-2)
           fluxhp(Nx  ) = lambdahp(Nx  )*Whp(Nx  ) + half*slope1
           fluxhp(Nx-1) = lambdahp(Nx-1)*Whp(Nx-1) + half*slopecorrect(slope1,slope2,method)
        else
           fluxhp(Nx-1) = half*(lambdahp(Nx-1)*Whp(Nx-1) + lambdahp(Nx)*Whp(Nx))
           fluxhp(Nx  ) = fluxhp(Nx-1)
        end if

        if (lambdahm(Nx) > zero) then
           slope1 = lambdahm(Nx  )*Whm(Nx  ) - lambdahm(Nx-1)*Whm(Nx-1)
           slope2 = lambdahm(Nx-1)*Whm(Nx-1) - lambdahm(Nx-2)*Whm(Nx-2)
           fluxhm(Nx  ) = lambdahm(Nx  )*Whm(Nx  ) + half*slope1
           fluxhm(Nx-1) = lambdahm(Nx-1)*Whm(Nx-1) + half*slopecorrect(slope1,slope2,method)
        else
           fluxhm(Nx-1) = half*(lambdahm(Nx-1)*Whm(Nx-1) + lambdahm(Nx)*Whm(Nx))
           fluxhm(Nx  ) = fluxhm(Nx-1)
        end if

     end if

!    Find interior fluxes. The fluxes are given by the upwind value
!    plus a correction given by the slope limiter.

!    W0.

     if (shift == "none") then
        flux0 = zero
     else
        do i=1,Nx-2
           if (lambda0(i) > zero) then
              slope1 = lambda0(i+1)*W0(i+1) - lambda0(i  )*W0(i  )
              slope2 = lambda0(i  )*W0(i  ) - lambda0(i-1)*W0(i-1)
              flux0(i) = lambda0(i  )*W0(i  ) + half*slopecorrect(slope1,slope2,method)
           else
              slope1 = lambda0(i+2)*W0(i+2) - lambda0(i+1)*W0(i+1)
              slope2 = lambda0(i+1)*W0(i+1) - lambda0(i  )*W0(i  )
              flux0(i) = lambda0(i+1)*W0(i+1) - half*slopecorrect(slope1,slope2,method)
           end if
           flux0(i) = flux0(i) + half*(Dbeta(i) + Dbeta(i+1))
        end do
     end if

!    (Wfp,Wfm).

     do i=1,Nx-2

        if (lambdafp(i) > zero) then
           slope1 = lambdafp(i+1)*Wfp(i+1) - lambdafp(i  )*Wfp(i  )
           slope2 = lambdafp(i  )*Wfp(i  ) - lambdafp(i-1)*Wfp(i-1)
           fluxfp(i) = lambdafp(i  )*Wfp(i  ) + half*slopecorrect(slope1,slope2,method)
        else
           slope1 = lambdafp(i+2)*Wfp(i+2) - lambdafp(i+1)*Wfp(i+1)
           slope2 = lambdafp(i+1)*Wfp(i+1) - lambdafp(i  )*Wfp(i  )
           fluxfp(i) = lambdafp(i+1)*Wfp(i+1) - half*slopecorrect(slope1,slope2,method)
        end if

        if (lambdafm(i) > zero) then
           slope1 = lambdafm(i+1)*Wfm(i+1) - lambdafm(i  )*Wfm(i  )
           slope2 = lambdafm(i  )*Wfm(i  ) - lambdafm(i-1)*Wfm(i-1)
           fluxfm(i) = lambdafm(i  )*Wfm(i  ) + half*slopecorrect(slope1,slope2,method)
        else
           slope1 = lambdafm(i+2)*Wfm(i+2) - lambdafm(i+1)*Wfm(i+1)
           slope2 = lambdafm(i+1)*Wfm(i+1) - lambdafm(i  )*Wfm(i  )
           fluxfm(i) = lambdafm(i+1)*Wfm(i+1) - half*slopecorrect(slope1,slope2,method)
        end if

     end do

!    (Whp,Whm).

     if (shift == "harmonic") then
        do i=1,Nx-2

           if (lambdahp(i) > zero) then
              slope1 = lambdahp(i+1)*Whp(i+1) - lambdahp(i  )*Whp(i  )
              slope2 = lambdahp(i  )*Whp(i  ) - lambdahp(i-1)*Whp(i-1)
              fluxhp(i) = lambdahp(i  )*Whp(i  ) + half*slopecorrect(slope1,slope2,method)
           else
              slope1 = lambdahp(i+2)*Whp(i+2) - lambdahp(i+1)*Whp(i+1)
              slope2 = lambdahp(i+1)*Whp(i+1) - lambdahp(i  )*Whp(i  )
              fluxhp(i) = lambdahp(i+1)*Whp(i+1) - half*slopecorrect(slope1,slope2,method)
           end if

           if (lambdahm(i) > zero) then
              slope1 = lambdahm(i+1)*Whm(i+1) - lambdahm(i  )*Whm(i  )
              slope2 = lambdahm(i  )*Whm(i  ) - lambdahm(i-1)*Whm(i-1)
              fluxhm(i) = lambdahm(i  )*Whm(i  ) + half*slopecorrect(slope1,slope2,method)
           else
              slope1 = lambdahm(i+2)*Whm(i+2) - lambdahm(i+1)*Whm(i+1)
              slope2 = lambdahm(i+1)*Whm(i+1) - lambdahm(i  )*Whm(i  )
              fluxhm(i) = lambdahm(i+1)*Whm(i+1) - half*slopecorrect(slope1,slope2,method)
           end if

        end do
     end if

!    Find sources for eigenfields.

     do i=0,Nx
        sW0(i)  = - (flux0(i)  - flux0(i-1))*idx
        sWfp(i) = - (fluxfp(i) - fluxfp(i-1))*idx
        sWfm(i) = - (fluxfm(i) - fluxfm(i-1))*idx
     end do

     if (shift == "harmonic") then
        do i=0,Nx
           sWhp(i) = - (fluxhp(i) - fluxhp(i-1))*idx
           sWhm(i) = - (fluxhm(i) - fluxhm(i-1))*idx
        end do
     end if

!    Characteristic surfaces.  The algorithm here is the same as
!    above and has less comments to save on typing.

     if (trackchar) then

!       Interior points.

        do i=1,Nx-2

           if (lambdafp(i) > zero) then
              slope1 = charp(i+1) - charp(i  )
              slope2 = charp(i  ) - charp(i-1)
              fluxfp(i) = charp(i  ) + half*slopecorrect(slope1,slope2,method)
           else
              slope1 = charp(i+2) - charp(i+1)
              slope2 = charp(i+1) - charp(i  )
              fluxfp(i) = charp(i+1) - half*slopecorrect(slope1,slope2,method)
           end if

           if (lambdafm(i) > zero) then
              slope1 = charm(i+1) - charm(i  )
              slope2 = charm(i  ) - charm(i-1)
              fluxfm(i) = charm(i  ) + half*slopecorrect(slope1,slope2,method)
           else
              slope1 = charm(i+2) - charm(i+1)
              slope2 = charm(i+1) - charm(i  )
              fluxfm(i) = charm(i+1) - half*slopecorrect(slope1,slope2,method)
           end if

        end do

!       Fluxes on left boundary.

        if (lambdafp(0) > zero) then
           fluxfp(0 ) = half*(charp(0) + charp(1))
           fluxfp(-1) = two*fluxfp(0) - fluxfp(1)
        else
           slope1 = charp(1) - charp(0)
           slope2 = charp(2) - charp(1)
           fluxfp( 0) = charp(1) - half*slopecorrect(slope1,slope2,method)
           fluxfp(-1) = charp(0) - half*slope1
        end if

        if (lambdafm(0) > zero) then
           fluxfm(0 ) = half*(charm(0) + charm(1))
           fluxfm(-1) = two*fluxfm(0) - fluxfm(1)
        else
           slope1 = charm(1) - charm(0)
           slope2 = charm(2) - charm(1)
           fluxfm( 0) = charm(1) - half*slopecorrect(slope1,slope2,method)
           fluxfm(-1) = charm(0) - half*slope1
        end if

!       Fluxes on right boundary.

        if (lambdafp(Nx) > zero) then
           slope1 = charp(Nx  ) - charp(Nx-1)
           slope2 = charp(Nx-1) - charp(Nx-2)
           fluxfp(Nx  ) = charp(Nx  ) + half*slope1
           fluxfp(Nx-1) = charp(Nx-1) + half*slopecorrect(slope1,slope2,method)
        else
           fluxfp(Nx-1) = half*(charp(Nx-1) + charp(Nx))
           fluxfp(Nx  ) = two*fluxfp(Nx-1) - fluxfp(Nx-2)
        end if

        if (lambdafm(Nx) > zero) then
           slope1 = charm(Nx  ) - charm(Nx-1)
           slope2 = charm(Nx-1) - charm(Nx-2)
           fluxfm(Nx  ) = charm(Nx  ) + half*slope1
           fluxfm(Nx-1) = charm(Nx-1) + half*slopecorrect(slope1,slope2,method)
        else
           fluxfm(Nx-1) = half*(charm(Nx-1) + charm(Nx))
           fluxfm(Nx  ) = two*fluxfm(Nx-1) - fluxfm(Nx-2)
        end if

!       Find sources.

        do i=0,Nx
           scharp(i) = - lambdafp(i)*(fluxfp(i) - fluxfp(i-1))*idx
           scharm(i) = - lambdafm(i)*(fluxfm(i) - fluxfm(i-1))*idx
        end do

     end if

  end if


! ******************************
! ***   EXTRA SOURCE TERMS   ***
! ******************************

! Here we add pure source terms, that is, those that involve
! no derivatives of the fields.
!
! In fact, only the equations for the shift eigenfields (Whp,Whm)
! involve source terms, the equations for all other eigenfields
! are fully conservative.
!
! The pure source terms are:  (hp := dh / dalpha)
!
! sWhp  =  alpha / (8 h sqrt(g)) 
!        ( ( 2 (h - sqrt(f h)) (1 + h) + alpha hp (f - sqrt(f h)) ) Wfp Whp
!       +  ( 2 (h + sqrt(f h)) (1 - h) + alpha hp (f + sqrt(f h)) ) Wfp Whm
!       +  ( 2 (h + sqrt(f h)) (1 + h) + alpha hp (f + sqrt(f h)) ) Wfm Whp
!       +  ( 2 (h - sqrt(f h)) (1 - h) + alpha hp (f - sqrt(f h)) ) Wfm Whm )
!
! sWhm  =  alpha / (8 h sqrt(g)) 
!        ( ( 2 (h - sqrt(f h)) (1 - h) + alpha hp (f - sqrt(f h)) ) Wfp Whp
!       +  ( 2 (h + sqrt(f h)) (1 + h) + alpha hp (f + sqrt(f h)) ) Wfp Whm
!       +  ( 2 (h + sqrt(f h)) (1 - h) + alpha hp (f + sqrt(f h)) ) Wfm Whp
!       +  ( 2 (h - sqrt(f h)) (1 + h) + alpha hp (f - sqrt(f h)) ) Wfm Whm )


  if (shift == "harmonic") then

     sWhp = sWhp + alpha/(8.0D0*h*dsqrt(g)) &
          *((two*(h - dsqrt(f*h))*(one + h) + alpha*hp*(f - dsqrt(f*h)))*Wfp*Whp &
          + (two*(h + dsqrt(f*h))*(one - h) + alpha*hp*(f + dsqrt(f*h)))*Wfp*Whm &
          + (two*(h + dsqrt(f*h))*(one + h) + alpha*hp*(f + dsqrt(f*h)))*Wfm*Whp &
          + (two*(h - dsqrt(f*h))*(one - h) + alpha*hp*(f - dsqrt(f*h)))*Wfm*Whm)

     sWhm = sWhm + alpha/(8.0D0*h*dsqrt(g)) &
          *((two*(h - dsqrt(f*h))*(one - h) + alpha*hp*(f - dsqrt(f*h)))*Wfp*Whp &
          + (two*(h + dsqrt(f*h))*(one + h) + alpha*hp*(f + dsqrt(f*h)))*Wfp*Whm &
          + (two*(h + dsqrt(f*h))*(one - h) + alpha*hp*(f + dsqrt(f*h)))*Wfm*Whp &
          + (two*(h - dsqrt(f*h))*(one + h) + alpha*hp*(f - dsqrt(f*h)))*Wfm*Whm)

  end if


! *****************************************
! ***   BOUNDARIES FOR INCOMING FIELDS  ***
! *****************************************

! Sources at boundaries for outgoing fields have already been
! calculated.  For incoming fields they depend on the boundary
! condition chosen.

  if (boundtype=="outgoing") then

!    In this case incoming fields have zero sources, but
!    we must check that they are really incoming.

     if (lambda0(0 ) > zero) then
        sW0(0 ) = 0.0D0
     end if

     if (lambda0(Nx) < zero) then
        sW0(Nx) = 0.0D0
     end if

     if (lambdafp(0 ) > zero) then
        sWfp(0 ) = 0.0D0
     end if

     if (lambdafp(Nx) < zero) then
        sWfp(Nx) = 0.0D0
     end if

     if (lambdafm(0 ) > zero) then
        sWfm(0 ) = 0.0D0
     end if

     if (lambdafm(Nx) < zero) then
        sWfm(Nx) = 0.0D0
     end if

  else if (boundtype=="reflectplus") then

!    Here incoming fields are set equal to the outgoing ones.
!    This doesn't really make sense when there is a non-zero
!    shift at the boundary.

     if (lambdafp(0 ) > zero) then
        sWfp(0 ) = sWfm(0 )
     end if

     if (lambdafm(Nx) < zero) then
        sWfm(Nx) = sWfp(Nx)
     end if

  else if (boundtype=="reflectminus") then

!    Here incoming fields are set equal to minus the outgoing ones.
!    This doesn't really make sense when there is a non-zero
!    shift at the boundary.

     if (lambdafp(0 ) > zero) then
        sWfp(0 ) = - sWfm(0 )
     end if

     if (lambdafm(Nx) < zero) then
        sWfm(Nx) = - sWfp(Nx)
     end if

  end if


! *******************************
! ***   RECONSTRUCT SOURCES   ***
! *******************************

! Source for Ktilde:
!
! sKtilde  =  - d ( alpha Dalpha / sqrt(g) - beta Ktilde )
!                x     
!
!          =  (sWfp + sWfm) / 2


  sKtilde = half*(sWfp + sWfm)


! Source for Dalpha:  (fp := df/dalpha)
!
! sDalpha  =  - d  ( alpha f Ktilde / sqrt(g) - beta Dalpha)
!                x
!
!          =  sqrt(f) (sWfp - sWfm) / 2  +  salpha fp Dalpha / 2 f


  sDalpha =  half*(dsqrt(f)*(sWfp - sWfm) + salpha*fp*Dalpha/f)


! Source for Dg:  (fp := df/dalpha)
!
! sDg  =  - d  ( 2 alpha Ktilde / sqrt(g) - beta Dg + 2 Dbeta ) 
!            x
!
! This has two possible values.  For a given shift (i.e. not harmonic):
!
! sDg  = (sWfp - sWfm) / sqrt(f)  -  salpha fp Dalpha / f**2  -  2 sW0
!
! And for a harmonic shift:
!
! sDg  =  sqrt(g) [(sWhp - sWhm) - sigma (sWfp + sWfm)]
!
!      + sg [(Whp - Whm) - sigma (Wfp + Wfm)] / 2 sqrt(g)
!
!      - ssigma (Wfp + Wfm) sqrt(g)


  if (shift /= "harmonic") then
     sDg = (sWfp - sWfm)/dsqrt(f) - salpha*fp*Dalpha/f**2 - two*sW0
  else
     sDg = dsqrt(g)*((sWhp - sWhm) - sigma*(sWfp + sWfm)) &
         + half*sg*((Whp - Whm) - sigma*(Wfp + Wfm))/dsqrt(g) &
         - ssigma*(Wfp + Wfm)*dsqrt(g) 
  end if


! Source for Dsigma (only for harmonic shift):
!
! sDsigma  =  d  { alpha [ sigma Dsigma - Dalpha / g
!              x
!
!          +  h ( Dg / 2g + sigma Ktilde / sqrt(g) ) ] }
!
!
!          =  [ (sWfp + sWfm) / 2 sqrt(g) - (sWhp + sWhm) sqrt(h) / 2
!
!          -  sg (Wfp + Wfm) / 4 sqrt(g)^3  + salpha hp (Whp + Whm) / 4 sqrt(h) ]


  if (shift == "harmonic") then
     sDsigma = half*((sWfp + sWfm)/dsqrt(g) - (sWhp + sWhm)*dsqrt(h) &
             - half*(sg*(Wfp + Wfm)/dsqrt(g)**3 + salpha*(Whp + Whm)*hp/dsqrt(h)))
  end if


! ***************
! ***   END   ***
! ***************

  end subroutine sources










  real(8) function slopecorrect(slope1,slope2,method)

! This function applies the different slope limiter methods.

  implicit none

  real(8) slope1,slope2

  character(*) method


! **************************
! ***   SLOPE LIMITERS   ***
! **************************

! Initialize slopecorrect to zero.

  slopecorrect = 0.0D0

! If both slopes have the same sign, find correction.

  if (slope1*slope2 > 0.0D0) then

!    minmod.

     if (method=="minmod") then

        if (dabs(slope1) < dabs(slope2)) then
           slopecorrect = slope1
        else
           slopecorrect = slope2
        end if

!    vanleer.

     else

        slopecorrect = 2.0D0*slope1*slope2/(slope1 + slope2)

     end if

  end if


! ***************
! ***   END   ***
! ***************

  end function slopecorrect

