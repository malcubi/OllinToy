!$Header: /usr/local/ollincvs/Codes/OllinToy/src/initial.f90,v 1.19 2024/09/06 20:52:52 malcubi Exp $

  subroutine initial

! ************************
! ***   INITIAL DATA   ***
! ************************

! Include modules.

  use param
  use arrays
  use arrayflags

! Extra variables.

  implicit none

  logical flag,inlist,contains

  integer i

  real(8) zero,half,one,two
  real(8) aux

  real(8), dimension (0:Nx) :: gauss
  real(8), dimension (0:Nx) :: hh,hhp,hhpp
  real(8), dimension (0:Nx) :: temp


! *******************
! ***   NUMBERS   ***
! *******************

  zero = 0.0D0
  half = 0.5D0
  one  = 1.0D0
  two  = 2.0D0

  flag = .false.


! *********************
! ***   MINKOWSKI   ***
! *********************

! By default, initial data is always set to Minkowski first.
! This is in order to avoid the code crashing because the
! metric components are all zero.

! Lapse.

  alpha  = one
  Dalpha = zero

! Shift.

  if (shift /= "none") then

     beta  = zero
     Dbeta = zero

     if (shift == "constant") then
        beta  = beta0
        Dbeta = zero
     else if (shift == "harmonic") then
        sigma  = zero
        Dsigma = zero
     end if

  end if

! Metric.

  g  = one
  Dg = zero

! Extrinsic curvature.

  Ktilde = zero

  if (flag_trK) then
     trK = zero
  end if


! **************************
! ***   GAUSSIAN LAPSE   ***
! **************************

! In this case we start with a trivial slice in Minkowski
! coordinates but with a non-trivial initial lapse.

  if (inlist(initialdata,"lapsegauss")) then

     flag = .true.

     gauss  = lapsegauss_a0*dexp(-(x-lapsegauss_x0)**2/lapsegauss_s0**2)

!    Lapse and derivative.

     alpha  = alpha + gauss
     Dalpha = Dalpha - two*gauss*(x-lapsegauss_x0)/lapsegauss_s0**2/alpha

!    Minkowski coordinates.  In this case the initial slice
!    is trivial.

     if (trackobs) then
        tm = zero
        xm = x
     end if

  end if


! **************************
! ***   GAUSSIAN SHIFT   ***
! **************************

! In this case we start with a trivial slice in Minkowski
! coordinates but with a non-trivial initial shift.

  if (inlist(initialdata,"shiftgauss")) then

     flag = .true.

     gauss = shiftgauss_a0*dexp(-(x-shiftgauss_x0)**2/shiftgauss_s0**2)

!    Shift and derivative.

     if ((shift=="none").or.(shift=="zero").or.(shift=="constant")) then
        print *
        print *, 'Error in parfile:  initialdata=shiftgauss and shift=',shift
        print *, 'are incompatible.  Aborting ...'
        print *
        stop
     end if

     beta  = beta + gauss
     Dbeta = Dbeta - two*gauss*(x-shiftgauss_x0)/shiftgauss_s0**2

     if (shift == "harmonic") then
        sigma = beta
        Dsigma = Dbeta
     end if

!    Minkowski coordinates.  In this case the initial slice
!    is trivial.

     if (trackobs) then
        tm = zero
        xm = x
     end if

  end if


! ***************************
! ***   GAUSSIAN METRIC   ***
! ***************************

! In this case we start with a trivial slice in Minkowski
! coordinates but with a non-trivial spatial metric. The
! spatial metric is obtained from a simple transformation
! from Minkowski coordinates:
!
! x_M = x + h(x)
!
! The metric then takes the form:
!
!                2
! g = (1 + dh/dx)
!
! 
! For this initial data we only expect evolution caused
! by the shift.

  if (inlist(initialdata,"metricgauss")) then

     if (flag) then
        print *
        print *, 'This combination of initial data is not allowed.'
        print *, 'Aborting (subroutine initial.f90)'
        print *
        stop
     end if

     flag = .true.

     hh   = metricgauss_a0*dexp(-(x-metricgauss_x0)**2/metricgauss_s0**2)
     hhp  = - two*hh*(x-metricgauss_x0)/metricgauss_s0**2
     hhpp = - two*hh/metricgauss_s0**2*(one - two*(x-metricgauss_x0)**2/metricgauss_s0**2)

!    Sanity check.

     do i=0,Nx
        if (hhp(i) < -one) then
           print *
           print *, 'The initial metric is singular.'
           print *, 'i = ',i
           print *, 'dh/dx = ',hhp(i)
           print *
           stop
        end if
     end do

!    Metric and derivative.

     g  = (one + hhp)**2
     Dg = two*hhpp/(one + hhp)

!    Minkowski coordinates.  In this case the initial slice
!    is trivial, but the initial spatial coordinates are not.

     if (trackobs) then
        tm = zero
        xm = x + hh
     end if

  end if


! ***************************************************************
! ***   PURE TRAVELING PULSES AND NON-TRIVIAL INITIAL SLICE   ***
! ***************************************************************

! In this case we take  a non-trivial initial slice.  We can
! have either pure traveling waves, or a trivial initial lapse.
! Notice that the pure traveling waves can only be used with
! f(alpha)= constant.
!
! The idea here is to take a non-trivial initial slice with profile
! in Minkowski coordinates given by a function t=h(x).  In this case
! one can show that the initial metric in terms of the original
! Minkowski coordinates is:
!
!                       2
!           g  =  1 - h'      =>   Dg  =  - 2 h' h'' / g 
!
! And the initial extrinsic curvature is:
!                                                          3/2
!           K_xx  = - h'' / sqrt(g)   =>  trK  =  - h'' / g
!
! which implies:
!
!           Ktilde  =  - h'' / g
!
! So far, this is independent of the slicing condition. But if we
! want either purely right going or left going pulses we must now
! ask for:
!
!           Dalpha  = + sqrt(f) Ktilde    (right going)
!           Dalpha  = - sqrt(f) Ktilde    (left going)
!
! These last expressions do depend on f(alpha), but we still don't
! know alpha, and hence we don't know f(alpha). In order to go further
! one needs to specify the form of f(alpha).  I have only looked at
! two cases:
!
! 1) f = constant. In that case one can integrate for alpha exactly to find:
!
!                             sqrt(f)/2
!    alpha  =  ((1-h')/(1+h'))            (right going)
!
!                             sqrt(f)/2
!    alpha  =  ((1+h')/(1-h'))            (left going)
!
!
! 2) f = 1 + k/alpha**2 (shock avoiding slicing).  In that case one can also
!    integrate for alpha exactly to find:
!
!                                 1/2                       1/2
!    alpha  =  ( c ((1-h')/(1+h'))    -  k/c ((1-h')/(1+h'))    )  (right going)
!
!
!                                 1/2                       1/2
!    alpha  =  ( c ((1+h')/(1-h'))    -  k/c ((1+h')/(1-h'))    )  (left going)
!
!
!    where c is an arbitrary integration constant. If we want to have alpha=1
!    far away then we must choose:  c = 1 + sqrt(1+k).


  if ((inlist(initialdata,"slicegauss")).or.(inlist(initialdata,"pureright")) &
  .or.(inlist(initialdata,"pureleft"))) then

     if (flag.or.((inlist(initialdata,"pureright")).and.(inlist(initialdata,"pureleft"))) &
        .or.((contains(initialdata,"pure")).and.(inlist(initialdata,"slicepulse")))) then
        print *
        print *, 'This combination of initial data is not allowed.'
        print *, 'Aborting (subroutine initial.f90)'
        print *
        stop
     end if

     flag = .true.

!    Function h and its first and second derivatives.

     hh   = pulsegauss_a0*dexp(-(x-pulsegauss_x0)**2/pulsegauss_s0**2)
     hhp  = - two*hh*(x-pulsegauss_x0)/pulsegauss_s0**2
     hhpp = - two*hh/pulsegauss_s0**2*(one - two*(x-pulsegauss_x0)**2/pulsegauss_s0**2)

     do i=0,Nx
        if (abs(hhp(i)) > one) then
           print *, 'Initial slice not spacelike!'
           print *, 'i = ',i
           print *, 'dh/dx = ',hhp(i)
           print *
           stop
        end if
     end do

!    Spatial metric:  g = 1 - hp**2

     g = one - hhp**2

!    Logarithmic derivative of metric:  Dg = - 2 hp hpp / g

     Dg = - two*hhp*hhpp/g

!    Extrinsic curvature:  Ktilde = - hpp/g

     Ktilde = - hhpp/g

!    Calculate lapse and its derivative for pure traveling pulses.

     if (initialdata=="pureright") then
 
        temp = dsqrt((one - hhp)/(one + hhp))

        if (slicing=="harmonic") then

!          alpha.

           alpha = temp**dsqrt(gauge_f)

!          f(alpha).

           f = gauge_f

!          Dalpha.

           Dalpha = + dsqrt(f)*Ktilde

        else if (slicing=="shockavoid") then

           aux = one + dsqrt(one + gauge_f)

           temp = aux*temp

!          alpha.

           alpha = half*(temp - gauge_f/temp)

!          f(alpha).

           f = one + gauge_f/alpha**2

!          Dalpha.

           Dalpha = + dsqrt(f)*Ktilde

        else

           print *, 'For pure traveling waves we must have'
           print *, 'slicing=harmonic or slicing=shockavoid'
           print *
           stop

        end if

     else if (initialdata=="pureleft") then

        temp = dsqrt((one + hhp)/(one - hhp))

        if (slicing=="harmonic") then

!          alpha.

           alpha = temp**dsqrt(gauge_f)

!          f(alpha).

           f = gauge_f

!          Dalpha.

           Dalpha = - dsqrt(f)*Ktilde

        else if (slicing=="shockavoid") then

           aux = one + dsqrt(one + gauge_f)

           temp = aux*temp

!          alpha.

           alpha = half*(temp - gauge_f/temp)

!          f(alpha).

           f = one + gauge_f/alpha**2

!          Dalpha.

           Dalpha = - dsqrt(f)*Ktilde

        else

           print *, 'For pure traveling waves we must have'
           print *, 'slicing=harmonic or slicing=shockavoid'
           print *
           stop

        end if

     end if

!    Minkowski coordinates.  In this case the initial
!    slice has a profile given by the function h.

     if (trackobs) then
        tm = hh
        xm = x
     end if

  end if


! **************************************
! ***   INITIALIZE CHARACTERISTICS   ***
! **************************************

! Initially the characteristic surfaces are taken to be
! equal to the position of the coordinates.

  if (trackchar) then

     charp = x
     charm = x

     Dcharp = one
     Dcharm = one

  end if


! ***************
! ***   END   ***
! ***************

  end subroutine initial
