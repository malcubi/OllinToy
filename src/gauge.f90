!$Header: /usr/local/ollincvs/Codes/OllinToy/src/gauge.f90,v 1.3 2005/04/27 23:14:09 malcubi Exp $

  subroutine gauge

! **************************************************
! ***   GAUGE FUNCTIONS  f(alpha) and h(alpha)   ***
! **************************************************

! This subroutine calculates the gauge functions f(alpha) and h(alpha)
! and their derivatives fp=d(f)/dalpha and fh=d(h)/dalpha.

! Include modules.

  use param
  use arrays

! Extra variables.

  implicit none

  real(8) zero,one,two,three,four,aux


! *******************
! ***   NUMBERS   ***
! *******************

  zero  = 0.0D0
  one   = 1.0D0
  two   = 2.0D0
  three = 3.0D0
  four  = 4.0D0


! ***************************
! ***   LAPSE  f(alpha)   ***
! ***************************

! Here we calculate the gauge function  f(alpha) and its derivative.

  if (slicing=="harmonic") then

!    **************************
!    ***   HARMONIC FAMILY  ***
!    **************************

!    In this case the gauge function is:
!
!    f  =  gauge_f
!
!    True harmonic slicing corresponds to the case when
!    the constant gauge_f is equal to 1.

     f  = gauge_f
     fp = zero

  else if (slicing=="1+log") then

!    ************************
!    ***   1+LOG FAMILY   ***
!    ************************

!    In this case the gauge function is:
!
!    f  =  gauge_f/alpha
!
!    Standard 1+log slicing corresponds to the case when
!    the constant gauge_f is equal to 2.

     f  = + gauge_f/alpha
     fp = - gauge_f/alpha**2

  else if (slicing=="shockavoid") then

!    **************************************
!    ***   FULLY SHOCK AVOIDING FAMILY  ***
!    **************************************

!    In this case the gauge function is:
!
!                         2
!    f = 1 + gauge_f/alpha
!
!    The whole family avoids shocks.
!    (See: Class.Quant.Grav. 20 (2003) 607-624; gr-qc/0210050)

     f  = one + gauge_f/alpha**2
     fp = - two*gauge_f/alpha**3

  else if (slicing=="shock0") then

!    *******************************************
!    ***   ZERO ORDER SHOCK AVOIDING FAMILY  ***
!    *******************************************

!    In this case the gauge function is:
!
!                 2
!    f  =  gauge_f  / [ (2 - gauge_f) + 2 alpha (gauge_f - 1) ]
!
!
!    The best choice for BH spacetimes is:  gauge_f = 2
!    (See: Class.Quant.Grav. 20 (2003) 607-624; gr-qc/0210050)

     f  = + gauge_f**2/((two - gauge_f) + two*alpha*(gauge_f - one))
     fp = - gauge_f**2/((two - gauge_f) + two*alpha*(gauge_f - one))**2 &
        *two*(gauge_f - one)

  else if (slicing=="shock1") then

!    ********************************************
!    ***   FIRST ORDER SHOCK AVOIDING FAMILY  ***
!    ********************************************

!    In this case the gauge function is:
!
!                 3
!    f  =  gauge_f  / { (4 - 3 gauge_f)
!
!       +  alpha (1 - gauge_f) [alpha (4-gauge_f) - 8] } 
!
!
!    The best choice for BH spacetimes is:  gauge_f = 4/3
!    (See: Class.Quant.Grav. 20 (2003) 607-624; gr-qc/0210050)

     f  = + gauge_f**3/((four - three*gauge_f) &
        + alpha*(one-gauge_f)*(alpha*(four-gauge_f) - 8.0D0))
     fp = - gauge_f**3/((four - three*gauge_f) &
        + alpha*(one-gauge_f)*(alpha*(four-gauge_f) - 8.0D0))**2 &
        * ((one-gauge_f)*(alpha*(four-gauge_f) - 8.0D0) &
        + alpha*(one-gauge_f)*(four-gauge_f))

  end if


! ***************************
! ***   SHIFT  h(alpha)   ***
! ***************************

  if (shift == "harmonic") then
     h  = gauge_h
     hp = zero
  end if


! ***************
! ***   END   ***
! ***************

  end subroutine gauge
