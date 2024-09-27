!$Header: /usr/local/ollincvs/Codes/OllinToy/src/evolve.f90,v 1.21 2024/09/06 20:52:33 malcubi Exp $

  subroutine evolve

! *******************************
! ***   NUMERICAL EVOLUTION   ***
! *******************************

! This subroutine controls the numerical iterations.

! Include modules.

  use param
  use arrays
  use arrayflags

! Extra variables.

  implicit none

  integer i,k,l,niter

  logical icn,rk4
  logical contains

  real(8) dt,t,dtw
  real(8) weight,max,aux
  real(8) zero,sixth,third,half,one,smallpi

! The new variables introduced above are:
!
! icn           Flag for Iterative Crank-Nicholson (ICN).
! rk4           Flag for fourth order Runge-Kutta.
!
! i,k,l         Counters.
!
! t:            Time.
! dt:           Time step.
! dtw:          Weighted time step.


! *******************
! ***   NUMBERS   ***
! *******************

  zero  = 0.0D0
  sixth = 1.0D0/6.0D0
  third = 1.0D0/3.0D0
  half  = 0.5D0
  one   = 1.0D0

  smallpi = acos(-one)


! *********************
! ***   TIME STEP   ***
! *********************

  dt = dx*dtfac


! ******************************
! ***   INTEGRATION METHOD   ***
! ******************************

  rk4 = .false.
  icn = .false.

! ICN.

  if (integrator=="icn") then

     icn = .true.

! Fourth  order Runge-Kutta.

  else if (integrator=="rk4") then

     rk4 = .true.

  end if


! ***************************
! ***   INITIALIZE TIME   ***
! ***************************

  t = zero


! *************************************
! ***   FIND GRID POINT POSITIONS   ***
! *************************************

! Here I fill in the array x(i) with the positions of
! the grid points.  Notice that I take half the grid
! points to be on the negative side of the origin.
! Also notice that if Nx is even, we will have a
! point on the origin, while if it is odd, we stagger
! the origin.

  do i=0,Nx
     x(i) = dble(i)*dx - half*dble(Nx)*dx
!    x(i) = dble(i)*dx - x0
  end do


! *****************************
! ***   FIND INITIAL DATA   ***
! *****************************

  call initial

! Find gauge functions.

  call gauge

! Find eigenfields.

  call eigenfields

! Calculate constraints.

  call constraints


! ***********************************************
! ***   PREDICT THE TIME OF SHOCK FORMATION   ***
! ***********************************************

! We can only predict the time of shock formation if the initial data
! is such that only one of the travelling waves is present, and if
! the gauge function f(alpha) is a constant.
!
! If this is the case then one can in fact show that a shock is
! expected at a time:
!
!      T  =  2 / [ (1-f) max{Omega} ] 
!
! where:
!
!      Omega  =  (alpha/sqrt(g)) Wf
!
! with  Wf  the corresponding traveling mode.  This proof is based
! on the fact that when one finds the evolution equation for "Omega",
! it turns out that the coefficient of the quadratic term is simply
! (1-f)/2, which is constant, so the equation can be integrated
! exactly.

  if ((slicing=="harmonic").and. &
     ((initialdata=="pureright").or.(initialdata=="pureleft"))) then

!    No shocks for harmonic slicing.

     if (gauge_f==one) then

        print * 
        print *,'No shocks for harmonic slicing.'
        print *  

     else

        max = zero
        aux = half*(one - gauge_f)

!       Pulse traveling to the right.

        if (initialdata=="pureright") then

           do i=0,Nx
              max = dmax1(max,aux*alpha(i)/dsqrt(g(i))*Wfp(i))
           end do

!       Pulse traveling to the left.

        else

           do i=0,Nx
              max = dmax1(max,aux*alpha(i)/dsqrt(g(i))*Wfm(i))
           end do

        end if

!       Output to screen.

        print *
        write(*,"(A23,F7.2)") ' Shock expected at time:',one/max
        print *

     end if
  end if


! ****************************
! ***   OUTPUT TO SCREEN   ***
! ****************************

  print *,'------------------------------'
  print *,'|  Time step  |     Time     |'
  print *,'------------------------------'

  write(*,"(A5,I7,A5,ES11.4,A4)") ' |   ',0,'   | ',t,'  | '


! *********************************
! ***   SAVE THE INITIAL DATA   ***
! *********************************

  call save0Ddata(0,t)
  call save1Ddata(0,t)


! *************************************
! ***   START MAIN EVOLUTION LOOP   ***
! *************************************

  do l=1,Nt

!    Time.

     t = t + dt

!    Save old time step.

     alpha_p = alpha
     g_p     = g

     Dalpha_p = Dalpha
     Dg_p     = Dg
     Ktilde_p = Ktilde

     if (shift /= "none") then
        beta_p = beta
     end if

     if (shift == "harmonic") then
        sigma_p  = sigma
        Dsigma_p = Dsigma
     end if

!    Save old characteristics.

     if (trackchar) then
        char0_p = char0
        charp_p = charp
        charm_p = charm
     end if

!    How many iterations to use?

     if (rk4) then

!       Fourth order Runge-Kutta needs 4 iterations.

        niter = 4

!       For RK4 we also need to initialize accumulator arrays.

        alpha_a  = alpha_p
        Dalpha_a = Dalpha_p

        g_a = g_p
        Dg_a = Dg_p

        Ktilde_a = Ktilde_p

     else if (icn) then

!       Standard ICN requires 3 iterations.

        niter = 3

     end if

!    Start iterations.

     do k=1,niter

!       Find out weights for each iteration for the
!       different time integration schemes.

        if (rk4) then

!          In fourth order Runge-Kutta the first two iterations
!          jump half a time step and the last two a full time step.
!          Here we also set the weights with which intermediate
!          results contribute to final answer: 1/6 for first and
!          last intermediate results and 1/3 for the two middle ones.

           if (k == 1) then
              dtw = half*dt
              weight = dt/6.0D0
           else if (k == 2) then
              dtw = half*dt
              weight = dt/3.0D0
           else if (k == 3) then
              dtw = dt
              weight = dt/3.0D0
           else
              dtw = dt
              weight = dt/6.0D0
           end if

        else if (icn) then

!          In normal ICN, all iterations except the last one
!          jump only half a time step.

           if (k<3) then
              dtw = half*dt
           else
              dtw = dt
           end if

        end if

!       Calculate sources.

        call sources

!       Advance variables.

        g      = g_p  + dtw*sg
        Dg     = Dg_p + dtw*sDg

        alpha  = alpha_p  + dtw*salpha
        Dalpha = Dalpha_p + dtw*sDalpha

        Ktilde = Ktilde_p + dtw*sKtilde

        if (shift == "harmonic") then
           sigma  = sigma_p  + dtw*ssigma
           Dsigma = Dsigma_p + dtw*sDsigma
           beta   = alpha*sigma
           Dbeta  = alpha*(sigma*Dalpha + Dsigma)
        end if

        if (trackchar) then
           char0 = char0_p + dtw*schar0
           charp = charp_p + dtw*scharp
           charm = charm_p + dtw*scharm
        end if

!       For fourth order Runge-Kutta, add to accumulator arrays
!       using the correct weights.  At the last step we make the
!       final variables equal to their accumulated values.

        if (rk4) then

           g_a  = g_a  + weight*sg
           Dg_a = Dg_a + weight*sDg

           alpha_a  = alpha_a  + weight*salpha
           Dalpha_a = Dalpha_a + weight*sDalpha

           Ktilde_a = Ktilde_a + weight*sKtilde

           if (shift == "harmonic") then
              sigma_a  = sigma_a  + weight*ssigma
              Dsigma_a = Dsigma_a + weight*sDsigma
           end if

           if (k == 4) then

              g  = g_a
              Dg = g_a

              alpha  = alpha_a
              Dalpha = Dalpha_a

              Ktilde = Ktilde_a

              if (shift == "harmonic") then
                 sigma  = sigma_a
                 Dsigma = Dsigma_a
                 beta   = alpha*sigma
                 Dbeta  = alpha*(sigma*Dalpha + Dsigma)
              end if

           end if

        end if

!       Find gauge functions.

        call gauge

!       Find eigenfields.

        call eigenfields

!       Boundary conditions.

        call boundary

!       Check for pathologies.

        do i=0,Nx

!          Negative lapse.

           if (alpha(i) < 0.0D0) then 
              print *, 'Negative lapse!'
              print *, 'i = ',i
              print *, 'alpha = ',alpha(i)
              print *
              stop
           end if

!          Negative metric.

           if (g(i) < 0.0D0) then 
              print *, 'Negative metric!'
              print *, 'i = ',i
              print *, 'g = ',g(i)
              print *
              stop
           end if

        end do

     end do

!    Analysis.

     if ((mod(l,Noutput0D).eq.0).or.(mod(l,Noutput1D).eq.0))  then

!       Trace of extrinsic curvature.

        if (flag_trK) then
           trK = Ktilde/dsqrt(g)
        end if

!       Constraints.

        call constraints

     end if

!    Track Minkowski coordinates of observers.

     if (trackobs) then

        xm_p = xm
        tm_p = tm

        call minkowski(t,dt)

     end if

!    Find inverse derivative of characteristics surfaces.

     do i=1,Nx
        Dchar0(i) = 2.0D0*dx/(char0(i+1) - char0(i-1))
        Dcharp(i) = 2.0D0*dx/(charp(i+1) - charp(i-1))
        Dcharm(i) = 2.0D0*dx/(charm(i+1) - charm(i-1))
     end do

     Dchar0(0)  = Dchar0(1)
     Dcharp(0)  = Dcharp(1)
     Dcharm(0)  = Dcharm(1)

     Dchar0(Nx) = Dchar0(Nx-1)
     Dcharp(Nx) = Dcharp(Nx-1)
     Dcharm(Nx) = Dcharm(Nx-1)


!    *****************************
!    ***   SAVE DATA TO FILE   ***
!    *****************************

!    Save 0D data (only every Noutput0D time steps).

     if (mod(l,Noutput0D).eq.0) then
        call save0Ddata(l,t)
     end if

!    Save 1D data (only every Noutput1D time steps).

     if (mod(l,Noutput1D).eq.0) then
        call save1Ddata(l,t)
     end if


!    ***********************************
!    ***   END MAIN EVOLUTION LOOP   ***
!    ***********************************

!    Time step information to screen.

     if (mod(l,Ninfo).eq.0) then
        write(*,"(A5,I7,A5,ES11.4,A4)") ' |   ',l,'   | ',t,'  | '
     end if

  end do

  print *,'------------------------------'


! ***************
! ***   END   ***
! ***************

  end subroutine evolve

