!$Header: /usr/local/ollincvs/Codes/OllinToy/src/minkowski.f90,v 1.7 2024/09/06 20:53:20 malcubi Exp $

  subroutine minkowski(t,dt)

! ***************************
! ***   TRACK OBSERVERS   ***
! ***************************

! This subroutine tracks the position of the coordinate observers
! in Minkowski coordinates.

! Include modules.

  use param
  use arrays

! Extra variables.

  implicit none

  integer i

  real(8) t,dt
  real(8) xguess,xright,xleft,tguess,tright,tleft
  real(8) aa,gg,bb
  real(8) dleft,dright
  real(8) half,two

! The variables introduced above are:
!
! i           Counter.
!
! dt          Time step.
!
! xguess      Initial guess for spatial coordinate of normal observer.
! xleft       Minkowski position of point to the left on previous time step.
! xright      Minkowski position of point to the right on previous time step.
!
! tguess      Initial guess for time coordinate of normal observer.
! tleft       Minkowski time of point to the left on previous time step.
! tright      Minkowski time of point to the right on previous time step.
!
! aa          Average lapse.
! gg          Average metric.
! bb          Average shift.
!
! dleft       Distance to left point.
! dright      Distance to right point.


! *******************
! ***   NUMBERS   ***
! *******************

  half = 0.5D0
  two  = 2.0D0


! ***********************************************
! ***   UPDATE POSITION OF COORDINATE LINES   ***
! ***********************************************

! To update the position of the coordinate lines I first calculate
! the interval of the point we are considering to the points to
! the left and right one time step below (forming a kind of triangle):
!
!                     ...   o  o  x  o  o  ...
!                     ...   o  x  o  x  o  ...
!
! The idea is then to find that point in Minkowski spacetime that is
! at precisely those distances from the previous points whose positions
! are known.  Notice that one would expect two possible solutions,
! one in the past and one on the future, but I lock onto the correct
! one by choosing a initial guess in the future.

! Interior points.

  do i=1,Nx-1

!    As initial guess I assume that the observer did not move in space
!    and advanced dt in time.
 
     xguess = xm(i)
     tguess = tm(i) + dt

!    Calculate interval to point on the left on previous time step.
!    First we average the lapse, shift and metric, and then we
!    find the interval.  Notice the sign of the mixed term, it is
!    positive since seen from the current point, the point in the
!    last time level is in the past (-dt) and to the left (-dx).
!    Also remember that beta is the contravariant shift, and 
!    the interval requires the covariant shift, so we need to
!    multiply with the metric.

     aa = half*(alpha(i) + alpha_p(i-1))
     gg = half*(g(i) + g_p(i-1))

     dleft = - (aa*dt)**2 + gg*dx**2

     if (shift /= "none") then
        bb = half*(beta(i) + beta_p(i-1))
        dleft = dleft + gg*(bb*dt)**2 + two*gg*bb*dx*dt
     end if

!    Calculate interval to point on the right on previous time step.
!    First we average the lapse, shift and metric, and then we
!    find the interval.  Notice the sign of the mixed term, it is
!    negative since seen from the current point, the point in the
!    last time level is in the past (-dt) and to the right (+dx).
!    Also remember that beta is the contravariant shift, and 
!    the interval requires the covariant shift, so we need to
!    multiply with the metric.

     aa = half*(alpha(i) + alpha_p(i+1))  
     gg = half*(g(i) + g_p(i+1))

     dright = - (aa*dt)**2 + gg*dx**2

     if (shift /= "none") then
        bb = half*(beta(i) + beta_p(i+1))
        dright = dright + gg*(bb*dt)**2 - two*gg*bb*dx*dt
     end if

!    Now solve for the position of the point that has precisely those
!    intervals in Minkowski coordinates.

     xleft  = xm_p(i-1)
     tleft  = tm_p(i-1)

     xright = xm_p(i+1)
     tright = tm_p(i+1)

     call solvepoint(xguess,tguess,xleft,tleft,xright,tright,dleft,dright)

!    Save new coordinates.

     xm(i) = xguess
     tm(i) = tguess

  end do

! Boundaries.

  xm(0)  = xm(1) - dx
  tm(0)  = tm(1)

  xm(Nx) = xm(Nx-1) + dx
  tm(Nx) = tm(Nx-1)


! ***************
! ***   END   ***
! ***************

  end subroutine minkowski










  subroutine solvepoint(x,t,x1,t1,x2,t2,d1,d2)

! ***************************
! ***   SOLVE FOR POINT   ***
! ***************************

! This subroutine solves for the position in Minkowski
! coordinates of a point (x,t) that has an interval d1
! with respect to the point (x1,t1) and an interval d2
! with respect to the point (x2,t2).
!
! The routine does that by using Newton-Raphson.  Notice
! that on entry (x,t) contains the initial guess.

  implicit none

  integer k,Nmax

  real(8) x,x1,x2
  real(8) t,t1,t2
  real(8) d1,d2
  real(8) det
  real(8) epsilon

  real(8) res(1:2),delta(1:2),jac(1:2,1:2)

! The variables introduced above are:
!
! k          Counter.
! Nmax       Maximum number of iterations.
!
! (x,t)      Solution (and initial guess).
!
! (x1,t1)    Coordinates of point 1.
! (x2,t2)    Coordinates of point 2. 
!
! d1         Distance to point 1.
! d2         Distance to point 2.
!
! res        Vector of residuals.
! jac        Jacobian matrix.
!
! det        Determinant of jacobian.
!
! delta      Vector of increments.


! **************************
! ***   NEWTON-RAPHSON   ***
! **************************

! Start iterations.

  Nmax = 1000

  do k=1,Nmax

!    Find residuals.

     res(1) = (x - x1)**2 - (t - t1)**2 - d1
     res(2) = (x - x2)**2 - (t - t2)**2 - d2

!    Find jacobian matrix and its determinant.

     jac(1,1) = + 2.0D0*(x - x1)
     jac(1,2) = - 2.0D0*(t - t1)

     jac(2,1) = + 2.0D0*(x - x2)
     jac(2,2) = - 2.0D0*(t - t2)

     det = jac(1,1)*jac(2,2) - jac(1,2)*jac(2,1)

!    Solve for increments (J delta = - res).

     delta(1) = (res(2)*jac(1,2) - res(1)*jac(2,2))/det
     delta(2) = (res(1)*jac(2,1) - res(2)*jac(1,1))/det

!    Correct solution.

     x = x + delta(1)
     t = t + delta(2)

!    Check if we achieved desired tolerance.

     epsilon = dabs(delta(1)) + dabs(delta(2))

     if (epsilon < 1.0D-10) return

  end do

  print *, "Too many iterations in Newton-Raphson: subroutine minkowski"
  print *


! ***************
! ***   END   ***
! ***************

  end subroutine solvepoint
