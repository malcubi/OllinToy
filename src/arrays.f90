!$Header: /usr/local/ollincvs/Codes/OllinToy/src/arrays.f90,v 1.22 2005/04/27 22:13:14 malcubi Exp $

  module arrays

! ****************************
! ***  MODULE FOR ARRAYS   ***
! ****************************

!               VERY IMPORTANT  (PLEASE READ)
!
! Declare only one array per line.  Even if FORTRAN allows
! to declare several arrays separated by comas, this file
! will be processed by a perl script that expects only
! one array declared  per line.
!
! The expected syntax for the perl file is:
!
! real(8), allocatable, dimension (:) ::  varname
!
! One can also add control information at the end of a declaration as
! a comment.  For example, an array can be declared for "analysis"
! in the following form:
!
! real(8), allocatable, dimension (:) ::  varname   ! analysis
!
! This indicates that the array "varname" is only needed for output,
! so memory will only be allocated if we want output of this array.
!
! In the same way, one can add a conditional statement that will
! control when memory is allocated.  For example:
!
! real(8), allocatable, dimension (:) ::  varname   ! if (expression)
!
! where "expression" is a correctly formulated Fortran logical expression
! involving the value of some parameter the code knows about.

  implicit none


! ***********************
! ***   COORDINATES   ***
! ***********************

! Spatial coordinate.

  real(8), allocatable, dimension (:) :: x

! Minkowski coordinates of observers.

  real(8), allocatable, dimension (:) :: tm
  real(8), allocatable, dimension (:) :: tm_p

  real(8), allocatable, dimension (:) :: xm
  real(8), allocatable, dimension (:) :: xm_p


! *******************
! ***   SLICING   ***
! *******************

! *      Variable at present time level.
! *_p    Variable at past time level.
! s*     Source term for variable.
! *_a    Runge-Kutta auxiliary array for variable.

! Lapse.

  real(8), allocatable, dimension (:) :: alpha
  real(8), allocatable, dimension (:) :: alpha_p
  real(8), allocatable, dimension (:) :: salpha
  real(8), allocatable, dimension (:) :: alpha_a

! Logarithmic derivative of lapse:  Dalpha = d(ln alpha)/dx.

  real(8), allocatable, dimension (:) :: Dalpha
  real(8), allocatable, dimension (:) :: Dalpha_p
  real(8), allocatable, dimension (:) :: sDalpha
  real(8), allocatable, dimension (:) :: Dalpha_a

! Bona-Masso gauge function f and its derivative fp = df/dalpha.

  real(8), allocatable, dimension (:) :: f
  real(8), allocatable, dimension (:) :: fp


! *****************
! ***   SHIFT   ***
! *****************

! Shift.

  real(8), allocatable, dimension (:) :: beta     ! if (shift /= "none") 
  real(8), allocatable, dimension (:) :: beta_p   ! if (shift /= "none")

! Derivative of shift: Dbeta = d beta / dx.

  real(8), allocatable, dimension (:) :: Dbeta    ! if (shift /= "none") 

! Rescaled shift:  beta = alpha*sigma.

  real(8), allocatable, dimension (:) :: sigma    ! if (shift == "harmonic")
  real(8), allocatable, dimension (:) :: sigma_p  ! if (shift == "harmonic")
  real(8), allocatable, dimension (:) :: ssigma   ! if (shift == "harmonic")
  real(8), allocatable, dimension (:) :: sigma_a  ! if (shift == "harmonic")

! Derivative of rescaled shift: Dsigma = d sigma / dx.

  real(8), allocatable, dimension (:) :: Dsigma   ! if (shift == "harmonic")
  real(8), allocatable, dimension (:) :: Dsigma_p ! if (shift == "harmonic")
  real(8), allocatable, dimension (:) :: sDsigma  ! if (shift == "harmonic")
  real(8), allocatable, dimension (:) :: Dsigma_a ! if (shift == "harmonic")

! Gauge function h and its derivative hp = dh / dalpha.

  real(8), allocatable, dimension (:) :: h        ! if (shift == "harmonic")
  real(8), allocatable, dimension (:) :: hp       ! if (shift == "harmonic")


! ******************
! ***   METRIC   ***
! ******************

! *      Variable at present time level.
! *_p    Variable at past time level.
! s*     Source term for variable.
! *_a    Runge-Kutta auxiliary array for variable.

! Spatial metric.

  real(8), allocatable, dimension (:) :: g
  real(8), allocatable, dimension (:) :: g_p
  real(8), allocatable, dimension (:) :: sg
  real(8), allocatable, dimension (:) :: g_a

! Logarithmic derivative of metric:  Dg = d(ln g)/dx.

  real(8), allocatable, dimension (:) :: Dg
  real(8), allocatable, dimension (:) :: Dg_p
  real(8), allocatable, dimension (:) :: sDg
  real(8), allocatable, dimension (:) :: Dg_a


! *******************************
! ***   EXTRINSIC CURVATURE   ***
! *******************************

! *      Variable at present time level.
! *_p    Variable at past time level.
! s*     Source term for variable.
! *_a    Runge-Kutta auxiliary array for variable.

! Trace of K.

  real(8), allocatable, dimension (:) :: trK        ! analysis

! Densitized K:  Ktilde := sqrt(g) trK.

  real(8), allocatable, dimension (:) :: Ktilde
  real(8), allocatable, dimension (:) :: Ktilde_p
  real(8), allocatable, dimension (:) :: sKtilde
  real(8), allocatable, dimension (:) :: Ktilde_a


! ***********************
! ***   EIGENFIELDS   ***
! ***********************

! Eigenspeeds:
!
! lambda0  = - beta
! lambdafp = - beta + alpha sqrt(f/g)
! lambdafm = - beta - alpha sqrt(f/g)
! lambdahp = - beta + alpha sqrt(h/g)
! lambdahm = - beta - alpha sqrt(h/g)

  real(8), allocatable, dimension (:) :: lambda0
  real(8), allocatable, dimension (:) :: lambdafp
  real(8), allocatable, dimension (:) :: lambdafm
  real(8), allocatable, dimension (:) :: lambdahp   ! if (shift == "harmonic")
  real(8), allocatable, dimension (:) :: lambdahm   ! if (shift == "harmonic")

! Eigenfield with zero speed:
!
! W0 = Dalpha / f - Dg / 2

  real(8), allocatable, dimension (:) :: W0
  real(8), allocatable, dimension (:) :: sW0

! Eigenfields associated with slicing (Dalpha,Ktilde):
!
! Wfp = Ktilde + Dalpha / sqrt(f)
! Wfm = Ktilde - Dalpha / sqrt(f)

  real(8), allocatable, dimension (:) :: Wfp
  real(8), allocatable, dimension (:) :: sWfp

  real(8), allocatable, dimension (:) :: Wfm
  real(8), allocatable, dimension (:) :: sWfm

! Shift eigenfields:

  real(8), allocatable, dimension (:) :: Whp        ! if (shift == "harmonic")
  real(8), allocatable, dimension (:) :: sWhp       ! if (shift == "harmonic")

  real(8), allocatable, dimension (:) :: Whm        ! if (shift == "harmonic")
  real(8), allocatable, dimension (:) :: sWhm       ! if (shift == "harmonic")


! ***************************
! ***   CHARACTERISTICS   ***
! ***************************

! char0     Characteristic surface propagating along the time lines.
! charp     Characteristic surface propagating to the right.
! charm     Characteristic surface propagating to the left.

  real(8), allocatable, dimension (:) :: char0
  real(8), allocatable, dimension (:) :: char0_p
  real(8), allocatable, dimension (:) :: schar0

  real(8), allocatable, dimension (:) :: charp
  real(8), allocatable, dimension (:) :: charp_p
  real(8), allocatable, dimension (:) :: scharp

  real(8), allocatable, dimension (:) :: charm
  real(8), allocatable, dimension (:) :: charm_p
  real(8), allocatable, dimension (:) :: scharm

! Inverse derivatives of characteristics surfaces.

  real(8), allocatable, dimension (:) :: Dchar0
  real(8), allocatable, dimension (:) :: Dcharp
  real(8), allocatable, dimension (:) :: Dcharm


! ***********************
! ***   CONSTRAINTS   ***
! ***********************

! Calpha:    Dalpha - d ln(alpha) / dx
! Cg:        Dg - d ln(g) / dx
! Csigma:    Dsigma - d sigma / dx

  real(8), allocatable, dimension (:) :: Calpha     ! analysis
  real(8), allocatable, dimension (:) :: Cg         ! analysis
  real(8), allocatable, dimension (:) :: Csigma     ! analysis


! ***************
! ***   END   ***
! ***************

  end module arrays
