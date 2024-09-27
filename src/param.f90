!$Header: /usr/local/ollincvs/Codes/OllinToy/src/param.f90,v 1.28 2005/06/02 18:39:09 malcubi Exp $

! ***************************************
! ***  PARAMETERS ARE DECLARED HERE   ***
! ***************************************

  module param

!               VERY IMPORTANT  (PLEASE READ)
!
! Declare only one parameter per line.  Even if FORTRAN allows
! to declare several parameters separated by commas, this file
! will be processed by a perl script that does NOT understand
! that (at least not yet).
!
! Use always the format:
!
!    type :: name = value
!
! The following variations are permitted:  Character type parameters
! that are allowed to receive multiple values at the same time
! (separated by commas) should be declared as:
!
!    character :: name = value     ! multiple
!
! Also, a range can be defined for character type parameters as:
!
!    character :: name = value     ! range = (value1,value2,...,valuen)
!
! The range is not compulsory.  If it is not there, any value
! is allowed.
!
! REMEMBER:  All parameters must be initialized.  The initial
! value should basically correspond to the code NOT doing
! anything.  That is, initialize to Minkowski, geodesic
! slicing, no shift, and all special features turned off.


! ****************
! ***   GRID   ***
! ****************

! dx:           Spatial interval.
! Nx:           Total number of grid points.

  real(8) :: dx = 1.0D0

  integer :: Nx = 10


! *************************
! ***   TIME STEPPING   ***
! *************************

! dtfac:        Courant parameter (dtfac=dt/dx).
! Nt:           Total number of time steps.

  real(8) :: dtfac = 0.5D0

  integer :: Nt = 10


! ******************
! ***   OUTPUT   ***
! ******************

! directory:    Name of directory for output.
!
! An important point to remember is that, because of the
! way Fortran works, this string has to be defined with a
! fixed length.  This means that before using it to identify
! a directory, it must first be trimmed to its true size.

  character(30) :: directory = "output"

! Output parameters.
! 
! Ninfo:        How often do we output information to screen?
! Noutput0D:    How often do we do 0D output?
! Noutput1D:    How often do we do 1D output?
! outvars0D:    Variables that need 0D output (a list separated by commas).
! outvars1D:    Variables that need 1D output (a list separated by commas).
! commenttype:  Type of comment lines on files (xgraph,gnuplot)
! outtype:      Type of output (standard,test).

  integer :: Ninfo = 1
  integer :: Noutput0D = 1
  integer :: Noutput1D = 1

  character(1000) :: outvars0D = "alpha"     ! multiple
  character(1000) :: outvars1D = "alpha"     ! multiple
  character(1000) :: commenttype = "xgraph"  ! range = (xgraph,gnuplot)
  character(1000) :: outtype   = "standard"  ! range = (standard,test)


! *******************
! ***   SLICING   ***
! *******************

! slicing:      Type of slicing condition.
! gauge_f:      Coefficient for Bona-Masso type slicing condition (positive).

  real(8) :: gauge_f = 1.0D0

  character(30) :: slicing = "harmonic"  ! range = (harmonic,1+log,shockavoid,shock0,shock1)


! *****************
! ***   SHIFT   ***
! *****************

! shift:      Type of shift condition.
! beta0:      Initial value of shift.
! gauge_h:    Coefficient for harmonic type shift (Ollin shift).

  real(8) :: beta0 = 0.0D0
  real(8) :: gauge_h = 1.0D0

  character(30) :: shift = "none"  ! range = (none,zero,constant,static,harmonic,1+sigma2)


! ************************
! ***   INITIAL DATA   ***
! ************************

! initialdata:  Type of initial data.
!
! gauss_a0:     Amplitude of gaussian perturbation.
! gauss_x0:     Center of gaussian perturbation.
! gauss_s0:     Width of gaussian perturbation.

  real(8) :: lapsegauss_a0 = 0.0D0
  real(8) :: lapsegauss_x0 = 0.0D0
  real(8) :: lapsegauss_s0 = 1.0D0

  real(8) :: shiftgauss_a0 = 0.0D0
  real(8) :: shiftgauss_x0 = 0.0D0
  real(8) :: shiftgauss_s0 = 1.0D0

  real(8) :: metricgauss_a0 = 0.0D0
  real(8) :: metricgauss_x0 = 0.0D0
  real(8) :: metricgauss_s0 = 1.0D0

  real(8) :: pulsegauss_a0 = 0.0D0
  real(8) :: pulsegauss_x0 = 0.0D0
  real(8) :: pulsegauss_s0 = 1.0D0

  character(30) :: initialdata = "lapsegauss"  ! multiple, range = (lapsegauss,shiftgauss,metricgauss,slicegauss,pureright,pureleft)


! *********************
! ***   EVOLUTION   ***
! *********************

! integrator:   Time integration method.
! method:       Spatial differencing method.

  character(30) :: integrator = "icn"      ! range = (icn,rk4)
  character(30) :: method = "center"       ! range = (center,upwind,minmod,vanleer)


! **********************
! ***   BOUNDARIES   ***
! **********************

! boundary:     Type of boundary condition.

  character(30) :: boundtype = "outgoing"  ! range = (flat,periodic,outgoing,reflectplus,reflectminus)


! *********************
! ***   OBSERVERS   ***
! *********************

! trackobs:     Do we want to track observers?

  logical :: trackobs = .false.


! ***************************
! ***   CHARACTERISTICS   ***
! ***************************

! trackchar:    Do we want to track characteristics?

  logical :: trackchar = .false.


! ***************
! ***   END   ***
! ***************

  end module param

