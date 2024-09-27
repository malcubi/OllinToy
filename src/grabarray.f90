! Automatically generated file.  Do not edit!

  subroutine grabarray(varname,var,iscomplex)

  use param
  use arrays

  implicit none
  logical exists,iscomplex
  complex(8), dimension (0:Nx) :: var
  character(len=*) varname

  exists = .false.

  if (varname=='x') then
     exists = .true.
     iscomplex = .false.
     var = x
  end if

  if (varname=='tm') then
     exists = .true.
     iscomplex = .false.
     var = tm
  end if

  if (varname=='tm_p') then
     exists = .true.
     iscomplex = .false.
     var = tm_p
  end if

  if (varname=='xm') then
     exists = .true.
     iscomplex = .false.
     var = xm
  end if

  if (varname=='xm_p') then
     exists = .true.
     iscomplex = .false.
     var = xm_p
  end if

  if (varname=='alpha') then
     exists = .true.
     iscomplex = .false.
     var = alpha
  end if

  if (varname=='alpha_p') then
     exists = .true.
     iscomplex = .false.
     var = alpha_p
  end if

  if (varname=='salpha') then
     exists = .true.
     iscomplex = .false.
     var = salpha
  end if

  if (varname=='alpha_a') then
     exists = .true.
     iscomplex = .false.
     var = alpha_a
  end if

  if (varname=='Dalpha') then
     exists = .true.
     iscomplex = .false.
     var = Dalpha
  end if

  if (varname=='Dalpha_p') then
     exists = .true.
     iscomplex = .false.
     var = Dalpha_p
  end if

  if (varname=='sDalpha') then
     exists = .true.
     iscomplex = .false.
     var = sDalpha
  end if

  if (varname=='Dalpha_a') then
     exists = .true.
     iscomplex = .false.
     var = Dalpha_a
  end if

  if (varname=='f') then
     exists = .true.
     iscomplex = .false.
     var = f
  end if

  if (varname=='fp') then
     exists = .true.
     iscomplex = .false.
     var = fp
  end if

  if (varname=='beta') then
     exists = .true.
     iscomplex = .false.
     var = beta
  end if

  if (varname=='beta_p') then
     exists = .true.
     iscomplex = .false.
     var = beta_p
  end if

  if (varname=='Dbeta') then
     exists = .true.
     iscomplex = .false.
     var = Dbeta
  end if

  if (varname=='sigma') then
     exists = .true.
     iscomplex = .false.
     var = sigma
  end if

  if (varname=='sigma_p') then
     exists = .true.
     iscomplex = .false.
     var = sigma_p
  end if

  if (varname=='ssigma') then
     exists = .true.
     iscomplex = .false.
     var = ssigma
  end if

  if (varname=='sigma_a') then
     exists = .true.
     iscomplex = .false.
     var = sigma_a
  end if

  if (varname=='Dsigma') then
     exists = .true.
     iscomplex = .false.
     var = Dsigma
  end if

  if (varname=='Dsigma_p') then
     exists = .true.
     iscomplex = .false.
     var = Dsigma_p
  end if

  if (varname=='sDsigma') then
     exists = .true.
     iscomplex = .false.
     var = sDsigma
  end if

  if (varname=='Dsigma_a') then
     exists = .true.
     iscomplex = .false.
     var = Dsigma_a
  end if

  if (varname=='h') then
     exists = .true.
     iscomplex = .false.
     var = h
  end if

  if (varname=='hp') then
     exists = .true.
     iscomplex = .false.
     var = hp
  end if

  if (varname=='g') then
     exists = .true.
     iscomplex = .false.
     var = g
  end if

  if (varname=='g_p') then
     exists = .true.
     iscomplex = .false.
     var = g_p
  end if

  if (varname=='sg') then
     exists = .true.
     iscomplex = .false.
     var = sg
  end if

  if (varname=='g_a') then
     exists = .true.
     iscomplex = .false.
     var = g_a
  end if

  if (varname=='Dg') then
     exists = .true.
     iscomplex = .false.
     var = Dg
  end if

  if (varname=='Dg_p') then
     exists = .true.
     iscomplex = .false.
     var = Dg_p
  end if

  if (varname=='sDg') then
     exists = .true.
     iscomplex = .false.
     var = sDg
  end if

  if (varname=='Dg_a') then
     exists = .true.
     iscomplex = .false.
     var = Dg_a
  end if

  if (varname=='trK') then
     exists = .true.
     iscomplex = .false.
     var = trK
  end if

  if (varname=='Ktilde') then
     exists = .true.
     iscomplex = .false.
     var = Ktilde
  end if

  if (varname=='Ktilde_p') then
     exists = .true.
     iscomplex = .false.
     var = Ktilde_p
  end if

  if (varname=='sKtilde') then
     exists = .true.
     iscomplex = .false.
     var = sKtilde
  end if

  if (varname=='Ktilde_a') then
     exists = .true.
     iscomplex = .false.
     var = Ktilde_a
  end if

  if (varname=='lambda0') then
     exists = .true.
     iscomplex = .false.
     var = lambda0
  end if

  if (varname=='lambdafp') then
     exists = .true.
     iscomplex = .false.
     var = lambdafp
  end if

  if (varname=='lambdafm') then
     exists = .true.
     iscomplex = .false.
     var = lambdafm
  end if

  if (varname=='lambdahp') then
     exists = .true.
     iscomplex = .false.
     var = lambdahp
  end if

  if (varname=='lambdahm') then
     exists = .true.
     iscomplex = .false.
     var = lambdahm
  end if

  if (varname=='W0') then
     exists = .true.
     iscomplex = .false.
     var = W0
  end if

  if (varname=='sW0') then
     exists = .true.
     iscomplex = .false.
     var = sW0
  end if

  if (varname=='Wfp') then
     exists = .true.
     iscomplex = .false.
     var = Wfp
  end if

  if (varname=='sWfp') then
     exists = .true.
     iscomplex = .false.
     var = sWfp
  end if

  if (varname=='Wfm') then
     exists = .true.
     iscomplex = .false.
     var = Wfm
  end if

  if (varname=='sWfm') then
     exists = .true.
     iscomplex = .false.
     var = sWfm
  end if

  if (varname=='Whp') then
     exists = .true.
     iscomplex = .false.
     var = Whp
  end if

  if (varname=='sWhp') then
     exists = .true.
     iscomplex = .false.
     var = sWhp
  end if

  if (varname=='Whm') then
     exists = .true.
     iscomplex = .false.
     var = Whm
  end if

  if (varname=='sWhm') then
     exists = .true.
     iscomplex = .false.
     var = sWhm
  end if

  if (varname=='char0') then
     exists = .true.
     iscomplex = .false.
     var = char0
  end if

  if (varname=='char0_p') then
     exists = .true.
     iscomplex = .false.
     var = char0_p
  end if

  if (varname=='schar0') then
     exists = .true.
     iscomplex = .false.
     var = schar0
  end if

  if (varname=='charp') then
     exists = .true.
     iscomplex = .false.
     var = charp
  end if

  if (varname=='charp_p') then
     exists = .true.
     iscomplex = .false.
     var = charp_p
  end if

  if (varname=='scharp') then
     exists = .true.
     iscomplex = .false.
     var = scharp
  end if

  if (varname=='charm') then
     exists = .true.
     iscomplex = .false.
     var = charm
  end if

  if (varname=='charm_p') then
     exists = .true.
     iscomplex = .false.
     var = charm_p
  end if

  if (varname=='scharm') then
     exists = .true.
     iscomplex = .false.
     var = scharm
  end if

  if (varname=='Dchar0') then
     exists = .true.
     iscomplex = .false.
     var = Dchar0
  end if

  if (varname=='Dcharp') then
     exists = .true.
     iscomplex = .false.
     var = Dcharp
  end if

  if (varname=='Dcharm') then
     exists = .true.
     iscomplex = .false.
     var = Dcharm
  end if

  if (varname=='Calpha') then
     exists = .true.
     iscomplex = .false.
     var = Calpha
  end if

  if (varname=='Cg') then
     exists = .true.
     iscomplex = .false.
     var = Cg
  end if

  if (varname=='Csigma') then
     exists = .true.
     iscomplex = .false.
     var = Csigma
  end if

  if (.not.exists) then
     print *
     print *, 'Error in parfile, non-existent array: ',varname
     print *
     print *, 'Aborting! (subroutine grabarray.f90)'
     print *
     stop
  end if

  end subroutine grabarray
