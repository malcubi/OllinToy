! Automatically generated file.  Do not edit!

  subroutine allocatearrays(status)

  use param
  use arrays
  use arrayflags

  implicit none
  logical contains,inlist
  character(len=*) status

  flag_x = .true.
  if (trim(status)=='on') then
     allocate(x(0:Nx))
     x = 0.0D0
  else
     deallocate(x)
  end if

  flag_tm = .true.
  if (trim(status)=='on') then
     allocate(tm(0:Nx))
     tm = 0.0D0
  else
     deallocate(tm)
  end if

  flag_tm_p = .true.
  if (trim(status)=='on') then
     allocate(tm_p(0:Nx))
     tm_p = 0.0D0
  else
     deallocate(tm_p)
  end if

  flag_xm = .true.
  if (trim(status)=='on') then
     allocate(xm(0:Nx))
     xm = 0.0D0
  else
     deallocate(xm)
  end if

  flag_xm_p = .true.
  if (trim(status)=='on') then
     allocate(xm_p(0:Nx))
     xm_p = 0.0D0
  else
     deallocate(xm_p)
  end if

  flag_alpha = .true.
  if (trim(status)=='on') then
     allocate(alpha(0:Nx))
     alpha = 0.0D0
  else
     deallocate(alpha)
  end if

  flag_alpha_p = .true.
  if (trim(status)=='on') then
     allocate(alpha_p(0:Nx))
     alpha_p = 0.0D0
  else
     deallocate(alpha_p)
  end if

  flag_salpha = .true.
  if (trim(status)=='on') then
     allocate(salpha(0:Nx))
     salpha = 0.0D0
  else
     deallocate(salpha)
  end if

  flag_alpha_a = .true.
  if (trim(status)=='on') then
     allocate(alpha_a(0:Nx))
     alpha_a = 0.0D0
  else
     deallocate(alpha_a)
  end if

  flag_Dalpha = .true.
  if (trim(status)=='on') then
     allocate(Dalpha(0:Nx))
     Dalpha = 0.0D0
  else
     deallocate(Dalpha)
  end if

  flag_Dalpha_p = .true.
  if (trim(status)=='on') then
     allocate(Dalpha_p(0:Nx))
     Dalpha_p = 0.0D0
  else
     deallocate(Dalpha_p)
  end if

  flag_sDalpha = .true.
  if (trim(status)=='on') then
     allocate(sDalpha(0:Nx))
     sDalpha = 0.0D0
  else
     deallocate(sDalpha)
  end if

  flag_Dalpha_a = .true.
  if (trim(status)=='on') then
     allocate(Dalpha_a(0:Nx))
     Dalpha_a = 0.0D0
  else
     deallocate(Dalpha_a)
  end if

  flag_f = .true.
  if (trim(status)=='on') then
     allocate(f(0:Nx))
     f = 0.0D0
  else
     deallocate(f)
  end if

  flag_fp = .true.
  if (trim(status)=='on') then
     allocate(fp(0:Nx))
     fp = 0.0D0
  else
     deallocate(fp)
  end if

  if (shift /= "none") then
     flag_beta = .true.
     if (status=='on') then
        allocate(beta(0:Nx))
        beta = 0.0D0
     else
        deallocate(beta)
     end if
  else if (inlist(outvars0D,"beta").or. &
           inlist(outvars1D,"beta")) then
     print *
     print *, 'Error in parfile: array beta has no storage,'
     print *, 'so no output for it is possible.'
     print *
     print *, 'Aborting! (subroutine allocatearrays.f90)'
     print *
     stop
  end if

  if (shift /= "none") then
     flag_beta_p = .true.
     if (status=='on') then
        allocate(beta_p(0:Nx))
        beta_p = 0.0D0
     else
        deallocate(beta_p)
     end if
  else if (inlist(outvars0D,"beta_p").or. &
           inlist(outvars1D,"beta_p")) then
     print *
     print *, 'Error in parfile: array beta_p has no storage,'
     print *, 'so no output for it is possible.'
     print *
     print *, 'Aborting! (subroutine allocatearrays.f90)'
     print *
     stop
  end if

  if (shift /= "none") then
     flag_Dbeta = .true.
     if (status=='on') then
        allocate(Dbeta(0:Nx))
        Dbeta = 0.0D0
     else
        deallocate(Dbeta)
     end if
  else if (inlist(outvars0D,"Dbeta").or. &
           inlist(outvars1D,"Dbeta")) then
     print *
     print *, 'Error in parfile: array Dbeta has no storage,'
     print *, 'so no output for it is possible.'
     print *
     print *, 'Aborting! (subroutine allocatearrays.f90)'
     print *
     stop
  end if

  if (shift == "harmonic") then
     flag_sigma = .true.
     if (status=='on') then
        allocate(sigma(0:Nx))
        sigma = 0.0D0
     else
        deallocate(sigma)
     end if
  else if (inlist(outvars0D,"sigma").or. &
           inlist(outvars1D,"sigma")) then
     print *
     print *, 'Error in parfile: array sigma has no storage,'
     print *, 'so no output for it is possible.'
     print *
     print *, 'Aborting! (subroutine allocatearrays.f90)'
     print *
     stop
  end if

  if (shift == "harmonic") then
     flag_sigma_p = .true.
     if (status=='on') then
        allocate(sigma_p(0:Nx))
        sigma_p = 0.0D0
     else
        deallocate(sigma_p)
     end if
  else if (inlist(outvars0D,"sigma_p").or. &
           inlist(outvars1D,"sigma_p")) then
     print *
     print *, 'Error in parfile: array sigma_p has no storage,'
     print *, 'so no output for it is possible.'
     print *
     print *, 'Aborting! (subroutine allocatearrays.f90)'
     print *
     stop
  end if

  if (shift == "harmonic") then
     flag_ssigma = .true.
     if (status=='on') then
        allocate(ssigma(0:Nx))
        ssigma = 0.0D0
     else
        deallocate(ssigma)
     end if
  else if (inlist(outvars0D,"ssigma").or. &
           inlist(outvars1D,"ssigma")) then
     print *
     print *, 'Error in parfile: array ssigma has no storage,'
     print *, 'so no output for it is possible.'
     print *
     print *, 'Aborting! (subroutine allocatearrays.f90)'
     print *
     stop
  end if

  if (shift == "harmonic") then
     flag_sigma_a = .true.
     if (status=='on') then
        allocate(sigma_a(0:Nx))
        sigma_a = 0.0D0
     else
        deallocate(sigma_a)
     end if
  else if (inlist(outvars0D,"sigma_a").or. &
           inlist(outvars1D,"sigma_a")) then
     print *
     print *, 'Error in parfile: array sigma_a has no storage,'
     print *, 'so no output for it is possible.'
     print *
     print *, 'Aborting! (subroutine allocatearrays.f90)'
     print *
     stop
  end if

  if (shift == "harmonic") then
     flag_Dsigma = .true.
     if (status=='on') then
        allocate(Dsigma(0:Nx))
        Dsigma = 0.0D0
     else
        deallocate(Dsigma)
     end if
  else if (inlist(outvars0D,"Dsigma").or. &
           inlist(outvars1D,"Dsigma")) then
     print *
     print *, 'Error in parfile: array Dsigma has no storage,'
     print *, 'so no output for it is possible.'
     print *
     print *, 'Aborting! (subroutine allocatearrays.f90)'
     print *
     stop
  end if

  if (shift == "harmonic") then
     flag_Dsigma_p = .true.
     if (status=='on') then
        allocate(Dsigma_p(0:Nx))
        Dsigma_p = 0.0D0
     else
        deallocate(Dsigma_p)
     end if
  else if (inlist(outvars0D,"Dsigma_p").or. &
           inlist(outvars1D,"Dsigma_p")) then
     print *
     print *, 'Error in parfile: array Dsigma_p has no storage,'
     print *, 'so no output for it is possible.'
     print *
     print *, 'Aborting! (subroutine allocatearrays.f90)'
     print *
     stop
  end if

  if (shift == "harmonic") then
     flag_sDsigma = .true.
     if (status=='on') then
        allocate(sDsigma(0:Nx))
        sDsigma = 0.0D0
     else
        deallocate(sDsigma)
     end if
  else if (inlist(outvars0D,"sDsigma").or. &
           inlist(outvars1D,"sDsigma")) then
     print *
     print *, 'Error in parfile: array sDsigma has no storage,'
     print *, 'so no output for it is possible.'
     print *
     print *, 'Aborting! (subroutine allocatearrays.f90)'
     print *
     stop
  end if

  if (shift == "harmonic") then
     flag_Dsigma_a = .true.
     if (status=='on') then
        allocate(Dsigma_a(0:Nx))
        Dsigma_a = 0.0D0
     else
        deallocate(Dsigma_a)
     end if
  else if (inlist(outvars0D,"Dsigma_a").or. &
           inlist(outvars1D,"Dsigma_a")) then
     print *
     print *, 'Error in parfile: array Dsigma_a has no storage,'
     print *, 'so no output for it is possible.'
     print *
     print *, 'Aborting! (subroutine allocatearrays.f90)'
     print *
     stop
  end if

  if (shift == "harmonic") then
     flag_h = .true.
     if (status=='on') then
        allocate(h(0:Nx))
        h = 0.0D0
     else
        deallocate(h)
     end if
  else if (inlist(outvars0D,"h").or. &
           inlist(outvars1D,"h")) then
     print *
     print *, 'Error in parfile: array h has no storage,'
     print *, 'so no output for it is possible.'
     print *
     print *, 'Aborting! (subroutine allocatearrays.f90)'
     print *
     stop
  end if

  if (shift == "harmonic") then
     flag_hp = .true.
     if (status=='on') then
        allocate(hp(0:Nx))
        hp = 0.0D0
     else
        deallocate(hp)
     end if
  else if (inlist(outvars0D,"hp").or. &
           inlist(outvars1D,"hp")) then
     print *
     print *, 'Error in parfile: array hp has no storage,'
     print *, 'so no output for it is possible.'
     print *
     print *, 'Aborting! (subroutine allocatearrays.f90)'
     print *
     stop
  end if

  flag_g = .true.
  if (trim(status)=='on') then
     allocate(g(0:Nx))
     g = 0.0D0
  else
     deallocate(g)
  end if

  flag_g_p = .true.
  if (trim(status)=='on') then
     allocate(g_p(0:Nx))
     g_p = 0.0D0
  else
     deallocate(g_p)
  end if

  flag_sg = .true.
  if (trim(status)=='on') then
     allocate(sg(0:Nx))
     sg = 0.0D0
  else
     deallocate(sg)
  end if

  flag_g_a = .true.
  if (trim(status)=='on') then
     allocate(g_a(0:Nx))
     g_a = 0.0D0
  else
     deallocate(g_a)
  end if

  flag_Dg = .true.
  if (trim(status)=='on') then
     allocate(Dg(0:Nx))
     Dg = 0.0D0
  else
     deallocate(Dg)
  end if

  flag_Dg_p = .true.
  if (trim(status)=='on') then
     allocate(Dg_p(0:Nx))
     Dg_p = 0.0D0
  else
     deallocate(Dg_p)
  end if

  flag_sDg = .true.
  if (trim(status)=='on') then
     allocate(sDg(0:Nx))
     sDg = 0.0D0
  else
     deallocate(sDg)
  end if

  flag_Dg_a = .true.
  if (trim(status)=='on') then
     allocate(Dg_a(0:Nx))
     Dg_a = 0.0D0
  else
     deallocate(Dg_a)
  end if

  if (inlist(outvars0D,"trK").or. &
      inlist(outvars1D,"trK")) then
     flag_trK = .true.
     if (trim(status)=='on') then
        allocate(trK(0:Nx))
        trK = 0.0D0
     else
        deallocate(trK)
     end if
  end if

  flag_Ktilde = .true.
  if (trim(status)=='on') then
     allocate(Ktilde(0:Nx))
     Ktilde = 0.0D0
  else
     deallocate(Ktilde)
  end if

  flag_Ktilde_p = .true.
  if (trim(status)=='on') then
     allocate(Ktilde_p(0:Nx))
     Ktilde_p = 0.0D0
  else
     deallocate(Ktilde_p)
  end if

  flag_sKtilde = .true.
  if (trim(status)=='on') then
     allocate(sKtilde(0:Nx))
     sKtilde = 0.0D0
  else
     deallocate(sKtilde)
  end if

  flag_Ktilde_a = .true.
  if (trim(status)=='on') then
     allocate(Ktilde_a(0:Nx))
     Ktilde_a = 0.0D0
  else
     deallocate(Ktilde_a)
  end if

  flag_lambda0 = .true.
  if (trim(status)=='on') then
     allocate(lambda0(0:Nx))
     lambda0 = 0.0D0
  else
     deallocate(lambda0)
  end if

  flag_lambdafp = .true.
  if (trim(status)=='on') then
     allocate(lambdafp(0:Nx))
     lambdafp = 0.0D0
  else
     deallocate(lambdafp)
  end if

  flag_lambdafm = .true.
  if (trim(status)=='on') then
     allocate(lambdafm(0:Nx))
     lambdafm = 0.0D0
  else
     deallocate(lambdafm)
  end if

  if (shift == "harmonic") then
     flag_lambdahp = .true.
     if (status=='on') then
        allocate(lambdahp(0:Nx))
        lambdahp = 0.0D0
     else
        deallocate(lambdahp)
     end if
  else if (inlist(outvars0D,"lambdahp").or. &
           inlist(outvars1D,"lambdahp")) then
     print *
     print *, 'Error in parfile: array lambdahp has no storage,'
     print *, 'so no output for it is possible.'
     print *
     print *, 'Aborting! (subroutine allocatearrays.f90)'
     print *
     stop
  end if

  if (shift == "harmonic") then
     flag_lambdahm = .true.
     if (status=='on') then
        allocate(lambdahm(0:Nx))
        lambdahm = 0.0D0
     else
        deallocate(lambdahm)
     end if
  else if (inlist(outvars0D,"lambdahm").or. &
           inlist(outvars1D,"lambdahm")) then
     print *
     print *, 'Error in parfile: array lambdahm has no storage,'
     print *, 'so no output for it is possible.'
     print *
     print *, 'Aborting! (subroutine allocatearrays.f90)'
     print *
     stop
  end if

  flag_W0 = .true.
  if (trim(status)=='on') then
     allocate(W0(0:Nx))
     W0 = 0.0D0
  else
     deallocate(W0)
  end if

  flag_sW0 = .true.
  if (trim(status)=='on') then
     allocate(sW0(0:Nx))
     sW0 = 0.0D0
  else
     deallocate(sW0)
  end if

  flag_Wfp = .true.
  if (trim(status)=='on') then
     allocate(Wfp(0:Nx))
     Wfp = 0.0D0
  else
     deallocate(Wfp)
  end if

  flag_sWfp = .true.
  if (trim(status)=='on') then
     allocate(sWfp(0:Nx))
     sWfp = 0.0D0
  else
     deallocate(sWfp)
  end if

  flag_Wfm = .true.
  if (trim(status)=='on') then
     allocate(Wfm(0:Nx))
     Wfm = 0.0D0
  else
     deallocate(Wfm)
  end if

  flag_sWfm = .true.
  if (trim(status)=='on') then
     allocate(sWfm(0:Nx))
     sWfm = 0.0D0
  else
     deallocate(sWfm)
  end if

  if (shift == "harmonic") then
     flag_Whp = .true.
     if (status=='on') then
        allocate(Whp(0:Nx))
        Whp = 0.0D0
     else
        deallocate(Whp)
     end if
  else if (inlist(outvars0D,"Whp").or. &
           inlist(outvars1D,"Whp")) then
     print *
     print *, 'Error in parfile: array Whp has no storage,'
     print *, 'so no output for it is possible.'
     print *
     print *, 'Aborting! (subroutine allocatearrays.f90)'
     print *
     stop
  end if

  if (shift == "harmonic") then
     flag_sWhp = .true.
     if (status=='on') then
        allocate(sWhp(0:Nx))
        sWhp = 0.0D0
     else
        deallocate(sWhp)
     end if
  else if (inlist(outvars0D,"sWhp").or. &
           inlist(outvars1D,"sWhp")) then
     print *
     print *, 'Error in parfile: array sWhp has no storage,'
     print *, 'so no output for it is possible.'
     print *
     print *, 'Aborting! (subroutine allocatearrays.f90)'
     print *
     stop
  end if

  if (shift == "harmonic") then
     flag_Whm = .true.
     if (status=='on') then
        allocate(Whm(0:Nx))
        Whm = 0.0D0
     else
        deallocate(Whm)
     end if
  else if (inlist(outvars0D,"Whm").or. &
           inlist(outvars1D,"Whm")) then
     print *
     print *, 'Error in parfile: array Whm has no storage,'
     print *, 'so no output for it is possible.'
     print *
     print *, 'Aborting! (subroutine allocatearrays.f90)'
     print *
     stop
  end if

  if (shift == "harmonic") then
     flag_sWhm = .true.
     if (status=='on') then
        allocate(sWhm(0:Nx))
        sWhm = 0.0D0
     else
        deallocate(sWhm)
     end if
  else if (inlist(outvars0D,"sWhm").or. &
           inlist(outvars1D,"sWhm")) then
     print *
     print *, 'Error in parfile: array sWhm has no storage,'
     print *, 'so no output for it is possible.'
     print *
     print *, 'Aborting! (subroutine allocatearrays.f90)'
     print *
     stop
  end if

  flag_char0 = .true.
  if (trim(status)=='on') then
     allocate(char0(0:Nx))
     char0 = 0.0D0
  else
     deallocate(char0)
  end if

  flag_char0_p = .true.
  if (trim(status)=='on') then
     allocate(char0_p(0:Nx))
     char0_p = 0.0D0
  else
     deallocate(char0_p)
  end if

  flag_schar0 = .true.
  if (trim(status)=='on') then
     allocate(schar0(0:Nx))
     schar0 = 0.0D0
  else
     deallocate(schar0)
  end if

  flag_charp = .true.
  if (trim(status)=='on') then
     allocate(charp(0:Nx))
     charp = 0.0D0
  else
     deallocate(charp)
  end if

  flag_charp_p = .true.
  if (trim(status)=='on') then
     allocate(charp_p(0:Nx))
     charp_p = 0.0D0
  else
     deallocate(charp_p)
  end if

  flag_scharp = .true.
  if (trim(status)=='on') then
     allocate(scharp(0:Nx))
     scharp = 0.0D0
  else
     deallocate(scharp)
  end if

  flag_charm = .true.
  if (trim(status)=='on') then
     allocate(charm(0:Nx))
     charm = 0.0D0
  else
     deallocate(charm)
  end if

  flag_charm_p = .true.
  if (trim(status)=='on') then
     allocate(charm_p(0:Nx))
     charm_p = 0.0D0
  else
     deallocate(charm_p)
  end if

  flag_scharm = .true.
  if (trim(status)=='on') then
     allocate(scharm(0:Nx))
     scharm = 0.0D0
  else
     deallocate(scharm)
  end if

  flag_Dchar0 = .true.
  if (trim(status)=='on') then
     allocate(Dchar0(0:Nx))
     Dchar0 = 0.0D0
  else
     deallocate(Dchar0)
  end if

  flag_Dcharp = .true.
  if (trim(status)=='on') then
     allocate(Dcharp(0:Nx))
     Dcharp = 0.0D0
  else
     deallocate(Dcharp)
  end if

  flag_Dcharm = .true.
  if (trim(status)=='on') then
     allocate(Dcharm(0:Nx))
     Dcharm = 0.0D0
  else
     deallocate(Dcharm)
  end if

  if (inlist(outvars0D,"Calpha").or. &
      inlist(outvars1D,"Calpha")) then
     flag_Calpha = .true.
     if (trim(status)=='on') then
        allocate(Calpha(0:Nx))
        Calpha = 0.0D0
     else
        deallocate(Calpha)
     end if
  end if

  if (inlist(outvars0D,"Cg").or. &
      inlist(outvars1D,"Cg")) then
     flag_Cg = .true.
     if (trim(status)=='on') then
        allocate(Cg(0:Nx))
        Cg = 0.0D0
     else
        deallocate(Cg)
     end if
  end if

  if (inlist(outvars0D,"Csigma").or. &
      inlist(outvars1D,"Csigma")) then
     flag_Csigma = .true.
     if (trim(status)=='on') then
        allocate(Csigma(0:Nx))
        Csigma = 0.0D0
     else
        deallocate(Csigma)
     end if
  end if

  end subroutine allocatearrays
