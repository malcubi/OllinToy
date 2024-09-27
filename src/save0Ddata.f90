!$Header: /usr/local/ollincvs/Codes/OllinToy/src/save0Ddata.f90,v 1.8 2005/04/27 23:14:49 malcubi Exp $

  subroutine save0Ddata(step,t)

! ********************************
! ***   SAVE 0D DATA TO FILE   ***
! ********************************

! This routine saves "0D" data to files. By 0D data I mean reduced
! quantities as functions of time (minimum, maximum, norms, etc).

! Include modules.

  use param
  use arrays

! Extra variables.

  implicit none

  logical firstcall,iscomplex

  integer i,j,step
  integer l,nvars
  integer, allocatable, dimension (:) :: commas

  real(8) t

  real(8), dimension (0:Nx) :: varr,vari
  complex(8), dimension (0:Nx) :: var

  character(1)  aa
  character(20) filestatus

  character(50), allocatable, dimension (:) :: outvars

  data firstcall / .true. /

  save firstcall,nvars,outvars


! ***********************************
! ***   IS THIS THE FIRST CALL?   ***
! ***********************************

! On first call, replace file and figure
! out what needs output.

  if (firstcall) then

     firstcall = .false.

!    File status.

     filestatus = 'replace'

!    Find out length of string "outvars0D".

     l = len_trim(outvars0D)

!    Find out how many variables need output.

     nvars = 1

     do i=1,l
        aa = outvars0D(i:i)
        if (aa==",") then
           nvars = nvars + 1
        end if
     end do

!    Identify comma positions.

     allocate(commas(0:nvars))

     j = 0
     commas(0) = 0
     commas(nvars) = l+1

     do i=1,l
        aa = outvars0D(i:i)
        if (aa==",") then
           j = j+1
           commas(j)=i
        end if
     end do

!    Now read variable names, and eliminate spaces.

     allocate(outvars(1:nvars))

     do i=1,nvars
        outvars(i) = outvars0D(commas(i-1)+1:commas(i)-1)
        outvars(i) = trim(adjustl(outvars(i)))
     end do

!    Check if any name is repeated.

     do i=1,nvars
        do j=1,nvars
           if ((i.ne.j).and.(trim(outvars(i))==trim(outvars(j)))) then
              print *
              print *, 'Error in parfile, array name repeated in outvars0D: ',outvars(i)
              print *
              print *, 'Aborting! (subroutine save0Ddata)'
              print *
              stop
           end if
        end do
     end do

! Not first call.

  else

     filestatus = 'old'

  end if


! *********************
! ***   SAVE DATA   ***
! *********************

  do i=1,nvars

     call grabarray(trim(outvars(i)),var,iscomplex)

     if (iscomplex) then
        varr = real(var)
        call save0Dvariable(Nx,t,x,varr,trim(outvars(i))//"_r",trim(directory), &
        filestatus,trim(outtype),trim(commenttype))
        vari = aimag(var)
        call save0Dvariable(Nx,t,x,vari,trim(outvars(i))//"_i",trim(directory), &
        filestatus,trim(outtype),trim(commenttype))
     else
        varr = var
        call save0Dvariable(Nx,t,x,varr,trim(outvars(i)),trim(directory), &
        filestatus,trim(outtype),trim(commenttype))
     end if

  end do


! ***********************************
! ***   DEALLOCATE LOCAL ARRAYS   ***
! ***********************************

! On last step deallocate local arrays.  For some reason I don't
! understand, the code sends an error if I try to deallocate the
! integer array 'commas', so I only deallocate the array 'outvars'.

  if (step==Nt) then
     deallocate(outvars)
  end if


! ***************
! ***   END   ***
! ***************

  end subroutine save0Ddata











  subroutine save0Dvariable(Nx,t,x,var,varname,directory,filestatus,outtype,commenttype)

! This subroutine calculates minimum, maximum, norm 1 and norm 2
! of a variable and outputs them.
!
! There are two types of output:
!
! standard:     Formatted output for normal runs.
!
! test:         Output for testsuites, where I take care to reduce
!               the chance of round-off errors being output.

  implicit none

  integer i,Nx

  real(8) t
  real(8) vmax,vmin,nm1,nm2
  real(8) Tvar,xmax,xmin

  real(8), dimension (0:Nx) :: x,var

  character(len=1) comment
  character(len=50) aa,bb
  character(len=*) varname,directory,filestatus,outtype,commenttype


! *************************
! ***   SAVE VARIABLE   ***
! *************************

! Decide on comment type.

  if (commenttype=='xgraph') then
     comment = '"'
  else
     comment = '#'
  end if

! The quantities saved are:
!
! vmax:    Maximum value of the variable over the grid.
! vmin:    Minimum value of the variable over the grid.
!
! nm1:     Maximum absolute value.
! nm2:     Root mean square (rms).

  nm1 = 0.0D0
  nm2 = 0.0D0

  vmax = var(0)
  vmin = var(0)

  xmax = x(0)
  xmin = x(0)

  do i=0,Nx
     nm2 = nm2 + var(i)**2
     if (var(i)>vmax) then
        vmax = var(i); xmax = x(i)
     end if
     if (var(i)<vmin) then
        vmin = var(i); xmin = x(i)
     end if
  end do

  nm1 = max(abs(vmax),abs(vmin))
  nm2 = dsqrt(nm2/dble(Nx+1))

  Tvar = 0.0D0

  do i=0,Nx-1
     Tvar = Tvar + abs(var(i+1)-var(i))
  end do

! Save maximum value.

  if (filestatus == 'replace') then
     open(1,file=directory//'/'//varname//'_max.tl',form='formatted', &
     status='replace')
     write(1,*) comment//varname//'_max.tl'
  else
     open(1,file=directory//'/'//varname//'_max.tl',form='formatted', &
     status='old',position='append')
  end if

  write(1,"(2ES16.8)") t,vmax

  close(1)

! Save minimum value.

  if (filestatus == 'replace') then
     open(1,file=directory//'/'//varname//'_min.tl',form='formatted', &
     status='replace')
     write(1,*) comment//varname//'_min.tl'
  else
     open(1,file=directory//'/'//varname//'_min.tl',form='formatted', &
     status='old',position='append')
  end if

  write(1,"(2ES16.8)") t,vmin

  close(1)

! Save norm 1.

  if (filestatus == 'replace') then
     open(1,file=directory//'/'//varname//'_nm1.tl',form='formatted', &
     status='replace')
     write(1,*) comment//varname//'_nm1.tl'
  else
     open(1,file=directory//'/'//varname//'_nm1.tl',form='formatted', &
     status='old',position='append')
  end if

  write(1,"(2ES16.8)") t,nm1

  close(1)

! Save norm 2.

  if (filestatus == 'replace') then
     open(1,file=directory//'/'//varname//'_nm2.tl',form='formatted', &
     status='replace')
     write(1,*) comment//varname//'_nm2.tl'
  else
     open(1,file=directory//'/'//varname//'_nm2.tl',form='formatted', &
     status='old',position='append')
  end if

  write(1,"(2ES16.8)") t,nm2

  close(1)

! Save total variation.

  if (filestatus == 'replace') then
     open(1,file=directory//'/'//varname//'_var.tl',form='formatted', &
     status='replace')
     write(1,*) comment//varname//'_var.tl'
  else
     open(1,file=directory//'/'//varname//'_var.tl',form='formatted', &
     status='old',position='append')
  end if

  write(1,"(2ES16.8)") t,Tvar

  close(1)



! ***************
! ***   END   ***
! ***************

  end subroutine save0Dvariable

