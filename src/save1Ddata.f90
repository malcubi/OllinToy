!$Header: /usr/local/ollincvs/Codes/OllinToy/src/save1Ddata.f90,v 1.12 2005/04/27 22:41:41 malcubi Exp $

  subroutine save1Ddata(step,t)

! ********************************
! ***   SAVE 1D DATA TO FILE   ***
! ********************************

! This routine saves 1D data to the files, that is,
! the whole spatial arrays at a given time.

! Include modules.

  use param
  use arrays

! Extra variables.

  implicit none

  logical firstcall,iscomplex

  integer i,j,k,step
  integer l,nvars
  integer, allocatable, dimension (:) :: commas

  real(8) t

  real(8), dimension (0:Nx) :: varr,vari
  complex(8), dimension (0:Nx) :: var

  character(1)  aa
  character(20) filestatus,number

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

!    Find out length of string "outvars1D".

     l = len_trim(outvars1D)

!    Find out how many variables need output.

     nvars = 1

     do i=1,l
        aa = outvars1D(i:i)
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
        aa = outvars1D(i:i)
        if (aa==",") then
           j = j+1
           commas(j)=i
        end if
     end do

!    Now read variable names, and eliminate spaces.

     allocate(outvars(1:nvars))

     do i=1,nvars
        outvars(i) = outvars1D(commas(i-1)+1:commas(i)-1)
        outvars(i) = trim(adjustl(outvars(i)))
     end do

!    Check if any name is repeated.

     do i=1,nvars
        do j=1,nvars
           if ((i.ne.j).and.(trim(outvars(i))==trim(outvars(j)))) then
              print *
              print *, 'Error in parfile, array name repeated in outvars1D: ',outvars(i)
              print *
              print *, 'Aborting! (subroutine save1Ddata)'
              print *
              stop
           end if
        end do
     end do

! Not first call.

  else

     filestatus = 'old'

  end if


! ***************************
! ***   SAVE DATA FILES   ***
! ***************************

  do i=1,nvars

     call grabarray(trim(outvars(i)),var,iscomplex)

     if (iscomplex) then
        varr = real(var)
        call save1Dvariable(Nx,t,x,varr,trim(outvars(i))//'_r.xl', &
        trim(directory),filestatus,trim(outtype),trim(commenttype))
        vari = aimag(var)
        call save1Dvariable(Nx,t,x,vari,trim(outvars(i))//'_i.xl', &
        trim(directory),filestatus,trim(outtype),trim(commenttype))
     else
        varr = var
        call save1Dvariable(Nx,t,x,varr,trim(outvars(i))//'.xl', &
        trim(directory),filestatus,trim(outtype),trim(commenttype))
     end if

  end do

  close(1)


! *************************************
! ***   SAVE MINKOWSKI COODINATES   ***
! *************************************

! This is done separately, as the format is different.

  if (trackobs) then

!    File for tm.

     varr = tm

     call save1Dvariable(Nx,t,xm,varr,'tm.xm', &
     trim(directory),filestatus,trim(outtype),trim(commenttype))

!    Files for (tm-t).

     varr = tm-t

     call save1Dvariable(Nx,t,x,varr,'tm_diff.xl', &
     trim(directory),filestatus,trim(outtype),trim(commenttype))

     call save1Dvariable(Nx,t,xm,varr,'tm_diff.xm', &
     trim(directory),filestatus,trim(outtype),trim(commenttype))

!    File for (xm-x).

     varr = xm-x

     call save1Dvariable(Nx,t,x,varr,'xm_diff.xl', &
     trim(directory),filestatus,trim(outtype),trim(commenttype))

  end if


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

  end subroutine save1Ddata











  subroutine save1Dvariable(Nx,t,x,var,filename,directory,filestatus,outtype,commenttype)

! This subroutine outputs 1D files.
!
! There are two types of output:
!
! standard:     Formatted output for normal runs.
!
! test:         Output for testsuites, where I take care to reduce
!               the chance of round-off errors being output.

  implicit none

  integer i,j,Nx
  integer ndec

  real(8) t
  real(8) oneplus,small

  real(8), dimension (0:Nx) :: x
  real(8), dimension (0:Nx) :: var

  character(len=50) aa,bb
  character(len=*)  filename,directory,filestatus,outtype,commenttype

! ndec is the number of decimal places to be output for testsuites.
! oneplus is 1 plus 10^-13, I use it to lift numbers up a bit so I can
! avoid having 1.0 output as 0.99999...

  oneplus = 1.0D0 + 1.0D-13
  ndec = 8
  small = 1.0D-8


! **************************************
! ***   SAVE VARIABLE in terms of r  ***
! **************************************

! Open file.

  if (filestatus=='replace') then
     open(1,file=directory//'/'//filename,form='formatted', &
     status='replace')
  else
     open(1,file=directory//'/'//filename,form='formatted', &
     status='old',position='append')
  end if

! Write current time.

  if (commenttype=='xgraph') then
     write(1,"(A8,ES14.6)") '"Time = ',t
  else
     write(1,"(A8,ES14.6)") '#Time = ',t
  end if

! Write 1D array to file.

  if (outtype=="standard") then

!    Standard output.

     do i=0,Nx
        if (dabs(var(i)).gt.1.0D-50) then
           write(1,"(2ES16.8)") x(i),var(i)
        else
           write(1,"(2ES16.8)") x(i),0.0D0
        end if 
     end do

  else

!    Output for tests, where we are careful not to output
!    quantities contaminated by round-off errors.

     do i=0,Nx
        write(aa,"(F30.16)") x(i)*oneplus
        if (abs(var(i)).lt.small) var(i)=0.0D0
        write(bb,"(F30.16)") var(i)*oneplus
        aa = aa(1:ndec+14); bb=bb(1:ndec+14)
        write(1,*) trim(adjustl(aa))," ",trim(adjustl(bb))
     end do

  end if

! Leave blank space before next time.

  write(1,*)

! Close file.

  close(1)


! ***************
! ***   END   ***
! ***************

  end subroutine save1Dvariable





