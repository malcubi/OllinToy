!$Header: /usr/local/ollincvs/Codes/OllinToy/src/main.f90,v 1.11 2009/04/29 23:53:21 malcubi Exp $

! *****************************
! ***   PROGRAM  OLLINTOY   ***
! *****************************

  program main

! Include modules.

  use param

! Extra variables.

  implicit none

  character(30) parfile
  character(50) message,string1,string2

! The variables introduced above are:
!
! parfile       Name of parameter file.
!
! message       Function to format messages to screen (see functions.f90).
! string*       Auxilliary strings.


! ***************************
! ***   INITIAL MESSAGE   ***
! ***************************

  print *
  print *, '******************************************'
  print *, '******************************************'
  print *, '***                                    ***'
  print *, '***          PROGRAM OLLINTOY          ***'
  print *, '***                                    ***'
  print *, '******************************************'
  print *, '******************************************'
  print *
  print *


! *********************************
! ***   CALL PARAMETER PARSER   ***
! *********************************

! Get name of parameter file.

  call getarg(1,parfile)

  if (parfile==" ") then
     print *, 'Missing parfile name.'
     print *
     print *, 'Aborting!'
     print *
     stop
  end if

! Call parser.

  call parse(parfile)


! **************************
! ***   OTHER MESSAGES   ***
! **************************

  print *, '******************************************'
  print *, '******************************************'
  print *, '***                                    ***'
  print *, '***   Evolving Einstein''s equations    ***'
  print *, '***         in 1+1 dimensions          ***'
  print *, '***                                    ***'

! Slicing message.

  string1 = "slicing:"
  string2 = trim(adjustl(slicing))

  print *, message(string1,string2)

! Shift message.

  string1 = "shift:"
  string2 = trim(adjustl(shift))

  print *, message(string1,string2)

! Initial data message.

  print *, '***                                    ***'

  string1 = "data:"
  string2 = trim(adjustl(initialdata))

  print *, message(string1,string2)

! Integrator message.

  print *, '***                                    ***'

  string1 = "integrator:"
  string2 = trim(adjustl(integrator))

  print *, message(string1,string2)

! Method message.

  string1 = "method:"
  string2 = trim(adjustl(method))

  print *, message(string1,string2)

! End message.

  print *, '***                                    ***'
  print *, '******************************************'
  print *, '******************************************'
  print *


! *********************************
! ***   CREATE DATA DIRECTORY   ***
! *********************************

  call system('mkdir -p '//trim(directory))
  call system('cp '//trim(parfile)//' '//trim(directory))


! ***************************
! ***   ALLOCATE ARRAYS   ***
! ***************************

! Allocate arrays and initialize to zero.

  call allocatearrays('on')


! *********************
! ***   EVOLUTION   ***
! *********************

  call evolve


! *****************************
! ***   DEALLOCATE ARRAYS   ***
! *****************************

! Deallocate arrays.

  call allocatearrays('off')


! ***************
! ***   END   ***
! ***************

  print *
  print *, 'PROGRAM OLLINTOY HAS FINISHED'
  print *
  print *, 'Have a nice day!'
  print *
  print *
  print *

  end program main
