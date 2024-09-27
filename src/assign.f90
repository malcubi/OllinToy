! Automatically generated file.  Do not edit!

  subroutine assign(nline,nvalues,var,value,values)

  use param

  implicit none

  logical contains
  integer i,nvalues
  integer(kind=2) nline
  character(50) type
  character(1000) range
  character(len=*) var,value
  character(len=*) values(1:nvalues)

  if (var=='dx') then
     if (nvalues>1) goto 200
     type = 'REAL'
     read(value,*,ERR=100) dx
     return
  end if

  if (var=='Nx') then
     if (nvalues>1) goto 200
     type = 'INTEGER'
     read(value,*,ERR=100) Nx
     return
  end if

  if (var=='dtfac') then
     if (nvalues>1) goto 200
     type = 'REAL'
     read(value,*,ERR=100) dtfac
     return
  end if

  if (var=='Nt') then
     if (nvalues>1) goto 200
     type = 'INTEGER'
     read(value,*,ERR=100) Nt
     return
  end if

  if (var=='directory') then
     type = 'CHARACTER'
     directory = value
     if (nvalues>1) goto 200
     return
  end if

  if (var=='Ninfo') then
     if (nvalues>1) goto 200
     type = 'INTEGER'
     read(value,*,ERR=100) Ninfo
     return
  end if

  if (var=='Noutput0D') then
     if (nvalues>1) goto 200
     type = 'INTEGER'
     read(value,*,ERR=100) Noutput0D
     return
  end if

  if (var=='Noutput1D') then
     if (nvalues>1) goto 200
     type = 'INTEGER'
     read(value,*,ERR=100) Noutput1D
     return
  end if

  if (var=='outvars0D') then
     type = 'CHARACTER'
     outvars0D = value
     return
  end if

  if (var=='outvars1D') then
     type = 'CHARACTER'
     outvars1D = value
     return
  end if

  if (var=='commenttype') then
     type = 'CHARACTER'
     commenttype = value
     if (nvalues>1) goto 200
     range = &
     '(xgraph,gnuplot)'
     do i=1,nvalues
        if ((.not.contains(range,'('//trim(values(i))//')')).and. &
            (.not.contains(range,'('//trim(values(i))//',')).and. &
            (.not.contains(range,','//trim(values(i))//')')).and. &
            (.not.contains(range,','//trim(values(i))//','))) then
           print *
           print *, 'Parfile error.'
           print *, 'Out of range value ''',trim(values(i)),''' for parameter ''',trim(var),''' in line:',nline
           print *
           print *, 'Aborting! (subroutine assign.f90)'
           print *
           stop
        end if
     end do
     return
  end if

  if (var=='outtype') then
     type = 'CHARACTER'
     outtype = value
     if (nvalues>1) goto 200
     range = &
     '(standard,test)'
     do i=1,nvalues
        if ((.not.contains(range,'('//trim(values(i))//')')).and. &
            (.not.contains(range,'('//trim(values(i))//',')).and. &
            (.not.contains(range,','//trim(values(i))//')')).and. &
            (.not.contains(range,','//trim(values(i))//','))) then
           print *
           print *, 'Parfile error.'
           print *, 'Out of range value ''',trim(values(i)),''' for parameter ''',trim(var),''' in line:',nline
           print *
           print *, 'Aborting! (subroutine assign.f90)'
           print *
           stop
        end if
     end do
     return
  end if

  if (var=='gauge_f') then
     if (nvalues>1) goto 200
     type = 'REAL'
     read(value,*,ERR=100) gauge_f
     return
  end if

  if (var=='slicing') then
     type = 'CHARACTER'
     slicing = value
     if (nvalues>1) goto 200
     range = &
     '(harmonic,1+log,shockavoid,shock0,shock1)'
     do i=1,nvalues
        if ((.not.contains(range,'('//trim(values(i))//')')).and. &
            (.not.contains(range,'('//trim(values(i))//',')).and. &
            (.not.contains(range,','//trim(values(i))//')')).and. &
            (.not.contains(range,','//trim(values(i))//','))) then
           print *
           print *, 'Parfile error.'
           print *, 'Out of range value ''',trim(values(i)),''' for parameter ''',trim(var),''' in line:',nline
           print *
           print *, 'Aborting! (subroutine assign.f90)'
           print *
           stop
        end if
     end do
     return
  end if

  if (var=='beta0') then
     if (nvalues>1) goto 200
     type = 'REAL'
     read(value,*,ERR=100) beta0
     return
  end if

  if (var=='gauge_h') then
     if (nvalues>1) goto 200
     type = 'REAL'
     read(value,*,ERR=100) gauge_h
     return
  end if

  if (var=='shift') then
     type = 'CHARACTER'
     shift = value
     if (nvalues>1) goto 200
     range = &
     '(none,zero,constant,static,harmonic,1+sigma2)'
     do i=1,nvalues
        if ((.not.contains(range,'('//trim(values(i))//')')).and. &
            (.not.contains(range,'('//trim(values(i))//',')).and. &
            (.not.contains(range,','//trim(values(i))//')')).and. &
            (.not.contains(range,','//trim(values(i))//','))) then
           print *
           print *, 'Parfile error.'
           print *, 'Out of range value ''',trim(values(i)),''' for parameter ''',trim(var),''' in line:',nline
           print *
           print *, 'Aborting! (subroutine assign.f90)'
           print *
           stop
        end if
     end do
     return
  end if

  if (var=='lapsegauss_a0') then
     if (nvalues>1) goto 200
     type = 'REAL'
     read(value,*,ERR=100) lapsegauss_a0
     return
  end if

  if (var=='lapsegauss_x0') then
     if (nvalues>1) goto 200
     type = 'REAL'
     read(value,*,ERR=100) lapsegauss_x0
     return
  end if

  if (var=='lapsegauss_s0') then
     if (nvalues>1) goto 200
     type = 'REAL'
     read(value,*,ERR=100) lapsegauss_s0
     return
  end if

  if (var=='shiftgauss_a0') then
     if (nvalues>1) goto 200
     type = 'REAL'
     read(value,*,ERR=100) shiftgauss_a0
     return
  end if

  if (var=='shiftgauss_x0') then
     if (nvalues>1) goto 200
     type = 'REAL'
     read(value,*,ERR=100) shiftgauss_x0
     return
  end if

  if (var=='shiftgauss_s0') then
     if (nvalues>1) goto 200
     type = 'REAL'
     read(value,*,ERR=100) shiftgauss_s0
     return
  end if

  if (var=='metricgauss_a0') then
     if (nvalues>1) goto 200
     type = 'REAL'
     read(value,*,ERR=100) metricgauss_a0
     return
  end if

  if (var=='metricgauss_x0') then
     if (nvalues>1) goto 200
     type = 'REAL'
     read(value,*,ERR=100) metricgauss_x0
     return
  end if

  if (var=='metricgauss_s0') then
     if (nvalues>1) goto 200
     type = 'REAL'
     read(value,*,ERR=100) metricgauss_s0
     return
  end if

  if (var=='pulsegauss_a0') then
     if (nvalues>1) goto 200
     type = 'REAL'
     read(value,*,ERR=100) pulsegauss_a0
     return
  end if

  if (var=='pulsegauss_x0') then
     if (nvalues>1) goto 200
     type = 'REAL'
     read(value,*,ERR=100) pulsegauss_x0
     return
  end if

  if (var=='pulsegauss_s0') then
     if (nvalues>1) goto 200
     type = 'REAL'
     read(value,*,ERR=100) pulsegauss_s0
     return
  end if

  if (var=='initialdata') then
     type = 'CHARACTER'
     initialdata = value
     range = &
     '(lapsegauss,shiftgauss,metricgauss,slicegauss,pureright,pureleft)'
     do i=1,nvalues
        if ((.not.contains(range,'('//trim(values(i))//')')).and. &
            (.not.contains(range,'('//trim(values(i))//',')).and. &
            (.not.contains(range,','//trim(values(i))//')')).and. &
            (.not.contains(range,','//trim(values(i))//','))) then
           print *
           print *, 'Parfile error.'
           print *, 'Out of range value ''',trim(values(i)),''' for parameter ''',trim(var),''' in line:',nline
           print *
           print *, 'Aborting! (subroutine assign.f90)'
           print *
           stop
        end if
     end do
     return
  end if

  if (var=='integrator') then
     type = 'CHARACTER'
     integrator = value
     if (nvalues>1) goto 200
     range = &
     '(icn,rk4)'
     do i=1,nvalues
        if ((.not.contains(range,'('//trim(values(i))//')')).and. &
            (.not.contains(range,'('//trim(values(i))//',')).and. &
            (.not.contains(range,','//trim(values(i))//')')).and. &
            (.not.contains(range,','//trim(values(i))//','))) then
           print *
           print *, 'Parfile error.'
           print *, 'Out of range value ''',trim(values(i)),''' for parameter ''',trim(var),''' in line:',nline
           print *
           print *, 'Aborting! (subroutine assign.f90)'
           print *
           stop
        end if
     end do
     return
  end if

  if (var=='method') then
     type = 'CHARACTER'
     method = value
     if (nvalues>1) goto 200
     range = &
     '(center,upwind,minmod,vanleer)'
     do i=1,nvalues
        if ((.not.contains(range,'('//trim(values(i))//')')).and. &
            (.not.contains(range,'('//trim(values(i))//',')).and. &
            (.not.contains(range,','//trim(values(i))//')')).and. &
            (.not.contains(range,','//trim(values(i))//','))) then
           print *
           print *, 'Parfile error.'
           print *, 'Out of range value ''',trim(values(i)),''' for parameter ''',trim(var),''' in line:',nline
           print *
           print *, 'Aborting! (subroutine assign.f90)'
           print *
           stop
        end if
     end do
     return
  end if

  if (var=='boundtype') then
     type = 'CHARACTER'
     boundtype = value
     if (nvalues>1) goto 200
     range = &
     '(flat,periodic,outgoing,reflectplus,reflectminus)'
     do i=1,nvalues
        if ((.not.contains(range,'('//trim(values(i))//')')).and. &
            (.not.contains(range,'('//trim(values(i))//',')).and. &
            (.not.contains(range,','//trim(values(i))//')')).and. &
            (.not.contains(range,','//trim(values(i))//','))) then
           print *
           print *, 'Parfile error.'
           print *, 'Out of range value ''',trim(values(i)),''' for parameter ''',trim(var),''' in line:',nline
           print *
           print *, 'Aborting! (subroutine assign.f90)'
           print *
           stop
        end if
     end do
     return
  end if

  if (var=='trackobs') then
     if (nvalues>1) goto 200
     type = 'LOGICAL'
     read(value,*,ERR=100) trackobs
     if (nvalues>1) goto 200
     return
  end if

  if (var=='trackchar') then
     if (nvalues>1) goto 200
     type = 'LOGICAL'
     read(value,*,ERR=100) trackchar
     if (nvalues>1) goto 200
     return
  end if

  print *
  print *, 'Parfile error, non-existent parameter in line:',nline
  print *
  print *, 'Aborting! (subroutine assign.f90)'
  print *
  stop

  100 continue
  print *
  print *, 'There was an error assigning the variable ''',trim(var),''' in line:',nline
  print *, 'Are you sure you gave a ',trim(type),' value?'
  print *
  print *, 'Aborting! (subroutine assign.f90)'
  print *
  stop

  200 continue
  print *
  print *, 'Multiple values not allowed for variable ''',trim(var),''' in line:',nline
  print *
  print *, 'Aborting! (subroutine assign.f90)'
  print *
  stop

  end subroutine assign
