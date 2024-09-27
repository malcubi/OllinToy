!$Header: /usr/local/ollincvs/Codes/OllinToy/src/functions.f90,v 1.3 2008/01/23 17:34:53 malcubi Exp $

! ************************************
! ***   VARIOUS USEFUL FUNCTIONS   ***
! ************************************

! Here I define various useful functions:
!
! Here I define various useful functions:
!
! contains        Checks if a string contains a specific pattern.
!
! inlist:         Similar to contains, but the pattern must be
!                 separates by commas from other patterns.
!
! message         Creates a properly formatted message to screen.


! *****************************
! ***   FUNCTION CONTAINS   ***
! *****************************

  logical function contains(string,pattern)

! This function checks if a given string contains
! a specific pattern.

  integer i,l1,l2

  character(len=*) string,pattern

! Initialize.

  contains = .false.

! Find out length of strings and check if the
! pattern is smaller than the string.

  l1 = len(string)
  l2 = len(pattern)

  if (l2>l1) return

! Now check if pattern is contained in string.

  do i=1,l1-l2+1
     if (string(i:i+l2-1)==pattern) contains = .true.
  end do

! End.

  end function contains





! ***************************
! ***   FUNCTION INLIST   ***
! ***************************

  logical function inlist(string,pattern)

! This function is similar to contains.
! It checks if a given string contains a specific pattern
! in a comma or space separated list, that is, the pattern
! must be separated by commas and/or spaces from other
! elements in the list.

  integer i,l1,l2

  character(len=*) string,pattern

! Initialize.

  inlist = .false.

! Find out length of strings and check if the
! pattern is smaller than the string.

  l1 = len(string)
  l2 = len(pattern)

  if (l2>l1) return

! Check if there are commas or spaces in the pattern.
! This is not allowed so the code complains and stops.

  do i=1,l2
     if ((pattern(i:i)==",").or.(pattern(i:i)==" ")) then
       stop 'Error in function inlist. The patern is not allowed to have commas or spaces.\n'
     end if
  end do

! If the pattern is equal to the full string then
! the list had only one element.

  if (string==pattern) then
     inlist = .true.
     return
  end if

! Now check if pattern is contained in string.

  do i=1,l1-l2+1

     if (string(i:i+l2-1)==pattern) then
 
!       The pattern is in the string.  Now
!       check for commas or spaces.

        if (((i==1).or.(string(i-1:i-1)==",").or.(string(i-1:i-1)==" ")).and. &
            ((i+l2-1==l1).or.(string(i+l2:i+l2)==",").or.(string(i+l2:i+l2)==" "))) then
           inlist = .true.
        end if

     end if

  end do

! End.

  end function inlist






! ****************************
! ***   FUNCTION MESSAGE   ***
! ****************************

  character(len=*) function message(string1,string2)

! This function creates a properly formatted message
! to be sent out to the screen taking the two input
! strings (string1,string2).

  implicit none

  integer i,j,l

  character(len=*) string1,string2
  character(50) work

! Find out length of full concatenated string.

  l = len(trim(string1)//" "//trim(string2))

! Length is even, concatenated string is OK.

  if (real(l/2)==real(l)/2.0D0) then

     j = l/2
     work = trim(string1)//" "//trim(string2)

! Length is odd, concatenated string needs extra space.

  else

     j = l/2 + 1
     work = trim(string1)//"  "//trim(string2)

  end if

! Construct message. Notice that the message is constructed
! from left to right.

  message = "***"

  do i=1,18-j
     message = " "//message
  end do

  message = trim(work)//message

  do i=1,18-j
     message = " "//message
  end do

  message = "***"//message

! End.

  end function message


