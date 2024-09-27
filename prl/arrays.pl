#!/usr/bin/env perl

#$Header: /usr/local/ollincvs/Codes/OllinToy/prl/arrays.pl,v 1.7 2009/04/30 00:06:16 malcubi Exp $

# This perl script creates the files:
#
# grabarray.f90
# allocatearrays.f90
# arrayflags.f90

print "CREATING FILES FOR DEALING WITH ARRAYS ...\n\n";

# Open outfiles

open(OUTFILE1,">src/grabarray.f90") or die "Can't open grabarrays.f90: $!";
open(OUTFILE2,">src/allocatearrays.f90") or die "Can't open allocatearrays.f90: $!";
open(OUTFILE3,">src/arrayflags.f90") or die "Can't open allocatearrays.f90: $!";

# Write beginning of file grabarray.f90

print OUTFILE1 "! Automatically generated file.  Do not edit!\n\n";
print OUTFILE1 "  subroutine grabarray(varname,var,iscomplex)\n\n";
print OUTFILE1 "  use param\n";
print OUTFILE1 "  use arrays\n\n";
print OUTFILE1 "  implicit none\n";
print OUTFILE1 "  logical exists,iscomplex\n";
print OUTFILE1 "  complex(8), dimension (0:Nx) :: var\n";
print OUTFILE1 "  character(len=*) varname\n\n";
print OUTFILE1 "  exists = .false.\n\n";

# Write beginning of file allocatearrays.f90

print OUTFILE2 "! Automatically generated file.  Do not edit!\n\n";
print OUTFILE2 "  subroutine allocatearrays(status)\n\n";
print OUTFILE2 "  use param\n";
print OUTFILE2 "  use arrays\n";
print OUTFILE2 "  use arrayflags\n\n";
print OUTFILE2 "  implicit none\n";
print OUTFILE2 "  logical contains,inlist\n";
print OUTFILE2 "  character(len=*) status\n\n";

# Write beginning of file arrayflags.f90

print OUTFILE3 "! Automatically generated file.  Do not edit!\n\n";
print OUTFILE3 "  module arrayflags\n\n";
print OUTFILE3 "  implicit none\n\n";

# Open infile arrays.f90

open(INFILE,"src/arrays.f90") or die "Can't open arrays.f90: $!";

# Parse file arrays.f90 to identify declared arrays

my $line = " ";
my $nline = 0;

while ($line=<INFILE>) {

   $nline = $nline+1;

#  Look for lines declaring real arrays.

   if ($line =~ /^\s+real\(8\)/) {

#      Check that all keywords are present and grab array name.
#      Make sure to ignore possible comment at the end.

       if ($line =~ /real\(8\)\s*,\s*allocatable\s*,\s*dimension\s*\(:\)\s*::\s*(\S+)\s*!?/) {
          $var = $1;

#         Check for commas

	  if ($var =~ /,/) {
             die "Bad array assignment in line ",$nline," of file arrays.f90\n\n";
	  }

#         Now write to outfile1 code to compare array name.

          print OUTFILE1  "  if (varname=='",$var,"') then\n";
          print OUTFILE1  "     exists = .true.\n";
          print OUTFILE1  "     iscomplex = .false.\n";
          print OUTFILE1  "     var = ",$var,"\n";
          print OUTFILE1  "  end if\n\n";

#         Write to outfile2 code to allocate memory.

          if ($line =~ /analysis/i) {
	     print OUTFILE2  "  if (inlist(outvars0D,\"",$var,"\").or. &\n";
             print OUTFILE2  "      inlist(outvars1D,\"",$var,"\")) then\n";
             print OUTFILE2  "     flag_",$var," = .true.\n";
             print OUTFILE2  "     if (trim(status)=='on') then\n";
             print OUTFILE2  "        allocate(",$var,"(0:Nx))\n";
             print OUTFILE2  "        ",$var," = 0.0D0\n";
             print OUTFILE2  "     else\n";
             print OUTFILE2  "        deallocate(",$var,")\n";
             print OUTFILE2  "     end if\n";
             print OUTFILE2  "  end if\n\n";
          } elsif ($line =~ /if\s+\((.*)\)/i) {
	     $cond = $1;
	     print OUTFILE2  "  if (",$cond,") then\n";
             print OUTFILE2  "     flag_",$var," = .true.\n";
             print OUTFILE2  "     if (status=='on') then\n";
             print OUTFILE2  "        allocate(",$var,"(0:Nx))\n";
             print OUTFILE2  "        ",$var," = 0.0D0\n";
             print OUTFILE2  "     else\n";
             print OUTFILE2  "        deallocate(",$var,")\n";
             print OUTFILE2  "     end if\n";
	     print OUTFILE2  "  else if (inlist(outvars0D,\"",$var,"\").or. &\n";
             print OUTFILE2  "           inlist(outvars1D,\"",$var,"\")) then\n";
             print OUTFILE2 "     print *\n";
             print OUTFILE2 "     print *, 'Error in parfile: array ",$var," has no storage,'\n";
             print OUTFILE2 "     print *, 'so no output for it is possible.'\n";
             print OUTFILE2 "     print *\n";
             print OUTFILE2 "     print *, 'Aborting! (subroutine allocatearrays.f90)'\n";
             print OUTFILE2 "     print *\n";
             print OUTFILE2 "     stop\n";
             print OUTFILE2  "  end if\n\n";
          } else {
             print OUTFILE2  "  flag_",$var," = .true.\n";
             print OUTFILE2  "  if (trim(status)=='on') then\n";
             print OUTFILE2  "     allocate(",$var,"(0:Nx))\n";
             print OUTFILE2  "     ",$var," = 0.0D0\n";
             print OUTFILE2  "  else\n";
             print OUTFILE2  "     deallocate(",$var,")\n";
             print OUTFILE2  "  end if\n\n";
	  }

#         Write to outfile3 code to declare array flags.

          print OUTFILE3  "  logical :: flag_",$var," = .false.\n";

#      Bad array assignment

       } else {
          die "Bad array assignment in line ",$nline," of file arrays.f90\n\n";
       }
   }

#  Look for lines declaring complex arrays.

   if ($line =~ /^\s+complex\(8\)/) {

#      Check that all keywords are present and grab array name.
#      Make sure to ignore possible comment at the end.

       if ($line =~ /complex\(8\)\s*,\s*allocatable\s*,\s*dimension\s*\(:\)\s*::\s*(\S+)\s*!?/) {
          $var = $1;

#         Check for commas

	  if ($var =~ /,/) {
             die "Bad array assignment in line ",$nline," of file arrays.f90\n\n";
	  }

#         Now write to outfile1 code to compare array name.

          print OUTFILE1  "  if (varname=='",$var,"') then\n";
          print OUTFILE1  "     exists = .true.\n";
          print OUTFILE1  "     iscomplex = .true.\n";
          print OUTFILE1  "     var = ",$var,"\n"; 
          print OUTFILE1  "  end if\n\n";

#         Write to outfile2 code to allocate memory.

          if ($line =~ /analysis/i) {
	     print OUTFILE2  "  if (inlist(outvars0D,\"",$var,"\").or. &\n";
             print OUTFILE2  "      inlist(outvars1D,\"",$var,"\")) then\n";
             print OUTFILE2  "     flag_",$var," = .true.\n";
             print OUTFILE2  "     if (status=='on') then\n";
             print OUTFILE2  "        allocate(",$var,"(0:Nx))\n";
             print OUTFILE2  "        ",$var," = (0.0D0,0.0D0)\n";
             print OUTFILE2  "     else\n";
             print OUTFILE2  "        deallocate(",$var,")\n";
             print OUTFILE2  "     end if\n";
             print OUTFILE2  "  end if\n\n";

          } elsif ($line =~ /if\s+\((.*)\)/i) {
	     $cond = $1;
	     print OUTFILE2  "  if (",$cond,") then\n";
             print OUTFILE2  "     flag_",$var," = .true.\n";
             print OUTFILE2  "     if (status=='on') then\n";
             print OUTFILE2  "        allocate(",$var,"(0:Nx))\n";
             print OUTFILE2  "        ",$var," = (0.0D0,0.0D0)\n";
             print OUTFILE2  "     else\n";
             print OUTFILE2  "        deallocate(",$var,")\n";
             print OUTFILE2  "     end if\n";
	     print OUTFILE2  "  else if (inlist(outvars0D,\"",$var,"\").or. &\n";
             print OUTFILE2  "           inlist(outvars1D,\"",$var,"\")) then\n";
             print OUTFILE2 "     print *\n";
             print OUTFILE2 "     print *, 'Error in parfile: array ",$var," has no storage,'\n";
             print OUTFILE2 "     print *, 'so no output for it is possible.'\n";
             print OUTFILE2 "     print *\n";
             print OUTFILE2 "     print *, 'Aborting! (subroutine allocatearrays.f90)'\n";
             print OUTFILE2 "     print *\n";
             print OUTFILE2 "     stop\n";
             print OUTFILE2  "  end if\n\n";
          } else {
             print OUTFILE2  "  flag_",$var," = .true.\n";
             print OUTFILE2  "  if (trim(status)=='on') then\n";
             print OUTFILE2  "     allocate(",$var,"(0:Nx))\n";
             print OUTFILE2  "     ",$var," = (0.0D0,0.0D0)\n";
             print OUTFILE2  "  else\n";
             print OUTFILE2  "     deallocate(",$var,")\n";
             print OUTFILE2  "  end if\n\n";
	  }

#         Write to outfile3 code to declare array flags.

          print OUTFILE3  "  logical :: flag_",$var," = .false.\n";

#      Bad array assignment

       } else {
          die "Bad array assignment in line ",$nline," of file arrays.f90\n\n";
       }
   }
}

# Close infile

close(INFILE);

# Write ending of outfile1

print OUTFILE1 "  if (.not.exists) then\n";
print OUTFILE1 "     print *\n";
print OUTFILE1 "     print *, 'Error in parfile, non-existent array: ',varname\n";
print OUTFILE1 "     print *\n";
print OUTFILE1 "     print *, 'Aborting! (subroutine grabarray.f90)'\n";
print OUTFILE1 "     print *\n";
print OUTFILE1 "     stop\n";
print OUTFILE1 "  end if\n\n";
print OUTFILE1 "  end subroutine grabarray\n";

# Write ending of outfile2

print OUTFILE2 "  end subroutine allocatearrays\n";

# Write ending of outfile3

print OUTFILE3 "\n";
print OUTFILE3 "  end module arrayflags\n";

# Close output files.

close(OUTFILE1);
close(OUTFILE2);
close(OUTFILE3);
