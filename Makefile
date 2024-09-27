#$Header: /usr/local/ollincvs/Codes/OllinToy/Makefile,v 1.14 2024/09/27 18:36:13 malcubi Exp $

#########################################
###   MAKEFILE FOR PROGRAM OLLINTOY   ###
#########################################

# Simple macro for new line (there must be two blank lines inside for it to work!).

define newline


endef

# Figure out on which machine and architecture we are.
# Notice that "uname" returns "Linux" on a linux box
# and "Darwin" on a Mac.

HOST := $(shell hostname -s)
ARCH := $(shell uname)

# Directories where make will search for files.

vpath %.pl  prl
vpath %.f90 src
vpath %.mod objs
vpath %.o objs
vpath .perl% objs

vpath ollintoy exe

# If we don't have MPI find a Fortran compiler.

ifeq ($(shell which g95 | sed -n "s:.*\/g95:g95:p"),g95)
FC := g95
else

ifeq ($(shell which gfortran | sed -n "s:.*\/gfortran:gfortran:p"),gfortran)
FC := gfortran
else

ifeq ($(shell which f95 | sed -n "s:.*\/ifc:ifc:p"),f95)
FC := f95
else

ifeq ($(shell which ifc | sed -n "s:.*\/ifc:ifc:p"),ifc)
FC := ifc
else

ifeq ($(shell which ifort | sed -n "s:.*\/ifort:ifort:p"),ifort)
FC := ifort
else

# No Fortran compiler found.

$(error $(newline)Makefile error: No FORTRAN compiler found)

# Close all the if statements.  This is ugly, but it avoids
# using "else if" that older versions of make don't allow.

endif
endif
endif
endif
endif

# Compiler and compilation flags for the g95 compiler.
#
# -O3                  Optimization.
# -ffree-form          Fortran free form format.
# -fmod=objs           Put module files in subdirectory "objs".

ifeq ($(FC),g95)
FLAGS := -O3 -ffree-form -fmod=objs
else

# Compiler and compilation flags for the gfortran compiler.
#
# -O3                  Optimization.
# -ffree-form          Fortran free form format.
# -Jobjs               Put module files in subdirectory "objs".

ifeq ($(FC),gfortran)
FLAGS := -O3 -ffree-form -Jobjs # -Wall
else

# Compiler and compilation flags for the f95 compiler.
#
# -O3                  Optimization.
# -ffree-form          Fortran free form format.
# -Jobjs               Put module files in subdirectory "objs".

ifeq ($(FC),f95)
FLAGS := -O3 -ffree-form -Jobjs
else

# Compiler and compilation flags for the old INTEL compiler "ifc".
#
# -O3                  Optimization.
# -FR                  Fortran free form format.
# -Vaxlib              Needed for system calls within Fortran.
# -i_dynamic           Dynamically look for libraries.
# -w                   Suppress warnings.
# -module objs         Put module files in subdirectory "objs".

ifeq ($(FC),ifc)
FLAGS := -O3 -FR -Vaxlib -i_dynamic -w -module objs
else

# For the current version of the INTEL compiler "ifort".
#
# -O3                  Optimization.
# -free                Fortran free form format.
# -shared-intel        Dynamically look for libraries.
# -nowarn              Suppress warnings.
# -module objs         Put module files in subdirectory "objs".

ifeq ($(FC),ifort)
FLAGS := -O3 -free -shared-intel -nowarn -module objs
else

# Unknown compiler.  If we get here it means that mpif90 uses
# a compiler that is not considered above so the flags are not
# known.

$(error $(newline)Makefile error: Unknown FORTRAN compiler: $(FC))

# Close all the if statements.  This is ugly, but it avoids
# using "else if" that older versions of make don't allow.

endif
endif
endif
endif
endif

# Now set the macro for the Fortran compiler.

FF := $(FC)

# Object files corresponding to Fortran modules. I separate
# them from the rest to be sure they are compiled first.

MODS = param.o arrays.o arrayflags.o

# Object files.  This line automatically looks for all f90 files in
# the "src" directory.  No need to add new files by hand.

FILES := $(patsubst %.f90,%.o,$(wildcard src/*.f90))

OBJS := $(notdir $(FILES))

# Add the object files corresponding to the f90 files that will be
# created by perl scripts, since they might very well not exist.

OBJS += assign.o allocatearrays.o grabarray.o
OBJS := $(filter-out $(MODS),$(sort $(OBJS)))

# Define pattern rule to compile f90 files.
#
# -I objs           Look for object files in subdirectory "objs".
# -c                Do not link, just create object file.
# -o                Name of object file follows.
#  $<               Source file (in this case %.f90)
#  $@               Target file (in this case %.o)

%.o : %.f90
	@ echo "COMPILING FILE: $(notdir $<)"
	@ $(FF) $(FLAGS) -I objs -c $< -o objs/$@
	@ echo

# By default, do nothing for targets for which no rule is specified.
# Basically, this stops make from complaining about the fact that the
# f90 files created by the perl scripts are not there to begin with.

.DEFAULT : ; @

# Main make target.  The prerequisites are done from left to right,
# and are defined below.

start :  hello dir perlscripts .timeparfiles compile link .timeend

# Hello message.

hello :
	@ touch .timestart
	@ /bin/rm -f .config; echo $(ARCH) > .config
	@ echo
	@ echo
	@ echo "******************************"
	@ echo "***   COMPILING OLLINTOY   ***"
	@ echo "******************************"
	@ echo

# Create subdirectories objs and exe.

dir :
	@ mkdir -p exe; mkdir -p objs

# Targets to run perl scripts that create files for parameter
# assignment, for dealing with arrays and for system calls.
# Notice that the scripts need to be run only if the prerequisite
# files have changed since the last time we compiled.  Since the
# make system can only compare the dates on files, I use this trick:
# I create empty files ".perl*" in the "objs" directory after running
# each perl script.  The next time we compile the code, the dates on
# these files can be compared with the dates of the prerequisite files.

perlscripts : .perl1 .perl2

.perl1 : param.f90 assign.pl src/assign.f90
	@ /bin/rm -f src/assign.f90
	@ chmod 755 prl/assign.pl
	@ prl/assign.pl
	@ touch objs/.perl1

.perl2 : arrays.f90 arrays.pl src/allocatearrays.f90 src/grabarray.f90 src/arrayflags.f90
	@ /bin/rm -f src/allocatearrays.f90
	@ /bin/rm -f src/grabarray.f90
	@ /bin/rm -f src/arrayflags.f90
	@ chmod 755 prl/arrays.pl
	@ prl/arrays.pl
	@ touch objs/.perl2

# Copy parfiles to subdirectory exe.

.timeparfiles : $(wildcard par/*.par)
	@ echo "COPYING PAR FILES TO SUBDIRECTORY EXE ..."
	@ echo
	@ cp par/*.par exe
	@ touch .timeparfiles

# Copy plotting package to subdirectory exe.

.timegraph : $(wildcard ollingraph/ollingraph)
	@ echo "COPYING PLOTTING PACKAGE OLLINGRAPH TO SUBDIRECTORY EXE ..."
	@ echo
	@ cp ollingraph/ollingraph exe
	@ touch .timegraph

# Compile and link object files.

compile : $(OBJS)

link : ollintoy

ollintoy : $(OBJS)
	@ echo
	@ echo "LINKING ..."
	@ echo
	@ echo
	cd objs; $(FF) $(FLAGS) $(MODS) $(OBJS) -o ../exe/ollintoy
	@ echo
	@ echo
	@ echo  "COMPILATION DONE!"
	@ echo
	@ echo
	@ touch .timeend

# Create object files.  Here we create all object files in one go
# using the rule defined above.  Notice that no prerequisites are
# needed apart from the default ones *.f90 defined by the compilation
# rule above.

$(MODS) :
$(OBJS) : $(MODS)

# Up to date message.  Here I make use of the same trick described
# above for the perl scripts.  To know if everything is up to date, I
# compare the date on the empty file ".timestart" which is touched as
# soon as make starts, with the date of the file ".timeend" which is
# touched only after creating the executable.  In this way, if the
# executable was not created (because it is more recent than the
# object files, which are in turn more recent that the f90 files,
# i.e. the executable is up to date), then the file ".timeend" will be
# older than the file ".timestart" and the message will be output.

.timeend : .timestart
	@ echo "EXECUTABLE IS UP TO DATE, NOTHING TO COMPILE."
	@ echo
	@ echo

# clean deletes subdirectory objs, timing files and executable file,
# but leaves everything else in subdirectory exe alone.

clean :
	@ /bin/rm -f -r objs .time*
	@ /bin/rm -f exe/ollintoy

# cleanobj deletes subdirectory objs and timing files, but leaves
# subdirectory exe alone.

cleanobj :
	@ /bin/rm -f -r objs .time*

# veryclean deletes the timing files plus the subdirectories objs and
# exe with ALL their contents!  It also deletes the f90 files created
# by the perl scripts.

veryclean :
	@ /bin/rm -f -r objs exe testresults .time*
	@ /bin/rm -f src/assign.f90 src/allocatearrays.f90 src/grabarray.f90 src/arrayflags.f90


######################
###   TEST SUITE   ###
######################

# Define empty spaces (for substitutions later).

empty:=
space:= $(empty) $(empty)

# Find names of all test directories.

DIRS := $(notdir $(patsubst %.par,%,$(wildcard testdata/*.par)))
TESTS := $(addsuffix .test,$(DIRS))

# Build list of tests.

COUNT := $(shell perl -e 'for ($$i=1;$$i<=$(words $(DIRS));$$i++){print "[$$i]. "}')

LIST := $(join $(COUNT),$(DIRS))
LIST := echo $(subst $(space),;echo ,$(LIST))
LIST := $(subst .,$(space),$(LIST))

# Rule to run tests.
# 
# This is rather pedestrian, it runs all tests sequentially, without
# asking.  It produces new output files in the directory
# "testresults", and compares them directly (using diff) with the
# files in "test".  If the files are identical, nothing
# happens. If the files differ make gets an error code which would
# normally stop it, but I ask it to ignore it (by simply putting - in
# front of the call to diff). So make keeps going but warns of the
# error and sends the output of diff to the screen.
#
# A better way of doing it would probably require a perl script.

# Define target specific variable with name of test directory.

%.test : NAME = $(notdir $(basename $<))

# Lots of string gymnastics to create failure or success message.
# This is based on a sequence of substitutions:
#
# 1.  I put the results of the diff comand into STRING1.  I also
#     append to this string "FOO".  If there were no differences
#     or missing files then the result of diff is empty, so we
#     simply have STRING1 = "FOO".
#
# 2.  STRING2 is simply equal to STRING1 with all spaces removed.
#
# 3.  To find STRING3 I check if STRING2 starts with "diff".  If it
#     does then there was a difference in the files, and all of STRING2
#     is substituted for the message "TEST FAILED".  If there was
#     no error, then STRING3 remains equal to STRING2.
#
# 4.  Now I check to see if STRING3 starts with "Only in testresults"
#     (with no spaces).  In that case there was no error but some
#     files are missing from "testdata".  STRING4 is then set to the
#     correct message.  Otherwise STRING4 remains equal to STRING3.
#
# 5.  Same as before, but now we check if the string starts with
#     "Only in testdata" (with no spaces).
#
# At the end of all this, if there was a difference, or a missing file,
# STRING5 has the corresponding error message.  If there were no
# differences or missing files, then STRING5 remains equal to "FOO".
# In that case FOO is substituted for the "TEST PASSED" message.

%.test : STRING1 = $(shell diff -rbB -x CVS testdata/$(NAME) testresults/$(NAME)) FOO
%.test : STRING2 = $(subst $(space),$(empty),$(STRING1))
%.test : STRING3 = $(STRING2:diff%=; echo TEST $(NAME) FAILED!)
%.test : STRING4 = $(STRING3:Onlyintestr%=; echo TEST PASSED?  BUT SOMETHING IS WRONG, FILES ARE MISSING IN testdata/$(NAME))
%.test : STRING5 = $(STRING4:Onlyintestd%=; echo TEST PASSED?  BUT SOMETHING IS WRONG, FILES ARE MISSING IN testresults/$(NAME))
%.test : MESSAGE = echo $(STRING5:FOO=TEST $(NAME) PASSED!)

# Now the real targets.

%.test : %.log
	@ -diff -rbB -x CVS testdata/$(NAME) testresults/$(NAME)
	@ $(MESSAGE)
	@ echo
	@ echo
	@ echo

%.log : testdata/%.par
	@ mkdir -p testdata/$(NAME)
	@ echo "RUNNING TEST: $(NAME)"
	@ echo
	@ cd testresults; ../exe/ollintoy $(notdir $<) > $@

# Main test target.  The prerequisites are done from left to right,
# and are defined below.

test : hellotest dirtest parfilestest runtests endtests

# Hello message.

hellotest :
	@ echo
	@ echo
	@ echo "*******************************"
	@ echo "***   OLLINTOY TEST SUITE   ***"
	@ echo "*******************************"
	@ echo
	@ echo
	@ echo "EXISTING TESTS:"
	@ echo
	@ $(LIST)
	@ echo
	@ echo
#	@ echo -n CHOOSE A TEST TO RUN \(a=all,q=quit\): " " 

# Create directory for comparison of results.

dirtest	:
	@ /bin/rm -rf testresults
	@ mkdir -p testresults

# Copy parameter files from test to testresults.

parfilestest :
	@ cp testdata/*par testresults

# Run the tests.

runtests : $(TESTS)

$(TESTS) :

# End.

endtests :
