dnl
dnl/*D 
dnl PAC_LIB_MPI - Check for MPI library
dnl
dnl Synopsis:
dnl PAC_LIB_MPI([action if found],[action if not found])
dnl
dnl Output Effect:
dnl
dnl Notes:
dnl Currently, only checks for lib mpi and mpi.h.  Later, we will add
dnl MPI_Pcontrol prototype (const int or not?).  
dnl
dnl Prerequisites:
dnl autoconf version 2.13 (for AC_SEARCH_LIBS)
dnl D*/
dnl Other tests to add:
dnl Version of MPI
dnl MPI-2 I/O?
dnl MPI-2 Spawn?
dnl MPI-2 RMA?
dnl PAC_LIB_MPI([found text],[not found text])
AC_DEFUN([PAC_LIB_MPI],[
dnl Set the prereq to 2.50 to avoid having 
AC_PREREQ(2.50)
if test "X$pac_lib_mpi_is_building" != "Xyes" ; then
  # Use CC if TESTCC is defined
  if test "X$pac_save_level" != "X" ; then
     pac_save_TESTCC="${TESTCC}"
     pac_save_TESTCPP="${TESTCPP}"
     CC="$pac_save_CC"
     if test "X$pac_save_CPP" != "X" ; then
         CPP="$pac_save_CPP"
     fi
  fi
  # Look for MPILIB first if it is defined
  AC_SEARCH_LIBS(MPI_Init,$MPILIB mpi mpich)
  if test "$ac_cv_search_MPI_Init" = "no" ; then
    ifelse($2,,
    AC_MSG_ERROR([Could not find MPI library]),[$2])
  fi
  AC_CHECK_HEADER(mpi.h,pac_have_mpi_h="yes",pac_have_mpi_h="no")
  if test $pac_have_mpi_h = "no" ; then
    ifelse($2,,
    AC_MSG_ERROR([Could not find mpi.h include file]),[$2])
  fi
  if test "X$pac_save_level" != "X" ; then
     CC="$pac_save_TESTCC"
     CPP="$pac_save_TESTCPP"
  fi
fi
ifelse($1,,,[$1])
])

dnl This should also set MPIRUN.
dnl
dnl/*D
dnl PAC_ARG_MPI_TYPES - Add command-line switches for different MPI 
dnl environments
dnl
dnl Synopsis:
dnl PAC_ARG_MPI_TYPES([default])
dnl
dnl Output Effects:
dnl Adds the following command line options to configure
dnl+ \-\-with\-mpich[=path] - MPICH.  'path' is the location of MPICH commands
dnl. \-\-with\-ibmmpi - IBM MPI
dnl. \-\-with\-lammpi[=path] - LAM/MPI
dnl. \-\-with\-mpichnt - MPICH NT
dnl- \-\-with\-sgimpi - SGI MPI
dnl If no type is selected, and a default ("mpich", "ibmmpi", or "sgimpi")
dnl is given, that type is used as if '--with-<default>' was given.
dnl
dnl Sets 'CC', 'F77', 'TESTCC', 'TESTF77', and 'MPILIBNAME'.  Does `not`
dnl perform an AC_SUBST for these values.
dnl Also sets 'MPIBOOT' and 'MPIUNBOOT'.  These are used to specify 
dnl programs that may need to be run before and after running MPI programs.
dnl For example, 'MPIBOOT' may start demons necessary to run MPI programs and
dnl 'MPIUNBOOT' will stop those demons.
dnl 
dnl The two forms of the compilers are to allow for tests of the compiler
dnl when the MPI version of the compiler creates executables that cannot
dnl be run on the local system (for example, the IBM SP, where executables
dnl created with mpcc will not run locally, but executables created
dnl with xlc may be used to discover properties of the compiler, such as
dnl the size of data types).
dnl
dnl Historical note:
dnl Some common autoconf tests, such as AC_CHECK_SIZEOF, used to require
dnl running a program.  But some MPI compilers (often really compilation 
dnl scripts) produced programs that could only be run with special commands,
dnl such as a batch submission system.  To allow these test programs to be
dnl run, a separate set of compiler variables, TESTCC, TESTF77, etc., 
dnl were defined.  However, in later versions of autoconf, it both became
dnl unnecessary to run programs for tests such as AC_CHECK_SIZEOF and 
dnl it became necessary to define CC etc. before invoking AC_PROG_CC (and
dnl the othe language compilers), because those commands now do much, much
dnl more than just determining the compiler.
dnl
dnl To address the change, we still define the TESTCC etc. compilers where
dnl possible to allow the use of AC_TRY_RUN when required, but we define
dnl the CC etc variables and do not define ac_cv_prog_CC etc., as these 
dnl cause autoconf to skip all of the other initialization code that 
dnl AC_PROG_CC etc. runs.  Note also that this command must occur before 
dnl AC_PROG_CC (or anything that might cause AC_PROG_CC to be invoked).
dnl
dnl See also:
dnl PAC_LANG_PUSH_COMPILERS, PAC_LIB_MPI
dnl D*/
AC_DEFUN([PAC_ARG_MPI_TYPES],[
# known types
PAC_ARG_MPI_KNOWN_TYPES
# find compilers
PAC_MPI_FIND_COMPILER_SCRIPTS
PAC_MPI_FIND_COMPILERS
# check for MPI library
PAC_MPI_CHECK_MPI_LIB
])
dnl
dnl To keep autoconf from prematurely invoking the compiler check scripts,
dnl we need a command that first sets the compilers and a separate one
dnl that makes any necessary checks for libraries
dnl
AC_DEFUN([PAC_ARG_MPI_KNOWN_TYPES],[
AC_ARG_WITH(mpich,
[--with-mpich=path  - Assume that we are building with MPICH],
ac_mpi_type=mpich)
# Allow MPICH as well as MPICH
AC_ARG_WITH(mpich,
[--with-mpich=path  - Assume that we are building with MPICH],
ac_mpi_type=mpich)
AC_ARG_WITH(lammpi,
[--with-lammpi=path  - Assume that we are building with LAM/MPI],
ac_mpi_type=lammpi)
AC_ARG_WITH(ibmmpi,
[--with-ibmmpi    - Use the IBM SP implementation of MPI],
ac_mpi_type=ibmmpi)
AC_ARG_WITH(sgimpi,
[--with-sgimpi    - Use the SGI implementation of MPI],
ac_mpi_type=sgimpi)
AC_ARG_WITH(mpichnt,
[--with-mpichnt - Use MPICH for Windows NT ],
ac_mpi_type=mpichnt)
AC_ARG_WITH(mpi,
[--with-mpi=path    - Use an MPI implementation with compile scripts mpicc
                     and mpif77 in path/bin],ac_mpi_type=generic)

if test "X$ac_mpi_type" = "X" ; then
    if test "X$1" != "X" ; then
        ac_mpi_type=$1
    else
        ac_mpi_type=unknown
    fi
fi
if test "$ac_mpi_type" = "unknown" -a "$pac_lib_mpi_is_building" = "yes" ; then
    ac_mpi_type="mpich"
fi
])
dnl
dnl Because autoconf insists on moving code to the beginning of
dnl certain definitions, it is *not possible* to define a single command
dnl that selects compilation scripts and also check for other options.
dnl Thus, this needs to be divided into 
dnl   MPI_FIND_COMPILER_SCRIPTS
dnl which can fail (i.e., not find a script), and
dnl   MPI_FIND_COMPILERS
dnl which runs the various PROC_xx for the compilers.
dnl WARNING: this function ignores --program-suffix and --program-prefix.
dnl However, this function is not currently used at all.
AC_DEFUN([PAC_MPI_FIND_COMPILER_SCRIPTS],[
# Set defaults
MPIRUN_NP="-np "
MPIEXEC_N="-n "
AC_SUBST(MPIRUN_NP)
AC_SUBST(MPIEXEC_N)
dnl
AC_ARG_VAR([MPIEXEC],[Name and path of mpiexec program])
AC_ARG_VAR([MPIRUN],[Name and path of mpirun program])
AC_ARG_VAR([MPIBOOT],[Name and path of program to run before mpirun])
AC_ARG_VAR([MPIUNBOOT],[Name and path of program to run after all mpirun])
AC_ARG_VAR([MPICC],[Name and absolute path of program used to compile MPI programs in C])
AC_ARG_VAR([MPIF77],[Name and absolute path of program used to compile MPI programs in F77])
AC_ARG_VAR([MPICXX],[Name and absolute path of program used to compile MPI programs in C++])
AC_ARG_VAR([MPIF90],[Name and absolute path of program used to compile MPI programs in F90])
#
# Check for things that will cause trouble.  For example, 
# if MPICC is defined but does not contain a / or \, then PATH_PROG will
# ignore the value
if test -n "$MPICC" ; then
   case $MPICC in
changequote(<<,>>)
    [\\/]* | ?:[\\/]*)
changequote([,])
    # Ok, PATH_PROG will figure it out
    ;;  
  *)
    AC_MSG_ERROR([MPICC must be set to an absolute path if it is set])
  esac
fi
if test -n "$MPICXX" ; then
   case $MPICXX in
changequote(<<,>>)
    [\\/]* | ?:[\\/]*)
changequote([,])
    # Ok, PATH_PROG will figure it out
    ;;  
  *)
    AC_MSG_ERROR([MPICXX must be set to an absolute path if it is set])
  esac
fi
if test -n "$MPIF77" ; then
   case $MPIF77 in
changequote(<<,>>)
    [\\/]* | ?:[\\/]*)
changequote([,])
    # Ok, PATH_PROG will figure it out
    ;;  
  *)
    AC_MSG_ERROR([MPIF77 must be set to an absolute path if it is set])
  esac
fi
if test -n "$MPIF90" ; then
   case $MPIF90 in
changequote(<<,>>)
    [\\/]* | ?:[\\/]*)
changequote([,])
    # Ok, PATH_PROG will figure it out
    ;;  
  *)
    AC_MSG_ERROR([MPIF90 must be set to an absolute path if it is set])
  esac
fi

case $ac_mpi_type in
	mpich)
        dnl 
        dnl This isn't correct.  It should try to get the underlying compiler
        dnl from the mpicc and mpif77 scripts or mpireconfig
        if test "X$pac_lib_mpi_is_building" != "Xyes" ; then
	    PAC_PUSH_FLAG([PATH])
            if test "$with_mpich" != "yes" -a "$with_mpich" != "no" ; then 
		# Look for commands; if not found, try adding bin to the
		# path
		if test ! -x $with_mpich/mpicc -a -x $with_mpich/bin/mpicc ; then
			with_mpich="$with_mpich/bin"
		fi
                PATH=$with_mpich:${PATH}
            fi
            AC_PATH_PROG(MPICC,mpicc)
	    if test -z "$TESTCC" ; then TESTCC=${CC-cc} ; fi
            CC="$MPICC"
	    # Note that autoconf may unconditionally change the value of 
	    # CC (!) in some other command. Thus, we define CCMASTER
	    CCMASTER=$CC
	    # Force autoconf to respect this choice
	    ac_ct_CC=$CC
	    # to permit configure codes to recover the correct CC.  This
	    # is an ugly not-quite-correct workaround for the fact that 
	    # does not want you to change the C compiler once you have set it
	    # (But since it does so unconditionally, it silently creates 
	    # bogus output files.)
            AC_PATH_PROG(MPIF77,mpif77)
	    if test -z "$TESTF77" ; then TESTF77=${F77-f77} ; fi
            F77="$MPIF77"
            AC_PATH_PROG(MPIFC,mpif90)
	    if test -z "$TESTFC" ; then TESTFC=${FC-f90} ; fi 
            FC="$MPIFC"
            AC_PATH_PROG(MPICXX,mpiCC)
	    if test -z "$TESTCXX" ; then TESTCXX=${CXX-CC} ; fi
            CXX="$MPICXX"
	    # We may want to restrict this to the path containing mpirun
	    AC_PATH_PROG(MPIEXEC,mpiexec)
	    AC_PATH_PROG(MPIRUN,mpirun)
	    AC_PATH_PROG(MPIBOOT,mpichboot)
	    AC_PATH_PROG(MPIUNBOOT,mpichstop)
	    PAC_POP_FLAG([PATH])
  	    MPILIBNAME="mpich"
        else 
	    # All of the above should have been passed in the environment!
	    :
        fi
	;;

        mpichnt)
        ;;

	lammpi)
	dnl
        dnl This isn't correct.  It should try to get the underlying compiler
        dnl from the mpicc and mpif77 scripts or mpireconfig
	PAC_PUSH_FLAG([PATH])
        if test "$with_mpich" != "yes" -a "$with_mpich" != "no" ; then 
	    # Look for commands; if not found, try adding bin to the path
		if test ! -x $with_lammpi/mpicc -a -x $with_lammpi/bin/mpicc ; then
			with_lammpi="$with_lammpi/bin"
		fi
                PATH=$with_lammpi:${PATH}
        fi
        AC_PATH_PROG(MPICC,mpicc)
        if test -z "$TESTCC" ; then TESTCC=${CC-cc} ; fi
        CC="$MPICC"
        AC_PATH_PROG(MPIF77,mpif77)
	if test -z "$TESTCC" ; then TESTF77=${F77-f77} ; fi
        F77="$MPIF77"
        AC_PATH_PROG(MPIFC,mpif90)
        TESTFC=${FC-f90}
	if test -z "$TESTFC" ; then TESTFC=${FC-f90} ; fi
        FC="$MPIFC"
        AC_PATH_PROG(MPICXX,mpiCC)
	if test -z "$TESTCXX" ; then TESTCXX=${CXX-CC} ; fi
        CXX="$MPICXX"
	PAC_POP_FLAG([PATH])
  	MPILIBNAME="lammpi"
	MPIBOOT="lamboot"
	MPIUNBOOT="wipe"
	MPIRUN="mpirun"
	;;

	ibmmpi)
	AC_CHECK_PROGS(MPCC,mpcc)
	AC_CHECK_PROGS(MPXLF,mpxlf mpfort)
	if test -z "$MPCC" -o -z "$MPXLF" ; then
	    AC_MSG_ERROR([Could not find IBM MPI compilation scripts.  Either mpcc or mpxlf/mpfort is missing])
	fi
	if test -z "$TESTCC" ; then TESTCC=${CC-xlC} ; fi
	if test -z "$TESTF77" ; then TESTF77=${F77-xlf}; fi
	CC=mpcc; F77=$MPXLF
	# There is no mpxlf90, but the options langlvl and free can
	# select the Fortran 90 version of xlf
	if test "$enable_f90" != no ; then
	    AC_CHECK_PROGS(MPIXLF90,mpxlf90 mpfort)
	    if test -z "$TESTFC" ; then TESTFC=${FC-xlf90}; fi
            if test "X$MPIXLF90" != "X" ; then 
	        FC="$MPIXLF90"
	    else
	    	FC="$MPXLF -qlanglvl=90ext -qfree=f90"
	    fi
	fi
	MPILIBNAME=""
	cross_compiling=yes
	# Turn off the autoconf version 3 warning message
	ac_tool_warned=yes
	;;

	sgimpi)
	if test -z "$TESTCC" ; then TESTCC=${CC:=cc} ; fi
	if test -z "$TESTF77" ; then TESTF77=${F77:=f77} ; fi
	if test -z "$TESTCXX" ; then TESTCXX=${CXX:=CC} ; fi
	if test -z "$TESTFC" ; then TESTFC=${FC:=f90} ; fi
	# Must check for the MPI library in a separate macro - adding
	# a test here will cause autoconf to prematurely define the
	# C compiler
	MPIRUN=mpirun
	MPIBOOT=""
	MPIUNBOOT=""
	;;

	generic)
	# in $with_mpi/bin or $with_mpi
        if test "X$MPICC" = "X" ; then
            if test -x "$with_mpi/bin/mpicc" ; then
                MPICC=$with_mpi/bin/mpicc
	    elif test -x "$with_mpi/mpicc" ; then
	        MPICC=$with_mpi/mpicc
            fi
        fi
        if test "X$MPICXX" = "X" ; then
            if test -x "$with_mpi/bin/mpicxx" ; then
                MPICXX=$with_mpi/bin/mpicxx
	    elif test -x "$with_mpi/mpicxx" ; then
	        MPICXX=$with_mpi/mpicxx
            fi
        fi
        if test "X$MPIF77" = "X" ; then
            if test -x "$with_mpi/bin/mpif77" ; then
                MPIF77=$with_mpi/bin/mpif77
	    elif test -x "$with_mpi/mpif77" ; then
	        MPIF77=$with_mpi/mpif77
            fi
        fi
        if test "X$MPIF90" = "X" ; then
            if test -x "$with_mpi/bin/mpif90" ; then
                MPIF90=$with_mpi/bin/mpif90
	    elif test -x "$with_mpi/mpif90" ; then
	        MPIF90=$with_mpi/mpif90
            fi
        fi
        if test "X$MPIEXEC" = "X" ; then
            if test -x "$with_mpi/bin/mpiexec" ; then
                MPIEXEC=$with_mpi/bin/mpiexec
	    elif test -x "$with_mpi/mpiexec" ; then
	        MPIEXEC=$with_mpi/mpiexec
            fi
        fi
        CC=$MPICC
        F77=$MPIF77
	if test "X$MPICXX" != "X" ; then CXX=$MPICXX ; fi
	if test "X$MPIF90" != "X" ; then F90=$MPIF90 ; fi
	;;

	*)
	# Use the default choices for the compilers
	;;
esac
])

AC_DEFUN([PAC_MPI_FIND_COMPILERS],[
# Tell autoconf to determine properties of the compilers (these are the 
# compilers for MPI programs)
PAC_PROG_CC
if test "$enable_f77" != no -a "$enable_fortran" != no ; then
    AC_PROG_F77
fi
if test "$enable_cxx" != no ; then
    AC_PROG_CXX
fi
if test "$enable_f90" != no ; then
    PAC_PROG_FC
fi
])

dnl
dnl This uses the selected CC etc to check for include paths and libraries
AC_DEFUN([PAC_MPI_CHECK_MPI_LIB],[
AC_REQUIRE([AC_PROG_CC])
case $ac_mpi_type in
    mpich)
	;;

    mpichnt)
        dnl
        dnl This isn't adequate, but it helps with using MPICH-NT/SDK.gcc
	PAC_PUSH_FLAG([CFLAGS])
        CFLAGS="$CFLAGS -I$with_mpichnt/include"
	PAC_PUSH_FLAG([CPPFLAGS])
        CPPFLAGS="$CPPFLAGS -I$with_mpichnt/include"
	PAC_PUSH_FLAG([LDFLAGS])
        LDFLAGS="$LDFLAGS -L$with_mpichnt/lib"
        AC_CHECK_LIB(mpich,MPI_Init,found="yes",found="no")
        if test "$found" = "no" ; then
          AC_CHECK_LIB(mpich,MPI_Init,found="yes",found="no")
        fi
	if test "$enable_cxx" != no ; then
	    AC_PROG_CXX
	fi
	if test "$enable_f90" != no ; then
	    PAC_PROG_FC
	fi
	# Set defaults for the TEST versions if not already set
	if test -z "$TESTCC" ; then TESTCC=${CC:=cc} ; fi
	if test -z "$TESTF77" ; then TESTF77=${F77:=f77} ; fi
	if test -z "$TESTCXX" ; then TESTCXX=${CXX:=CC} ; fi
	if test -z "$TESTFC" ; then TESTFC=${FC:=f90} ; fi
        if test "$found" = "no" ; then
	  PAC_POP_FLAG([CFLAGS])
	  PAC_POP_FLAG([CPPFLAGS])
	  PAC_POP_FLAG([LDFLAGS])
        fi
        ;;

    lammpi)
	;;

    ibmmpi)
	;;

    sgimpi)
	AC_CHECK_LIB(mpi,MPI_Init)
	if test "$ac_cv_lib_mpi_MPI_Init" = "yes" ; then
	    MPILIBNAME="mpi"
	fi	
	;;

    generic)
	AC_SEARCH_LIBS(MPI_Init,mpi mpich mpich)
	if test "$ac_cv_lib_mpi_MPI_Init" = "yes" ; then
	    MPILIBNAME="mpi"
	fi	
	;;

    *)
	;;
esac
])

dnl
dnl/*D
dnl PAC_MPI_F2C - Determine if MPI has the MPI-2 functions MPI_xxx_f2c and
dnl   MPI_xxx_c2f
dnl
dnl Output Effect:
dnl Define 'HAVE_MPI_F2C' if the routines are found.
dnl
dnl Notes:
dnl Looks only for 'MPI_Request_c2f'.
dnl D*/
AC_DEFUN([PAC_MPI_F2C],[
AC_CACHE_CHECK([for MPI F2C and C2F routines],
pac_cv_mpi_f2c,
[
AC_TRY_LINK([#include "mpi.h"],
[MPI_Request request;MPI_Fint a;a = MPI_Request_c2f(request);],
pac_cv_mpi_f2c="yes",pac_cv_mpi_f2c="no")
])
if test "$pac_cv_mpi_f2c" = "yes" ; then 
    AC_DEFINE(HAVE_MPI_F2C,1,[Define if MPI has F2C]) 
fi
])
dnl
dnl/*D
dnl PAC_HAVE_ROMIO - make mpi.h include mpio.h if romio enabled
dnl
dnl Output Effect:
dnl expands @HAVE_ROMIO@ in mpi.h into #include "mpio.h"
dnl D*/
AC_DEFUN([PAC_HAVE_ROMIO],[
if test "$enable_romio" = "yes" ; then HAVE_ROMIO='#include "mpio.h"'; fi
AC_SUBST(HAVE_ROMIO)
])
