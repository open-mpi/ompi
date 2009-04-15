dnl -*- shell-script -*-
dnl
dnl Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
dnl                         University Research and Technology
dnl                         Corporation.  All rights reserved.
dnl Copyright (c) 2004-2005 The University of Tennessee and The University
dnl                         of Tennessee Research Foundation.  All rights
dnl                         reserved.
dnl Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
dnl                         University of Stuttgart.  All rights reserved.
dnl Copyright (c) 2004-2005 The Regents of the University of California.
dnl                         All rights reserved.
dnl Copyright (c) 2006-2008 Cisco Systems, Inc.  All rights reserved.
dnl Copyright (c) 2007      Sun Microsystems, Inc.  All rights reserved.
dnl $COPYRIGHT$
dnl 
dnl Additional copyrights may follow
dnl 
dnl $HEADER$
dnl

AC_DEFUN([OMPI_CONFIGURE_OPTIONS],[
ompi_show_subtitle "Configuration options"

#
# Is this a developer copy?
#
if test -d .svn -o -d .hg; then
    OMPI_DEVEL=1
else
    OMPI_DEVEL=0
fi

#
# Code coverage options
#
AC_MSG_CHECKING([whether to run code coverage])
AC_ARG_ENABLE(coverage,
              AC_HELP_STRING([--enable-coverage],
                             [enable code coverage files to be generated]))
if test "$enable_coverage" = "yes"; then
    if test "$enable_shared" = "yes"; then 
        AC_MSG_WARN([Code coverage can run only with static libraries. Please 
run configure with --enable-static --disable-shared if 
you want code coverage. Also ensure that you execute 
make clean too ensure removal of all leftover shared 
mpi libraries])
        AC_MSG_ERROR([Cannot continue processing])
    fi
    AC_MSG_RESULT([yes])
    WANT_COVERAGE=1
else 
    AC_MSG_RESULT([no])
    WANT_COVERAGE=0
fi


#
# Branch Probabilities options
#
AC_MSG_CHECKING([whether to compile with branch probabilities])
AC_ARG_ENABLE(branch-probabilities,
              AC_HELP_STRING([--enable-branch-probabilities],
                             [enable profile arcs and branch probability optimization]))
if test "$enable_branch_probabilities" = "yes"; then
    AC_MSG_RESULT([yes])
    WANT_BRANCH_PROBABILITIES=1
else 
    AC_MSG_RESULT([no])
    WANT_BRANCH_PROBABILITIES=0
fi


#
# Memory debugging
#

AC_MSG_CHECKING([whether to debug memory usage])
AC_ARG_ENABLE(mem-debug, 
    AC_HELP_STRING([--enable-mem-debug],
                   [enable memory debugging (debugging only) (default: disabled)]))
if test "$enable_mem_debug" = "yes"; then
    AC_MSG_RESULT([yes])
    WANT_MEM_DEBUG=1
else
    AC_MSG_RESULT([no])
    WANT_MEM_DEBUG=0
fi
#################### Early development override ####################
if test "$WANT_MEM_DEBUG" = "0" -a -z "$enable_mem_debug" -a "$OMPI_DEVEL" = 1; then
    WANT_MEM_DEBUG=1
    echo "--> developer override: enable mem profiling by default"
fi
#################### Early development override ####################
AC_DEFINE_UNQUOTED(OMPI_ENABLE_MEM_DEBUG, $WANT_MEM_DEBUG,
    [Whether we want the memory profiling or not])

#
# Memory profiling
#

AC_MSG_CHECKING([whether to profile memory usage])
AC_ARG_ENABLE(mem-profile, 
    AC_HELP_STRING([--enable-mem-profile],
                   [enable memory profiling (debugging only) (default: disabled)]))
if test "$enable_mem_profile" = "yes"; then
    AC_MSG_RESULT([yes])
    WANT_MEM_PROFILE=1
else
    AC_MSG_RESULT([no])
    WANT_MEM_PROFILE=0
fi
#################### Early development override ####################
if test "$WANT_MEM_PROFILE" = "0" -a -z "$enable_mem_profile" -a "$OMPI_DEVEL" = 1; then
    WANT_MEM_PROFILE=1
    echo "--> developer override: enable mem profiling by default"
fi
#################### Early development override ####################
AC_DEFINE_UNQUOTED(OMPI_ENABLE_MEM_PROFILE, $WANT_MEM_PROFILE,
    [Whether we want the memory profiling or not])

#
# Developer picky compiler options
#

AC_MSG_CHECKING([if want developer-level compiler pickyness])
AC_ARG_ENABLE(picky, 
    AC_HELP_STRING([--enable-picky],
                   [enable developer-level compiler pickyness when building Open MPI (default: disabled)]))
if test "$enable_picky" = "yes"; then
    AC_MSG_RESULT([yes])
    WANT_PICKY_COMPILER=1
else
    AC_MSG_RESULT([no])
    WANT_PICKY_COMPILER=0
fi
#################### Early development override ####################
if test "$WANT_PICKY_COMPILER" = "0" -a -z "$enable_picky" -a "$OMPI_DEVEL" = 1; then
    WANT_PICKY_COMPILER=1
    echo "--> developer override: enable picky compiler by default"
fi
#################### Early development override ####################

#
# Developer debugging
#

AC_MSG_CHECKING([if want developer-level debugging code])
AC_ARG_ENABLE(debug, 
    AC_HELP_STRING([--enable-debug],
                   [enable developer-level debugging code (not for general MPI users!) (default: disabled)]))
if test "$enable_debug" = "yes"; then
    AC_MSG_RESULT([yes])
    WANT_DEBUG=1
else
    AC_MSG_RESULT([no])
    WANT_DEBUG=0
fi
#################### Early development override ####################
if test "$WANT_DEBUG" = "0" -a -z "$enable_debug" -a "$OMPI_DEVEL" = 1; then
    WANT_DEBUG=1
    echo "--> developer override: enable debugging code by default"
fi
#################### Early development override ####################
if test "$WANT_DEBUG" = "0"; then
    CFLAGS="-DNDEBUG $CFLAGS"
    CXXFLAGS="-DNDEBUG $CXXFLAGS"
fi
AC_DEFINE_UNQUOTED(OMPI_ENABLE_DEBUG, $WANT_DEBUG,
    [Whether we want developer-level debugging code or not])

AC_ARG_ENABLE(debug-symbols,
    AC_HELP_STRING([--disable-debug-symbols],
        [Disable adding compiler flags to enable debugging symbols if --enable-debug is specified.  For non-debugging builds, this flag has no effect.]))

#
# Sparse Groups
#

AC_MSG_CHECKING([if want sparse process groups])
AC_ARG_ENABLE(sparse-groups, 
    AC_HELP_STRING([--enable-sparse-groups],
                   [enable sparse process groups (default: not enabled)]))
if test "$enable_sparse_groups" = "yes"; then
    AC_MSG_RESULT([yes])
    GROUP_SPARSE=1
else
    AC_MSG_RESULT([no])
    GROUP_SPARSE=0
fi
AC_DEFINE_UNQUOTED([OMPI_GROUP_SPARSE],$GROUP_SPARSE,
    [Wether we want sparse process groups])

#
# Fortran 77
#

AC_MSG_CHECKING([if want Fortran 77 bindings])
AC_ARG_ENABLE(mpi-f77, 
    AC_HELP_STRING([--enable-mpi-f77],
                   [enable f77 MPI bindings (default: enabled)]))
if test "$enable_mpi_f77" != "no"; then
    AC_MSG_RESULT([yes])
    OMPI_WANT_F77_BINDINGS=1
else
    AC_MSG_RESULT([no])
    OMPI_WANT_F77_BINDINGS=0
fi


#
# Fortran 90
#

AC_MSG_CHECKING([if want Fortran 90 bindings])
AC_ARG_ENABLE(mpi-f90, 
    AC_HELP_STRING([--enable-mpi-f90],
                   [enable f90 MPI bindings (default: enabled)]))
if test "$enable_mpi_f90" != "no"; then
    AC_MSG_RESULT([yes])
    OMPI_WANT_F90_BINDINGS=1
else
    AC_MSG_RESULT([no])
    OMPI_WANT_F90_BINDINGS=0
fi

AC_MSG_CHECKING([desired Fortran 90 bindings "size"])
AC_ARG_WITH(mpi-f90-size,
    AC_HELP_STRING([--with-mpi-f90-size=SIZE],
                   [specify the types of functions in the Fortran 90 MPI module, where SIZE is one of: trivial (MPI-2 F90-specific functions only), small (trivial + all MPI functions without choice buffers), medium (small + all MPI functions with one choice buffer), large (medium + all MPI functions with 2 choice buffers, but only when both buffers are the same type).  Default SIZE is "small".]))

if test "$OMPI_WANT_F90_BINDINGS" = "0"; then
    AC_MSG_RESULT([disabled (Fortran 90 bindings disabled)])
elif test "$with_mpi_f90_size" = "no"; then
    OMPI_WANT_F90_BINDINGS=0
    AC_MSG_RESULT([disabling F90 MPI module (used specified)])
else
    # Default value
    if test "$with_mpi_f90_size" = ""; then
        with_mpi_f90_size=small
    fi

    # Check for each of the sizes
    if test "$with_mpi_f90_size" = "trivial"; then
        OMPI_F90_BUILD_SIZE=trivial
    elif test "$with_mpi_f90_size" = "small"; then
        OMPI_F90_BUILD_SIZE=small
    elif test "$with_mpi_f90_size" = "medium"; then
        OMPI_F90_BUILD_SIZE=medium
    elif test "$with_mpi_f90_size" = "large"; then
        OMPI_F90_BUILD_SIZE=large
    else
        AC_MSG_RESULT([Unrecognized size: $with_mpi_f90_size])
        AC_MSG_ERROR([Cannot continue])
    fi
fi

AM_CONDITIONAL([OMPI_WANT_BUILD_F90_TRIVIAL], 
               [test "$OMPI_F90_BUILD_SIZE" = "trivial"])
AM_CONDITIONAL([OMPI_WANT_BUILD_F90_SMALL], 
               [test "$OMPI_F90_BUILD_SIZE" = "small"])
AM_CONDITIONAL([OMPI_WANT_BUILD_F90_MEDIUM], 
               [test "$OMPI_F90_BUILD_SIZE" = "medium"])
AM_CONDITIONAL([OMPI_WANT_BUILD_F90_LARGE], 
               [test "$OMPI_F90_BUILD_SIZE" = "large"])

AC_SUBST(OMPI_F90_BUILD_SIZE)
AC_MSG_RESULT([$OMPI_F90_BUILD_SIZE])

#
# MPI profiling
#

AC_MSG_CHECKING([whether to enable PMPI])
AC_ARG_ENABLE(mpi-profile, 
    AC_HELP_STRING([--enable-mpi-profile],
                   [enable MPI profiling (default: enabled)]))
if test "$enable_mpi_profile" != "no"; then
    AC_MSG_RESULT([yes])
    WANT_MPI_PROFILING=1
    MPIF_H_PMPI_W_FUNCS=", PMPI_WTICK, PMPI_WTIME"
else
    AC_MSG_RESULT([no])
    WANT_MPI_PROFILING=0
    MPIF_H_PMPI_W_FUNCS=
fi
AC_SUBST(MPIF_H_PMPI_W_FUNCS)


#                                                                              
# C++                                                                          
#                                                                              

AC_MSG_CHECKING([if want C++ bindings])
AC_ARG_ENABLE(mpi-cxx,
    AC_HELP_STRING([--enable-mpi-cxx],
                   [enable C++ MPI bindings (default: enabled)]))
if test "$enable_mpi_cxx" != "no"; then
    AC_MSG_RESULT([yes])
    WANT_MPI_CXX_SUPPORT=1
else
    AC_MSG_RESULT([no])
    WANT_MPI_CXX_SUPPORT=0
fi

AC_MSG_CHECKING([if want MPI::SEEK_SET support])
AC_ARG_ENABLE([mpi-cxx-seek],
    [AC_HELP_STRING([--enable-mpi-cxx-seek],
                   [enable support for MPI::SEEK_SET, MPI::SEEK_END, and MPI::SEEK_POS in C++ bindings (default: enabled)])])
if test "$enable_mpi_cxx_seek" != "no" ; then
  AC_MSG_RESULT([yes])
  OMPI_WANT_MPI_CXX_SEEK=1
else
  AC_MSG_RESULT([no])
  OMPI_WANT_MPI_CXX_SEEK=0
fi
AC_DEFINE_UNQUOTED([OMPI_WANT_MPI_CXX_SEEK], [$OMPI_WANT_MPI_CXX_SEEK],
    [do we want to try to work around C++ bindings SEEK_* issue?])

#
# Do we want to disable weak symbols for some reason?
#

AC_MSG_CHECKING([if want to enable weak symbol support])
AC_ARG_ENABLE(weak-symbols,
    AC_HELP_STRING([--enable-weak-symbols],
                   [use weak symbols, if available (default: enabled)]))
if test "$enable_weak_symbols" != "no"; then
    AC_MSG_RESULT([yes])
    WANT_WEAK_SYMBOLS=1
else
    AC_MSG_RESULT([no])
    WANT_WEAK_SYMBOLS=0
fi

#
# Do we want to disable MPI parameter checking at run-time?
#

AC_MSG_CHECKING([if want run-time MPI parameter checking])
AC_ARG_WITH(mpi-param-check,
    AC_HELP_STRING([--with-mpi-param-check(=VALUE)],
                   [behavior of MPI function parameter checking.  Valid values are: always, never, runtime.  If --with-mpi-param-check is specified with no VALUE argument, it is equivalent to a VALUE of "always"; --without-mpi-param-check is equivalent to "never" (default: runtime).]))
mpi_param_check=ompi_mpi_param_check
if test "$with_mpi_param_check" = "no" -o \
    "$with_mpi_param_check" = "never"; then
    mpi_param_check=0
    AC_MSG_RESULT([never])
elif test "$with_mpi_param_check" = "yes" -o \
    "$with_mpi_param_check" = "always"; then
    mpi_param_check=1
    AC_MSG_RESULT([always])
elif test "$with_mpi_param_check" = "runtime" -o \
    -z "$with_mpi_params_check"; then
    AC_MSG_RESULT([runtime])
else
    AC_MSG_RESULT([unknown])
    AC_MSG_WARN([*** Unrecognized --with-mpi-param-check value])
    AC_MSG_WARN([*** See "configure --help" output])
    AC_MSG_WARN([*** Defaulting to "runtime"])
fi
AC_DEFINE_UNQUOTED(MPI_PARAM_CHECK, $mpi_param_check,
    [Whether we want to check MPI parameters always, never, or decide at run-time])


#
# Do we want to install all of OMPI's header files?
#

AC_MSG_CHECKING([if want to install OMPI header files])
AC_ARG_WITH(devel-headers,
    AC_HELP_STRING([--with-devel-headers],
                   [normal MPI users/applications do not need this (mpi.h and mpif.h are ALWAYS installed).  Developer headers are only necessary for MCA module authors (default: disabled).]))
if test "$with_devel_headers" = "yes"; then
    AC_MSG_RESULT([yes])
    WANT_INSTALL_HEADERS=1
else
    AC_MSG_RESULT([no])
    WANT_INSTALL_HEADERS=0
fi
AM_CONDITIONAL(WANT_INSTALL_HEADERS, test "$WANT_INSTALL_HEADERS" = 1)

#
# Do we want the pretty-print stack trace feature?
#
AC_MSG_CHECKING([if want pretty-print stacktrace])
AC_ARG_ENABLE([pretty-print-stacktrace],
    [AC_HELP_STRING([--enable-pretty-print-stacktrace],
                    [Pretty print stacktrace on process signal (default: enabled)])])
if test "$enable_pretty_print_stacktrace" = "no" ; then
    AC_MSG_RESULT([no])
    WANT_PRETTY_PRINT_STACKTRACE=0
else
    AC_MSG_RESULT([yes])
    WANT_PRETTY_PRINT_STACKTRACE=1
fi
AC_DEFINE_UNQUOTED([OMPI_WANT_PRETTY_PRINT_STACKTRACE],
                   [$WANT_PRETTY_PRINT_STACKTRACE],
                   [if want pretty-print stack trace feature])

#
# Do we want to enable peruse interface?
#
 
AC_MSG_CHECKING([if peruse support is required])
AC_ARG_ENABLE(peruse,
    AC_HELP_STRING([--enable-peruse],
                   [Support PERUSE interface (default: disabled)]))
if test "$enable_peruse" = "yes"; then
    AC_MSG_RESULT([yes])
    WANT_PERUSE=1
else
    AC_MSG_RESULT([no])
    WANT_PERUSE=0
fi
AC_DEFINE_UNQUOTED([OMPI_WANT_PERUSE],
                   [$WANT_PERUSE],
                   [if the peruse interface should be enabled])
AM_CONDITIONAL(WANT_PERUSE, test "$WANT_PERUSE" = "1")

#
# What is the max array rank that we want to support in the f90 bindings?
#

OMPI_FORTRAN_MAX_ARRAY_RANK=4
AC_MSG_CHECKING([max supported array dimension in F90 MPI bindings])
AC_ARG_WITH(f90-max-array-dim,
    AC_HELP_STRING([--with-f90-max-array-dim=<DIM>],
                   [The maximum array dimension supported in the F90 MPI bindings (default: $OMPI_FORTRAN_MAX_ARRAY_RANK).]))
if test ! -z "$with_f90_max_array_dim" -a "$with_f90_max_array_dim" != "no"; then
    # Ensure it's a number (hopefully an integer!), and >=1 and <=7
    expr $with_f90_max_array_dim + 1 > /dev/null 2> /dev/null
    happy=1
    AS_IF([test "$?" != "0"], [happy=0],
          [expr $with_f90_max_array_dim \>= 1 \& $with_f90_max_array_dim \<= 7 > /dev/null 2>/dev/null
           AS_IF([test "$?" != "0"], [happy=0])])

    # If badness in the above tests, bail
    AS_IF([test "$happy" = "0"],
          [AC_MSG_RESULT([bad value ($with_f90_max_array_dim)])
           AC_MSG_WARN([--with-f90-max-array-dim value must be >=1 and <=7])
           AC_MSG_ERROR([Cannot continue])])
    OMPI_FORTRAN_MAX_ARRAY_RANK="$with_f90_max_array_dim"
fi
AC_MSG_RESULT([$OMPI_FORTRAN_MAX_ARRAY_RANK])
AC_SUBST(OMPI_FORTRAN_MAX_ARRAY_RANK)

# do we want PTY support?
AC_MSG_CHECKING([if pty support should be enabled])
AC_ARG_ENABLE(pty-support,
    AC_HELP_STRING([--enable-pty-support],
                   [Enable/disable PTY support for STDIO forwarding.  (default: enabled)]))
if test "$enable_pty_support" = "no" ; then
    AC_MSG_RESULT([no])
    OMPI_ENABLE_PTY_SUPPORT=0
else
    AC_MSG_RESULT([yes])
    OMPI_ENABLE_PTY_SUPPORT=1
fi
AC_DEFINE_UNQUOTED([OMPI_ENABLE_PTY_SUPPORT], [$OMPI_ENABLE_PTY_SUPPORT],
                   [Whether user wants PTY support or not])

#
# Do we want to allow DLOPEN?
#
AC_MSG_CHECKING([if user wants dlopen support])
AC_ARG_ENABLE([dlopen],
    [AC_HELP_STRING([--enable-dlopen],
                    [Whether build should attempt to use dlopen (or
                     similar) to dynamically load components.
                     Disabling dlopen implies --disable-mca-dso.
                     (default: enabled)])])
if test "$enable_dlopen" = "no" ; then
    enable_mca_dso="no"
    enable_mca_static="yes"
    OMPI_ENABLE_DLOPEN_SUPPORT=0
    AC_MSG_RESULT([no])
else
    OMPI_ENABLE_DLOPEN_SUPPORT=1
    AC_MSG_RESULT([yes])
fi

#
# Heterogeneous support
#

AC_MSG_CHECKING([if heterogeneous support should be enabled])
AC_ARG_ENABLE([heterogeneous],
    [AC_HELP_STRING([--enable-heterogeneous],
                    [Enable features required for heterogeneous
                     platform support (default: disabled)])])
if test "$enable_heterogeneous" = "yes" ; then
     AC_MSG_RESULT([yes])
     ompi_want_heterogeneous=1
else
     AC_MSG_RESULT([no])
     ompi_want_heterogeneous=0
fi
AC_DEFINE_UNQUOTED([OMPI_ENABLE_HETEROGENEOUS_SUPPORT], 
                   [$ompi_want_heterogeneous], 
                   [Enable features required for heterogeneous support])

#
# Internal trace file logging (debugging)
#

AC_MSG_CHECKING([if want trace file debugging])
AC_ARG_ENABLE([trace],
    [AC_HELP_STRING([--enable-trace],
                    [Enable internal tracing of OMPI/ORTE/OPAL calls -- used only for developer debugging, not tracing of MPI applications (default: disabled)])])
if test "$enable_trace" = "yes"; then
    AC_MSG_RESULT([yes])
    opal_want_trace=1
else
    AC_MSG_RESULT([no])
    opal_want_trace=0
fi
AC_DEFINE_UNQUOTED([OPAL_ENABLE_TRACE], [$opal_want_trace],
                   [Enable run-time tracing of internal functions])


#
# Minimal RTE support
#
AC_MSG_CHECKING([if want full RTE support])
AC_ARG_WITH([rte-support],
    [AC_HELP_STRING([--without-rte-support],
                    [Build without RTE support for systems that do not require it (default: full RTE support built)])])
if test "$with_rte_support" = "no"; then
    AC_MSG_RESULT([no])
    orte_without_full_support=1
    list_of_frameworks="errmgr,ess-singleton,ess-hnp,ess-tool,ess-env,filem,grpcomm-basic,grpcomm-bad,iof,odls,oob,plm,ras,rmaps,rml,routed,snapc,btl-sm,coll-sm,common-sm,mpool-sm,dpm-orte,pubsub-orte"
    if test -z $enable_mca_no_build ; then
      enable_mca_no_build="$list_of_frameworks"
    else
      enable_mca_no_build="$enable_mca_no_build,$list_of_frameworks"
    fi
else
    AC_MSG_RESULT([yes])
    orte_without_full_support=0
fi
AC_DEFINE_UNQUOTED([ORTE_DISABLE_FULL_SUPPORT], [$orte_without_full_support],
                   [Build full RTE support])
AM_CONDITIONAL(ORTE_DISABLE_FULL_SUPPORT, test "$with_rte_support" = "no")

#
# Cross-compile data
#
AC_ARG_WITH([cross],
    [AC_HELP_STRING([--with-cross=FILE],
        [Specify configure values that can not be determined in a cross-compilation environment.  See the Open MPI FAQ.])])
if test "$with_cross" = "yes" ; then
    AC_MSG_ERROR([--with-cross argument must include FILE option])
elif test "$with_cross" = "no" ; then
    AC_MSG_ERROR([--without-cross is not a valid argument])
elif test "$with_cross" != "" ; then
    if test ! -r $with_cross ; then
        AC_MSG_ERROR([could not find cross-compile data file $with_cross])
    fi

    # eval into environment
    OMPI_LOG_MSG([Loading cross-compile file $with_cross, with contents below])
    OMPI_LOG_FILE([$with_cross])
    . "$with_cross"
fi

#
# --with-ft=TYPE
#  TYPE:
#    - LAM (synonym for 'cr' currently)
#    - cr
#  /* General FT sections */
#  #if OPAL_ENABLE_FT == 0 /* FT Disabled globaly */
#  #if OPAL_ENABLE_FT == 1 /* FT Enabled globaly */
#  /* CR Specific sections */
#  #if OPAL_ENABLE_FT_CR == 0 /* FT Ckpt/Restart Disabled */
#  #if OPAL_ENABLE_FT_CR == 1 /* FT Ckpt/Restart Enabled */
#
AC_MSG_CHECKING([if want fault tolerance])
AC_ARG_WITH(ft,
    [AC_HELP_STRING([--with-ft=TYPE],
            [Specify the type of fault tolerance to enable. Options: LAM (LAM/MPI-like), cr (Checkpoint/Restart) (default: disabled)])],
        [ompi_want_ft=1],
        [ompi_want_ft=0])
if test "$with_ft" = "no" -o "$ompi_want_ft" = "0"; then
    ompi_want_ft=0
    ompi_want_ft_cr=0
    AC_MSG_RESULT([Disabled fault tolerance])
else
    ompi_want_ft=1
    ompi_want_ft_cr=0
    ompi_want_ft_type=none

    # Default value
    if test "$with_ft" = "" -o "$with_ft" = "yes"; then
        ompi_want_ft_type=cr
        ompi_want_ft_cr=1
    elif test "$with_ft" = "LAM"; then
        ompi_want_ft_type=lam
        ompi_want_ft_cr=1
    elif test "$with_ft" = "lam"; then
        ompi_want_ft_type=lam
        ompi_want_ft_cr=1
    elif test "$with_ft" = "CR"; then
        ompi_want_ft_type=cr
        ompi_want_ft_cr=1
    elif test "$with_ft" = "cr"; then
        ompi_want_ft_type=cr
        ompi_want_ft_cr=1
    else
        AC_MSG_RESULT([Unrecognized FT TYPE: $with_ft])
        AC_MSG_ERROR([Cannot continue])
    fi
    AC_MSG_RESULT([Enabled $with_ft ($ompi_want_ft_type)])
    AC_MSG_WARN([**************************************************])
    AC_MSG_WARN([*** Fault Tolerance Integration into Open MPI is *])
    AC_MSG_WARN([*** a research quality implementation, and care  *])
    AC_MSG_WARN([*** should be used when choosing to enable it.   *])
    AC_MSG_WARN([**************************************************])
fi
AC_DEFINE_UNQUOTED([OPAL_ENABLE_FT], [$ompi_want_ft],
                   [Enable fault tolerance general components and logic])
AC_DEFINE_UNQUOTED([OPAL_ENABLE_FT_CR], [$ompi_want_ft_cr],
                   [Enable fault tolerance checkpoint/restart components and logic])
AM_CONDITIONAL(WANT_FT, test "$ompi_want_ft" = "1")

#
# Do we want to install binaries?
#
AC_ARG_ENABLE([binaries],
    [AC_HELP_STRING([--enable-binaries],
        [Build and install binaries required for Open MPI, such as the wrapper compilers.   Useful for multi-lib installations.  (default: enabled)])])
AM_CONDITIONAL([OMPI_INSTALL_BINARIES], [test "$enable_binaries" != "no"])

AC_ARG_ENABLE([script-wrapper-compilers],
  [AC_HELP_STRING([--enable-script-wrapper-compilers],
     [Use less featured script-based wrapper compilers instead of the standard C-based wrapper compilers.  This option ignores the --disable-binaries option and is mainly useful in cross-compile environments])])
AM_CONDITIONAL([OMPI_WANT_SCRIPT_WRAPPER_COMPILERS],
    [test "$enable_script_wrapper_compilers" = "yes"])
AC_CONFIG_FILES([ompi/tools/wrappers/ompi_wrapper_script], [chmod +x ompi/tools/wrappers/ompi_wrapper_script])

#
# Support per-user config files?
#
AC_ARG_ENABLE([per-user-config-files],
   [AC_HELP_STRING([--enable-per-user-config-files],
      [Disable per-user configuration files, to save disk accesses during job strart-up.  This is likely desirable for large jobs.  Note that this can also be acheived by environment variables at run-time.  (default: enabled)])])
if test "$enable_per_user_config_files" = "no" ; then
  result=0
else
  result=1
fi
AC_DEFINE_UNQUOTED([OMPI_WANT_HOME_CONFIG_FILES], [$result],
     [Enable per-user config files])

#
# Do we want to disable IPv6 support?
#
AC_MSG_CHECKING([if want IPv6 support])
AC_ARG_ENABLE([ipv6],
    [AC_HELP_STRING([--disable-ipv6],
        [Disable IPv6 support (default: enabled, but only if the underlying system supports it)])])
if test "$enable_ipv6" = "no"; then
    AC_MSG_RESULT([no])
    opal_want_ipv6=0
else
    AC_MSG_RESULT([yes (if underlying system supports it)])
    opal_want_ipv6=1
fi
AC_DEFINE_UNQUOTED([OPAL_ENABLE_IPV6], [$opal_want_ipv6],
                   [Enable IPv6 support, but only if the underlying system supports it])

#
# Do we want orterun's --prefix behavior to be enabled by default?
#
AC_MSG_CHECKING([if want orterun "--prefix" behavior to be enabled by default])
AC_ARG_ENABLE([orterun-prefix-by-default],
    [AC_HELP_STRING([--enable-orterun-prefix-by-default],
        [Make "orterun ..." behave exactly the same as "orterun --prefix \$prefix" (where \$prefix is the value given to --prefix in configure)])])
AC_ARG_ENABLE([mpirun-prefix-by-default],
    [AC_HELP_STRING([--enable-mpirun-prefix-by-default],
        [Synonym for --enable-orterun-prefix-by-default])])
if test "$enable_orterun_prefix_by_default" = ""; then
    enable_orterun_prefix_by_default=$enable_mpirun_prefix_by_default
fi
if test "$enable_orterun_prefix_by_default" = "yes"; then
    AC_MSG_RESULT([yes])
    orte_want_orterun_prefix_by_default=1
else
    AC_MSG_RESULT([no])
    orte_want_orterun_prefix_by_default=0
fi
AC_DEFINE_UNQUOTED([ORTE_WANT_ORTERUN_PREFIX_BY_DEFAULT],
                   [$orte_want_orterun_prefix_by_default],
                   [Whether we want orterun to effect "--prefix $prefix" by default])

#
# Package/brand string
#
AC_MSG_CHECKING([for package/brand string])
AC_ARG_WITH([package-string],
     [AC_HELP_STRING([--with-package-string=STRING],
                     [Use a branding string throughout Open MPI])])
if test "$with_package_string" = "" -o "$with_package_string" = "no"; then
    with_package_string="Open MPI $OMPI_CONFIGURE_USER@$OMPI_CONFIGURE_HOST Distribution"
fi
AC_DEFINE_UNQUOTED([OPAL_PACKAGE_STRING], ["$with_package_string"],
     [package/branding string for Open MPI])
AC_MSG_RESULT([$with_package_string])

#
# Ident string
#
AC_MSG_CHECKING([for ident string])
AC_ARG_WITH([ident-string],
     [AC_HELP_STRING([--with-ident-string=STRING],
                     [Embed an ident string into Open MPI object files])])
if test "$with_ident_string" = "" -o "$with_ident_string" = "no"; then
    with_ident_string="%VERSION%"
fi
with_ident_string="`echo $with_ident_string | sed -e 's/%VERSION%/'$OMPI_VERSION/`"
AC_DEFINE_UNQUOTED([OPAL_IDENT_STRING], ["$with_ident_string"],
     [ident string for Open MPI])
AC_MSG_RESULT([$with_ident_string])


#
# Add padding to OpenIB header
#
AC_MSG_CHECKING([whether to add padding to the openib control header])
AC_ARG_WITH([openib-control-hdr-padding],
     [AC_HELP_STRING([--with-openib-control-hdr-padding],
                     [Add padding bytes to the openib control header])])
if test "$with_openib_control_hdr_padding" = "yes"; then
    AC_MSG_RESULT([yes])
    ompi_openib_pad_hdr=1
else
    AC_MSG_RESULT([no])
    ompi_openib_pad_hdr=0
fi
AC_DEFINE_UNQUOTED([OMPI_OPENIB_PAD_HDR],
                   [$ompi_openib_pad_hdr],
                   [Add padding bytes to the openib control header])


#
# Use alternative checksum algorithm
#
AC_MSG_CHECKING([whether to use an alternative checksum algo for messages])
AC_ARG_WITH([dst-checksum],
     [AC_HELP_STRING([--with-dst-checksum],
                     [Use an alternative checksum algorithm for messages])])
if test "$with_dst_checksum" = "yes"; then
    AC_MSG_RESULT([yes])
    CFLAGS="-DOMPI_CSUM_DST $CFLAGS"
else
    AC_MSG_RESULT([no])
fi

])
