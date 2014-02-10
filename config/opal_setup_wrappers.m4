dnl -*- shell-script -*-
dnl
dnl Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
dnl                         University Research and Technology
dnl                         Corporation.  All rights reserved.
dnl Copyright (c) 2004-2005 The University of Tennessee and The University
dnl                         of Tennessee Research Foundation.  All rights
dnl                         reserved.
dnl Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
dnl                         University of Stuttgart.  All rights reserved.
dnl Copyright (c) 2004-2005 The Regents of the University of California.
dnl                         All rights reserved.
dnl Copyright (c) 2006-2010 Oracle and/or its affiliates.  All rights reserved.
dnl Copyright (c) 2009-2013 Cisco Systems, Inc.  All rights reserved.
dnl $COPYRIGHT$
dnl 
dnl Additional copyrights may follow
dnl 
dnl $HEADER$
dnl

# OPAL_WRAPPER_FLAGS_ADD(variable, new_argument)
# ----------------------------------------------
# Add new_argument to the list of arguments for variable in the
# wrapper compilers, if it's not already there.  For example:
#   OPAL_WRAPPER_FLAGS_ADD(CFLAGS, "-pthread")
# will add -pthread to the list of CFLAGS the wrappers use when invoked.
#
# This macro MAY NOT be invoked from configure macros for MCA components.
# See the comment in SETUP_WRAPPER_INIT (below) for more information.
AC_DEFUN([OPAL_WRAPPER_FLAGS_ADD], [
    m4_ifdef([mca_component_configure_active],
        [m4_fatal([OPAL_WRAPPER_FLAGS_ADD can not be called from a component configure])])
    m4_if([$1], [CPPFLAGS], [OPAL_FLAGS_APPEND_UNIQ([wrapper_extra_cppflags], [$2])],
          [$1], [CFLAGS], [OPAL_FLAGS_APPEND_UNIQ([wrapper_extra_cflags], [$2])],
          [$1], [CXXFLAGS], [OPAL_FLAGS_APPEND_UNIQ([wrapper_extra_cxxflags], [$2])],
          [$1], [FCFLAGS], [OPAL_FLAGS_APPEND_UNIQ([wrapper_extra_fcflags], [$2])],
          [$1], [LDFLAGS], [OPAL_FLAGS_APPEND_UNIQ([wrapper_extra_ldflags], [$2])],
          [$1], [LIBS], [OPAL_FLAGS_APPEND_UNIQ([wrapper_extra_libs], [$2])],
          [m4_fatal([Unknown wrapper flag type $1])])
])


# OPAL_SETUP_WRAPPER_INIT()
# -------------------------
# Setup wrapper compiler configuration information.  Should be called early to 
# prevent lots of calculations and then an abort for a silly user typo.  This 
# macro works in pair with OPAL_SETUP_WRAPPER_FINAL, which should be called
# almost at the end of configure (after the last call to OPAL_WRAPPER_FLAGS_ADD
# and after the MCA system has been setup).
#
# The wrapper compiler arguments are a little fragile and should NOT
# be edited by configure directly.  Instead, main configure should use
# OPAL_WRAPPER_FLAGS_ADD.
#
# When building statically, the MCA system will add
# <framework>_<component>_WRAPPER_EXTRA_{LDFLAGS, LIBS} if set and try
# to add <framework>_<component>_{LDFLAGS, LIBS} (if not an external
# configure) to the wrapper LDFLAGS and LIBS.  Any arguments in
# <framework>_<component>_WRAPPER_EXTRA_CPPFLAGS are passed to the
# wrapper compilers IF AND ONLY IF the framework was a STOP_AT_FIRST
# framework, the component is a static component, and devel headers
# are installed.  Note that MCA components are ONLY allowed to
# (indirectly) influence the wrapper CPPFLAGS, LDFLAGS, and LIBS.
# That is, a component may not influence CFLAGS, CXXFLAGS, or FCFLAGS.
#
# Notes:
#   * Keep user flags seperate as 1) they should have no influence
#     over build and 2) they don't go through the uniqification we do
#     with the other wrapper compiler options
#   * While the user (the person who runs configure) is allowed to set
#     <flag>_prefix, configure is not.  There's no known use case for
#     doing so, and we'd like to force the issue.
AC_DEFUN([OPAL_SETUP_WRAPPER_INIT],[
    AC_ARG_WITH([wrapper-cflags], 
                [AC_HELP_STRING([--with-wrapper-cflags],
                                [Extra flags to add to CFLAGS when using mpicc])])
    AS_IF([test "$with_wrapper_cflags" = "yes" -o "$with_wrapper_cflags" = "no"],
          [AC_MSG_ERROR([--with-wrapper-cflags must have an argument.])])

    AC_ARG_WITH([wrapper-cflags-prefix], 
                [AC_HELP_STRING([--with-wrapper-cflags-prefix],
                                [Extra flags (before user flags) to add to CFLAGS when using mpicc])])
    AS_IF([test "$with_wrapper_cflags_prefix" = "yes" -o "$with_wrapper_cflags_prefix" = "no"],
          [AC_MSG_ERROR([--with-wrapper-cflags-prefix must have an argument.])])

    AC_ARG_WITH([wrapper-cxxflags], 
        [AC_HELP_STRING([--with-wrapper-cxxflags],
                        [Extra flags to add to CXXFLAGS when using mpiCC/mpic++])])
    AS_IF([test "$with_wrapper_cxxflags" = "yes" -o "$with_wrapper_cxxflags" = "no"],
          [AC_MSG_ERROR([--with-wrapper-cxxflags must have an argument.])])

    AC_ARG_WITH([wrapper-cxxflags-prefix], 
        [AC_HELP_STRING([--with-wrapper-cxxflags-prefix],
                        [Extra flags to add to CXXFLAGS when using mpiCC/mpic++])])
    AS_IF([test "$with_wrapper_cxxflags_prefix" = "yes" -o "$with_wrapper_cxxflags_prefix" = "no"],
          [AC_MSG_ERROR([--with-wrapper-cxxflags-prefix must have an argument.])])

    m4_ifdef([project_ompi], [
            AC_ARG_WITH([wrapper-fcflags], 
                [AC_HELP_STRING([--with-wrapper-fcflags],
                        [Extra flags to add to FCFLAGS when using mpifort])])
            AS_IF([test "$with_wrapper_fcflags" = "yes" -o "$with_wrapper_fcflags" = "no"],
                [AC_MSG_ERROR([--with-wrapper-fcflags must have an argument.])])

            AC_ARG_WITH([wrapper-fcflags-prefix], 
                [AC_HELP_STRING([--with-wrapper-fcflags-prefix],
                        [Extra flags (before user flags) to add to FCFLAGS when using mpifort])])
            AS_IF([test "$with_wrapper_fcflags_prefix" = "yes" -o "$with_wrapper_fcflags_prefix" = "no"],
                [AC_MSG_ERROR([--with-wrapper-fcflags-prefix must have an argument.])])])

    AC_ARG_WITH([wrapper-ldflags], 
                [AC_HELP_STRING([--with-wrapper-ldflags],
                                [Extra flags to add to LDFLAGS when using wrapper compilers])])
    AS_IF([test "$with_wrapper_ldflags" = "yes" -o "$with_wrapper_ldflags" = "no"],
          [AC_MSG_ERROR([--with-wrapper-ldflags must have an argument.])])

    AC_ARG_WITH([wrapper-libs], 
                [AC_HELP_STRING([--with-wrapper-libs],
                                [Extra flags to add to LIBS when using wrapper compilers])])
    AS_IF([test "$with_wrapper_libs" = "yes" -o "$with_wrapper_libs" = "no"],
          [AC_MSG_ERROR([--with-wrapper-libs must have an argument.])])

    AC_MSG_CHECKING([if want wrapper compiler rpath support])
    AC_ARG_ENABLE([wrapper-rpath],
                  [AS_HELP_STRING([--enable-wrapper-rpath],
                  [enable rpath support in the wrapper compilers (default=no)])])
    AS_IF([test "$enable_wrapper_rpath" != "no"], [enable_wrapper_rpath=yes])
    AC_MSG_RESULT([$enable_wrapper_rpath])
])

# Check to see whether the linker supports DT_RPATH.  We'll need to
# use config.rpath to find the flags that it needs, if it does (see
# comments in config.rpath for an explanation of where it came from).
AC_DEFUN([OPAL_SETUP_RPATH],[
    OPAL_VAR_SCOPE_PUSH([rpath_libdir_save rpath_script rpath_outfile])
    AC_MSG_CHECKING([if linker supports RPATH])
    # Output goes into globally-visible $rpath_args.  Run this in a
    # sub-process so that we don't pollute the current process
    # environment.
    rpath_script=conftest.$$.sh
    rpath_outfile=conftest.$$.out
    rm -f $rpath_script $rpath_outfile
    cat > $rpath_script <<EOF
#!/bin/sh

# Slurp in the libtool config into my environment

# Apparently, "libtoool --config" calls "exit", so we can't source it
# (because if script A sources script B, and B calls "exit", then both
# B and A will exit).  Instead, we have to send the output to a file
# and then source that.
$OMPI_TOP_BUILDDIR/libtool --config > $rpath_outfile

chmod +x $rpath_outfile
. ./$rpath_outfile
rm -f $rpath_outfile

# Evaluate \$hardcode_libdir_flag_spec, and substitute in LIBDIR for \$libdir
libdir=LIBDIR
flags="\`eval echo \$hardcode_libdir_flag_spec\`"
echo \$flags

# Done
exit 0
EOF
    chmod +x $rpath_script
    rpath_args=`./$rpath_script`
    rm -f $rpath_script

    AS_IF([test -n "$rpath_args"],
          [WRAPPER_RPATH_SUPPORT=rpath
           AC_MSG_RESULT([yes ($rpath_args)])],
          [WRAPPER_RPATH_SUPPORT=unnecessary
           AC_MSG_RESULT([yes (no extra flags needed)])])

    OPAL_VAR_SCOPE_POP

    # If we found RPATH support, check for RUNPATH support, too
    AS_IF([test "$WRAPPER_RPATH_SUPPORT" = "rpath"],
          [OPAL_SETUP_RUNPATH])
])

# Check to see if the linker supports the DT_RUNPATH flags via
# --enable-new-dtags (a GNU ld-specific option).  These flags are more
# social than DT_RPATH -- they can be overridden by LD_LIBRARY_PATH
# (where a regular DT_RPATH cannot).  
#
# If DT_RUNPATH is supported, then we'll use *both* the RPATH and
# RUNPATH flags in the LDFLAGS.
AC_DEFUN([OPAL_SETUP_RUNPATH],[
    OPAL_VAR_SCOPE_PUSH([LDFLAGS_save])

    AC_MSG_CHECKING([if linker supports RUNPATH])
    # Set the output in $runpath_args
    runpath_args=
    LDFLAGS_save=$LDFLAGS
    LDFLAGS="$LDFLAGS -Wl,--enable-new-dtags"
    AC_LANG_PUSH([C])
    AC_LINK_IFELSE([AC_LANG_PROGRAM([], [return 7;])],
                   [WRAPPER_RPATH_SUPPORT=runpath
                    runpath_args="-Wl,--enable-new-dtags"
                    AC_MSG_RESULT([yes (-Wl,--enable-new-dtags)])],
                   [AC_MSG_RESULT([no])])
    AC_LANG_POP([C])
    LDFLAGS=$LDFLAGS_save

    OPAL_VAR_SCOPE_POP
])

# Called to find all -L arguments in the LDFLAGS and add in RPATH args
# for each of them.  Then also add in an RPATH for @{libdir} (which
# will be replaced by the wrapper compile to the installdir libdir at
# runtime), and the RUNPATH args, if we have them.
AC_DEFUN([RPATHIFY_LDFLAGS],[
    OPAL_VAR_SCOPE_PUSH([rpath_out rpath_dir rpath_tmp])
    AS_IF([test "$enable_wrapper_rpath" = "yes" && test "$WRAPPER_RPATH_SUPPORT" != "disabled" && test "$WRAPPER_RPATH_SUPPORT" != "unnecessary"], [
           rpath_out=""
           for val in ${$1}; do
               case $val in
               -L*)
                   rpath_dir=`echo $val | cut -c3-`
                   rpath_tmp=`echo $rpath_args | sed -e s@LIBDIR@$rpath_dir@`
                   rpath_out="$rpath_out $rpath_tmp"
                   ;;
               esac
           done

           # Now add in the RPATH args for @{libdir}, and the RUNPATH args
           rpath_tmp=`echo $rpath_args | sed -e s/LIBDIR/@{libdir}/`
           $1="${$1} $rpath_out $rpath_tmp $runpath_args"
          ])
    OPAL_VAR_SCOPE_POP
])


# OPAL_SETUP_WRAPPER_FINAL()
# ---------------------------
AC_DEFUN([OPAL_SETUP_WRAPPER_FINAL],[

    # Setup RPATH support, if desired
    WRAPPER_RPATH_SUPPORT=disabled
    AS_IF([test "$enable_wrapper_rpath" = "yes"],
          [OPAL_SETUP_RPATH])
    AS_IF([test "$enable_wrapper_rpath" = "yes" -a "$WRAPPER_RPATH_SUPPORT" = "disabled"],
          [AC_MSG_WARN([RPATH support requested but not available])
           AC_MSG_ERROR([Cannot continue])])

    # We now have all relevant flags.  Substitute them in everywhere.
    m4_ifdef([project_opal], [
       AC_MSG_CHECKING([for OPAL CPPFLAGS])
       OPAL_WRAPPER_EXTRA_CPPFLAGS="$opal_mca_wrapper_extra_cppflags $wrapper_extra_cppflags $with_wrapper_cppflags"
       AC_SUBST([OPAL_WRAPPER_EXTRA_CPPFLAGS])
       AC_MSG_RESULT([$OPAL_WRAPPER_EXTRA_CPPFLAGS])

       AC_MSG_CHECKING([for OPAL CFLAGS])
       OPAL_WRAPPER_EXTRA_CFLAGS="$wrapper_extra_cflags $with_wrapper_cflags"
       AC_SUBST([OPAL_WRAPPER_EXTRA_CFLAGS])
       AC_MSG_RESULT([$OPAL_WRAPPER_EXTRA_CFLAGS])

       AC_MSG_CHECKING([for OPAL CFLAGS_PREFIX])
       OPAL_WRAPPER_EXTRA_CFLAGS_PREFIX="$with_wrapper_cflags_prefix"
       AC_SUBST([OPAL_WRAPPER_EXTRA_CFLAGS_PREFIX])
       AC_MSG_RESULT([$OPAL_WRAPPER_EXTRA_CFLAGS_PREFIX])

       AC_MSG_CHECKING([for OPAL CXXFLAGS])
       OPAL_WRAPPER_EXTRA_CXXFLAGS="$wrapper_extra_cxxflags $with_wrapper_cxxflags"
       AC_SUBST([OPAL_WRAPPER_EXTRA_CXXFLAGS])
       AC_MSG_RESULT([$OPAL_WRAPPER_EXTRA_CXXFLAGS])

       AC_MSG_CHECKING([for OPAL CXXFLAGS_PREFIX])
       OPAL_WRAPPER_EXTRA_CXXFLAGS_PREFIX="$with_wrapper_cxxflags_prefix"
       AC_SUBST([OPAL_WRAPPER_EXTRA_CXXFLAGS_PREFIX])
       AC_MSG_RESULT([$OPAL_WRAPPER_EXTRA_CXXFLAGS_PREFIX])

       AC_MSG_CHECKING([for OPAL LDFLAGS])
       OPAL_WRAPPER_EXTRA_LDFLAGS="$opal_mca_wrapper_extra_ldflags $wrapper_extra_ldflags $with_wrapper_ldflags"
       RPATHIFY_LDFLAGS([OPAL_WRAPPER_EXTRA_LDFLAGS])
       AC_SUBST([OPAL_WRAPPER_EXTRA_LDFLAGS])
       AC_MSG_RESULT([$OPAL_WRAPPER_EXTRA_LDFLAGS])

       # wrapper_extra_libs doesn't really get populated until after the mca system runs
       # since most of the libs come from libtool.  So this is the first time we can
       # uniq them.  ROMIO in particular adds lots of things already in wrapper_extra_libs,
       # and this cleans the duplication up a bunch.  Always add everything the user
       # asked for, as they know better than us.
       AC_MSG_CHECKING([for OPAL LIBS])
       OPAL_WRAPPER_EXTRA_LIBS="$opal_mca_wrapper_extra_libs"
       OPAL_FLAGS_APPEND_UNIQ([OPAL_WRAPPER_EXTRA_LIBS], [$wrapper_extra_libs])
       OPAL_WRAPPER_EXTRA_LIBS="$OPAL_WRAPPER_EXTRA_LIBS $with_wrapper_libs"
       AC_SUBST([OPAL_WRAPPER_EXTRA_LIBS])
       AC_MSG_RESULT([$OPAL_WRAPPER_EXTRA_LIBS])

       AC_MSG_CHECKING([for OPAL extra include dirs])
       if test "$WANT_INSTALL_HEADERS" = "1" ; then
           OPAL_WRAPPER_EXTRA_INCLUDES="openmpi"
       else
           OPAL_WRAPPER_EXTRA_INCLUDES=
       fi
       AC_SUBST([OPAL_WRAPPER_EXTRA_INCLUDES])
       AC_MSG_RESULT([$OPAL_WRAPPER_EXTRA_INCLUDES])
    ])

    m4_ifdef([project_orte], [
       AC_MSG_CHECKING([for ORTE CPPFLAGS])
       ORTE_WRAPPER_EXTRA_CPPFLAGS="$orte_mca_wrapper_extra_cppflags $wrapper_extra_cppflags $with_wrapper_cppflags"
       AC_SUBST([ORTE_WRAPPER_EXTRA_CPPFLAGS])
       AC_MSG_RESULT([$ORTE_WRAPPER_EXTRA_CPPFLAGS])

       AC_MSG_CHECKING([for ORTE CFLAGS])
       ORTE_WRAPPER_EXTRA_CFLAGS="$wrapper_extra_cflags $with_wrapper_cflags"
       AC_SUBST([ORTE_WRAPPER_EXTRA_CFLAGS])
       AC_MSG_RESULT([$ORTE_WRAPPER_EXTRA_CFLAGS])

       AC_MSG_CHECKING([for ORTE CFLAGS_PREFIX])
       ORTE_WRAPPER_EXTRA_CFLAGS_PREFIX="$with_wrapper_cflags_prefix"
       AC_SUBST([ORTE_WRAPPER_EXTRA_CFLAGS_PREFIX])
       AC_MSG_RESULT([$ORTE_WRAPPER_EXTRA_CFLAGS_PREFIX])

       AC_MSG_CHECKING([for ORTE LDFLAGS])
       ORTE_WRAPPER_EXTRA_LDFLAGS="$orte_mca_wrapper_extra_ldflags $wrapper_extra_ldflags $with_wrapper_ldflags"
       RPATHIFY_LDFLAGS([ORTE_WRAPPER_EXTRA_LDFLAGS])
       AC_SUBST([ORTE_WRAPPER_EXTRA_LDFLAGS])
       AC_MSG_RESULT([$ORTE_WRAPPER_EXTRA_LDFLAGS])

       AC_MSG_CHECKING([for ORTE LIBS])
       ORTE_WRAPPER_EXTRA_LIBS="$orte_mca_wrapper_extra_libs"
       OPAL_FLAGS_APPEND_UNIQ([ORTE_WRAPPER_EXTRA_LIBS], [$wrapper_extra_libs])
       ORTE_WRAPPER_EXTRA_LIBS="$ORTE_WRAPPER_EXTRA_LIBS $with_wrapper_libs"
       AC_SUBST([ORTE_WRAPPER_EXTRA_LIBS])
       AC_MSG_RESULT([$ORTE_WRAPPER_EXTRA_LIBS])

       AC_MSG_CHECKING([for ORTE extra include dirs])
       if test "$WANT_INSTALL_HEADERS" = "1" ; then
           ORTE_WRAPPER_EXTRA_INCLUDES="openmpi"
       else
           ORTE_WRAPPER_EXTRA_INCLUDES=
       fi
       AC_SUBST([ORTE_WRAPPER_EXTRA_INCLUDES])
       AC_MSG_RESULT([$ORTE_WRAPPER_EXTRA_INCLUDES])

       # For script-based wrappers that don't do relocatable binaries.
       # Don't use if you don't have to.
       exec_prefix_save="${exec_prefix}"
       test "x$exec_prefix" = xNONE && exec_prefix="${prefix}"
       eval "ORTE_WRAPPER_INCLUDEDIR=\"${includedir}\""
       eval "ORTE_WRAPPER_LIBDIR=\"${libdir}\""
       exec_prefix="${exec_prefix_save}"
       AC_SUBST([ORTE_WRAPPER_INCLUDEDIR])
       AC_SUBST([ORTE_WRAPPER_LIBDIR])

       # if wrapper compilers were requested, set the orte one up
       if test "$WANT_SCRIPT_WRAPPER_COMPILERS" = "1" ; then
         AC_CONFIG_FILES([orte/tools/wrappers/orte_wrapper_script],
                         [chmod +x orte/tools/wrappers/orte_wrapper_script])
       fi

       m4_ifdef([project_ompi], [], [
          AC_DEFINE_UNQUOTED(WRAPPER_EXTRA_CFLAGS, "$ORTE_WRAPPER_EXTRA_CFLAGS",
              [Additional CFLAGS to pass through the wrapper compilers])
          AC_DEFINE_UNQUOTED(WRAPPER_EXTRA_CFLAGS_PREFIX, "$ORTE_WRAPPER_EXTRA_CFLAGS_PREFIX",
              [Additional CFLAGS_PREFIX to pass through the wrapper compilers])
          AC_DEFINE_UNQUOTED(WRAPPER_EXTRA_LDFLAGS, "$ORTE_WRAPPER_EXTRA_LDFLAGS",
              [Additional LDFLAGS to pass through the wrapper compilers])
          AC_DEFINE_UNQUOTED(WRAPPER_EXTRA_LIBS, "$ORTE_WRAPPER_EXTRA_LIBS",
              [Additional LIBS to pass through the wrapper compilers])
       ])
    ])

    m4_ifdef([project_ompi], [
       AC_MSG_CHECKING([for OMPI CPPFLAGS])
       OMPI_WRAPPER_EXTRA_CPPFLAGS="$ompi_mca_wrapper_extra_cppflags $wrapper_extra_cppflags $with_wrapper_cppflags"
       AC_SUBST([OMPI_WRAPPER_EXTRA_CPPFLAGS])
       AC_MSG_RESULT([$OMPI_WRAPPER_EXTRA_CPPFLAGS])

       AC_MSG_CHECKING([for OMPI CFLAGS])
       OMPI_WRAPPER_EXTRA_CFLAGS="$wrapper_extra_cflags $with_wrapper_cflags"
       AC_SUBST([OMPI_WRAPPER_EXTRA_CFLAGS])
       AC_MSG_RESULT([$OMPI_WRAPPER_EXTRA_CFLAGS])

       AC_MSG_CHECKING([for OMPI CFLAGS_PREFIX])
       OMPI_WRAPPER_EXTRA_CFLAGS_PREFIX="$with_wrapper_cflags_prefix"
       AC_SUBST([OMPI_WRAPPER_EXTRA_CFLAGS_PREFIX])
       AC_MSG_RESULT([$OMPI_WRAPPER_EXTRA_CFLAGS_PREFIX])

       AC_MSG_CHECKING([for OMPI CXXFLAGS])
       OMPI_WRAPPER_EXTRA_CXXFLAGS="$wrapper_extra_cxxflags $with_wrapper_cxxflags"
       AC_SUBST([OMPI_WRAPPER_EXTRA_CXXFLAGS])
       AC_MSG_RESULT([$OMPI_WRAPPER_EXTRA_CXXFLAGS])

       AC_MSG_CHECKING([for OMPI CXXFLAGS_PREFIX])
       OMPI_WRAPPER_EXTRA_CXXFLAGS_PREFIX="$with_wrapper_cxxflags_prefix"
       AC_SUBST([OMPI_WRAPPER_EXTRA_CXXFLAGS_PREFIX])
       AC_MSG_RESULT([$OMPI_WRAPPER_EXTRA_CXXFLAGS_PREFIX])

       AC_MSG_CHECKING([for OMPI FCFLAGS])
       OMPI_WRAPPER_EXTRA_FCFLAGS="$wrapper_extra_fcflags $with_wrapper_fcflags"
       AC_SUBST([OMPI_WRAPPER_EXTRA_FCFLAGS])
       AC_MSG_RESULT([$OMPI_WRAPPER_EXTRA_FCFLAGS])

       AC_MSG_CHECKING([for OMPI FCFLAGS_PREFIX])
       OMPI_WRAPPER_EXTRA_FCFLAGS_PREFIX="$with_wrapper_fcflags_prefix"
       AC_SUBST([OMPI_WRAPPER_EXTRA_FCFLAGS_PREFIX])
       AC_MSG_RESULT([$OMPI_WRAPPER_EXTRA_FCFLAGS_PREFIX])

       AC_MSG_CHECKING([for OMPI LDFLAGS])
       OMPI_WRAPPER_EXTRA_LDFLAGS="$ompi_mca_wrapper_extra_ldflags $wrapper_extra_ldflags $with_wrapper_ldflags"
       RPATHIFY_LDFLAGS([OMPI_WRAPPER_EXTRA_LDFLAGS])
       AC_SUBST([OMPI_WRAPPER_EXTRA_LDFLAGS])
       AC_MSG_RESULT([$OMPI_WRAPPER_EXTRA_LDFLAGS])

       AC_MSG_CHECKING([for OMPI LIBS])
       OMPI_WRAPPER_EXTRA_LIBS="$ompi_mca_wrapper_extra_libs"
       OPAL_FLAGS_APPEND_UNIQ([OMPI_WRAPPER_EXTRA_LIBS], [$wrapper_extra_libs])
       OMPI_WRAPPER_EXTRA_LIBS="$OMPI_WRAPPER_EXTRA_LIBS $with_wrapper_libs"
       AC_SUBST([OMPI_WRAPPER_EXTRA_LIBS])
       AC_MSG_RESULT([$OMPI_WRAPPER_EXTRA_LIBS])

       AC_MSG_CHECKING([for OMPI extra include dirs])
       if test "$WANT_INSTALL_HEADERS" = "1" ; then
           OMPI_WRAPPER_EXTRA_INCLUDES="openmpi"
       else
           OMPI_WRAPPER_EXTRA_INCLUDES=
       fi
       AC_SUBST([OMPI_WRAPPER_EXTRA_INCLUDES])
       AC_MSG_RESULT([$OMPI_WRAPPER_EXTRA_INCLUDES])

      # language binding support.  C++ is a bit different, as the
      # compiler should work even if there is no MPI C++ bindings
      # support.  However, we do want it to fail if there is no C++
      # compiler.
       if test "$WANT_MPI_CXX_SUPPORT" = "1" ; then
          OMPI_WRAPPER_CXX_LIB="-lmpi_cxx"
          OMPI_WRAPPER_CXX_REQUIRED_FILE=""
       elif test "$CXX" = "none"; then
          OMPI_WRAPPER_CXX_LIB=""
          OMPI_WRAPPER_CXX_REQUIRED_FILE="not supported"
       else
          OMPI_WRAPPER_CXX_LIB=""
          OMPI_WRAPPER_CXX_REQUIRED_FILE=""
       fi
       AC_SUBST([OMPI_WRAPPER_CXX_LIB])
       AC_SUBST([OMPI_WRAPPER_CXX_REQUIRED_FILE])

       if test "$OMPI_WANT_FORTRAN_BINDINGS" = "1" ; then
          OMPI_WRAPPER_FORTRAN_REQUIRED_FILE=""
       else
          OMPI_WRAPPER_FORTRAN_REQUIRED_FILE="not supported"
       fi
       AC_SUBST([OMPI_WRAPPER_FORTRAN_REQUIRED_FILE])

       # For script-based wrappers that don't do relocatable binaries.
       # Don't use if you don't have to.
       exec_prefix_save="${exec_prefix}"
       test "x$exec_prefix" = xNONE && exec_prefix="${prefix}"
       eval "OMPI_WRAPPER_INCLUDEDIR=\"${includedir}\""
       eval "OMPI_WRAPPER_LIBDIR=\"${libdir}\""
       exec_prefix="${exec_prefix_save}"
       AC_SUBST([OMPI_WRAPPER_INCLUDEDIR])
       AC_SUBST([OMPI_WRAPPER_LIBDIR])

       # if wrapper compilers were requested, set the ompi one up
       if test "$WANT_SCRIPT_WRAPPER_COMPILERS" = "1" ; then
         AC_CONFIG_FILES([ompi/tools/wrappers/ompi_wrapper_script],
                         [chmod +x ompi/tools/wrappers/ompi_wrapper_script])
       fi

       AC_DEFINE_UNQUOTED(WRAPPER_EXTRA_CFLAGS, "$OMPI_WRAPPER_EXTRA_CFLAGS",
           [Additional CFLAGS to pass through the wrapper compilers])
       AC_DEFINE_UNQUOTED(WRAPPER_EXTRA_CFLAGS_PREFIX, "$OMPI_WRAPPER_EXTRA_CFLAGS_PREFIX",
           [Additional CFLAGS_PREFIX to pass through the wrapper compilers])
       AC_DEFINE_UNQUOTED(WRAPPER_EXTRA_CXXFLAGS, "$OMPI_WRAPPER_EXTRA_CXXFLAGS",
           [Additional CXXFLAGS to pass through the wrapper compilers])
       AC_DEFINE_UNQUOTED(WRAPPER_EXTRA_CXXFLAGS_PREFIX, "$OMPI_WRAPPER_EXTRA_CXXFLAGS_PREFIX",
           [Additional CXXFLAGS_PREFIX to pass through the wrapper compilers])
       AC_DEFINE_UNQUOTED(WRAPPER_EXTRA_FCFLAGS, "$OMPI_WRAPPER_EXTRA_FCFLAGS",
           [Additional FCFLAGS to pass through the wrapper compilers])
       AC_DEFINE_UNQUOTED(WRAPPER_EXTRA_FCFLAGS_PREFIX, "$OMPI_WRAPPER_EXTRA_FCFLAGS_PREFIX",
           [Additional FCFLAGS to pass through the wrapper compilers])
       AC_DEFINE_UNQUOTED(WRAPPER_EXTRA_LDFLAGS, "$OMPI_WRAPPER_EXTRA_LDFLAGS",
           [Additional LDFLAGS to pass through the wrapper compilers])
       AC_DEFINE_UNQUOTED(WRAPPER_EXTRA_LIBS, "$OMPI_WRAPPER_EXTRA_LIBS",
           [Additional LIBS to pass through the wrapper compilers])
    ])

    AC_DEFINE_UNQUOTED(WRAPPER_RPATH_SUPPORT, "$WRAPPER_RPATH_SUPPORT",
        [Whether the wrapper compilers add rpath flags by default])
])
