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
dnl Copyright (c) 2009-2016 Cisco Systems, Inc.  All rights reserved.
dnl Copyright (c) 2015-2017 Research Organization for Information Science
dnl                         and Technology (RIST). All rights reserved.
dnl Copyright (c) 2016      IBM Corporation.  All rights reserved.
dnl Copyright (c) 2017-2019 Intel, Inc.  All rights reserved.
dnl $COPYRIGHT$
dnl
dnl Additional copyrights may follow
dnl
dnl $HEADER$
dnl

# PMIX_WRAPPER_FLAGS_ADD(variable, new_argument)
# ----------------------------------------------
# Add new_argument to the list of arguments for variable in the
# wrapper compilers, if it's not already there.  For example:
#   PMIX_WRAPPER_FLAGS_ADD(CFLAGS, "-pthread")
# will add -pthread to the list of CFLAGS the wrappers use when invoked.
#
# This macro MAY NOT be invoked from configure macros for MCA components.
# See the comment in SETUP_WRAPPER_INIT (below) for more information.
AC_DEFUN([PMIX_WRAPPER_FLAGS_ADD], [
    m4_ifdef([mca_component_configure_active],
        [m4_fatal([PMIX_WRAPPER_FLAGS_ADD can not be called from a component configure])])
    m4_if([$1], [CPPFLAGS], [PMIX_FLAGS_APPEND_UNIQ([wrapper_extra_cppflags], [$2])],
          [$1], [CFLAGS], [PMIX_FLAGS_APPEND_UNIQ([wrapper_extra_cflags], [$2])],
          [$1], [LDFLAGS], [PMIX_FLAGS_APPEND_UNIQ([wrapper_extra_ldflags], [$2])],
          [$1], [LIBS], [PMIX_FLAGS_APPEND_UNIQ([wrapper_extra_libs], [$2])],
          [m4_fatal([Unknown wrapper flag type $1])])
])


# PMIX_SETUP_WRAPPER_INIT()
# -------------------------
# Setup wrapper compiler configuration information.  Should be called early to
# prevent lots of calculations and then an abort for a silly user typo.  This
# macro works in pair with PMIX_SETUP_WRAPPER_FINAL, which should be called
# almost at the end of configure (after the last call to PMIX_WRAPPER_FLAGS_ADD
# and after the MCA system has been setup).
#
# The wrapper compiler arguments are a little fragile and should NOT
# be edited by configure directly.  Instead, main configure should use
# PMIX_WRAPPER_FLAGS_ADD.
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
#   * Keep user flags separate as 1) they should have no influence
#     over build and 2) they don't go through the uniqification we do
#     with the other wrapper compiler options
#   * While the user (the person who runs configure) is allowed to set
#     <flag>_prefix, configure is not.  There's no known use case for
#     doing so, and we'd like to force the issue.
AC_DEFUN([PMIX_SETUP_WRAPPER_INIT],[
    AC_ARG_WITH([wrapper-cflags],
                [AC_HELP_STRING([--with-wrapper-cflags],
                                [Extra flags to add to CFLAGS when using mpicc])])
    AS_IF([test "$with_wrapper_cflags" = "yes" || test "$with_wrapper_cflags" = "no"],
          [AC_MSG_ERROR([--with-wrapper-cflags must have an argument.])])

    AC_ARG_WITH([wrapper-cflags-prefix],
                [AC_HELP_STRING([--with-wrapper-cflags-prefix],
                                [Extra flags (before user flags) to add to CFLAGS when using mpicc])])
    AS_IF([test "$with_wrapper_cflags_prefix" = "yes" || test "$with_wrapper_cflags_prefix" = "no"],
          [AC_MSG_ERROR([--with-wrapper-cflags-prefix must have an argument.])])

    AC_ARG_WITH([wrapper-ldflags],
                [AC_HELP_STRING([--with-wrapper-ldflags],
                                [Extra flags to add to LDFLAGS when using wrapper compilers])])
    AS_IF([test "$with_wrapper_ldflags" = "yes" || test "$with_wrapper_ldflags" = "no"],
          [AC_MSG_ERROR([--with-wrapper-ldflags must have an argument.])])

    AC_ARG_WITH([wrapper-libs],
                [AC_HELP_STRING([--with-wrapper-libs],
                                [Extra flags to add to LIBS when using wrapper compilers])])
    AS_IF([test "$with_wrapper_libs" = "yes" || test "$with_wrapper_libs" = "no"],
          [AC_MSG_ERROR([--with-wrapper-libs must have an argument.])])

    AC_MSG_CHECKING([if want wrapper compiler rpath support])
    AC_ARG_ENABLE([wrapper-rpath],
                  [AS_HELP_STRING([--enable-wrapper-rpath],
                  [enable rpath/runpath support in the wrapper compilers (default=yes)])])
    AS_IF([test "$enable_wrapper_rpath" != "no"], [enable_wrapper_rpath=yes])
    AC_MSG_RESULT([$enable_wrapper_rpath])

    AC_MSG_CHECKING([if want wrapper compiler runpath support])
    AC_ARG_ENABLE([wrapper-runpath],
                  [AS_HELP_STRING([--enable-wrapper-runpath],
                  [enable runpath in the wrapper compilers if linker supports it (default: enabled,  unless wrapper-rpath is disabled).])])
    AS_IF([test "$enable_wrapper_runpath" != "no"], [enable_wrapper_runpath=yes])
    AC_MSG_RESULT([$enable_wrapper_runpath])

    AS_IF([test "$enable_wrapper_rpath" = "no" && test "$enable_wrapper_runpath" = "yes"],
          [AC_MSG_ERROR([--enable-wrapper-runpath cannot be selected with --disable-wrapper-rpath])])
])

# PMIX_LIBTOOL_CONFIG(libtool-variable, result-variable,
#                     libtool-tag, extra-code)
# Retrieve information from the generated libtool
AC_DEFUN([PMIX_LIBTOOL_CONFIG],[
    PMIX_VAR_SCOPE_PUSH([rpath_script rpath_outfile])
    # Output goes into globally-visible variable.  Run this in a
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
$PMIX_TOP_BUILDDIR/libtool $3 --config > $rpath_outfile

chmod +x $rpath_outfile
. ./$rpath_outfile
rm -f $rpath_outfile

# Evaluate \$$1, and substitute in LIBDIR for \$libdir
$4
flags="\`eval echo \$$1\`"
echo \$flags

# Done
exit 0
EOF
    chmod +x $rpath_script
    $2=`./$rpath_script`
    rm -f $rpath_script
    PMIX_VAR_SCOPE_POP
])

# Check to see whether the linker supports DT_RPATH.  We'll need to
# use config.rpath to find the flags that it needs, if it does (see
# comments in config.rpath for an explanation of where it came from).
AC_DEFUN([PMIX_SETUP_RPATH],[
    PMIX_VAR_SCOPE_PUSH([rpath_libdir_save])
    AC_MSG_CHECKING([if linker supports RPATH])
    PMIX_LIBTOOL_CONFIG([hardcode_libdir_flag_spec],[rpath_args],[],[libdir=LIBDIR])

    AS_IF([test -n "$rpath_args"],
          [WRAPPER_RPATH_SUPPORT=rpath
           PMIX_LIBTOOL_CONFIG([hardcode_libdir_flag_spec],[rpath_fc_args],[--tag=FC],[libdir=LIBDIR])
           AC_MSG_RESULT([yes ($rpath_args + $rpath_fc_args)])],
          [WRAPPER_RPATH_SUPPORT=unnecessary
           AC_MSG_RESULT([yes (no extra flags needed)])])

    PMIX_VAR_SCOPE_POP

    # If we found RPATH support, check for RUNPATH support, too
    AS_IF([test "$WRAPPER_RPATH_SUPPORT" = "rpath"],
          [PMIX_SETUP_RUNPATH])
])

# Check to see if the linker supports the DT_RUNPATH flags via
# --enable-new-dtags (a GNU ld-specific option).  These flags are more
# social than DT_RPATH -- they can be overridden by LD_LIBRARY_PATH
# (where a regular DT_RPATH cannot).
#
# If DT_RUNPATH is supported, then we'll use *both* the RPATH and
# RUNPATH flags in the LDFLAGS.
AC_DEFUN([PMIX_SETUP_RUNPATH],[
    PMIX_VAR_SCOPE_PUSH([LDFLAGS_save wl_fc])

    # Set the output in $runpath_args
    runpath_args=
    LDFLAGS_save=$LDFLAGS
    LDFLAGS="$LDFLAGS -Wl,--enable-new-dtags"
    AS_IF([test x"$enable_wrapper_runpath" = x"yes"],
           [AC_LANG_PUSH([C])
            AC_MSG_CHECKING([if linker supports RUNPATH])
            AC_LINK_IFELSE([AC_LANG_PROGRAM([], [return 7;])],
                           [WRAPPER_RPATH_SUPPORT=runpath
                            runpath_args="-Wl,--enable-new-dtags"
                            AC_MSG_RESULT([yes (-Wl,--enable-new-dtags)])],
                           [AC_MSG_RESULT([no])])
            AC_LANG_POP([C])])
    LDFLAGS=$LDFLAGS_save

    PMIX_VAR_SCOPE_POP
])

# Called to find all -L arguments in the LDFLAGS and add in RPATH args
# for each of them.  Then also add in an RPATH for @{libdir} (which
# will be replaced by the wrapper compile to the installdir libdir at
# runtime), and the RUNPATH args, if we have them.
AC_DEFUN([RPATHIFY_LDFLAGS_INTERNAL],[
    PMIX_VAR_SCOPE_PUSH([rpath_out rpath_dir rpath_tmp])
    AS_IF([test "$enable_wrapper_rpath" = "yes" && test "$WRAPPER_RPATH_SUPPORT" != "disabled" && test "$WRAPPER_RPATH_SUPPORT" != "unnecessary"], [
           rpath_out=""
           for val in ${$1}; do
               case $val in
               -L*)
                   rpath_dir=`echo $val | cut -c3-`
                   rpath_tmp=`echo ${$2} | sed -e s@LIBDIR@$rpath_dir@`
                   rpath_out="$rpath_out $rpath_tmp"
                   ;;
               esac
           done

           # Now add in the RPATH args for @{libdir}, and the RUNPATH args
           rpath_tmp=`echo ${$2} | sed -e s/LIBDIR/@{libdir}/`
           $1="${$1} $rpath_out $rpath_tmp ${$3}"
          ])
    PMIX_VAR_SCOPE_POP
])

AC_DEFUN([RPATHIFY_LDFLAGS],[RPATHIFY_LDFLAGS_INTERNAL([$1], [rpath_args], [runpath_args])])

AC_DEFUN([RPATHIFY_FC_LDFLAGS],[RPATHIFY_LDFLAGS_INTERNAL([$1], [rpath_fc_args], [runpath_fc_args])])

dnl
dnl Avoid some repetitive code below
dnl
AC_DEFUN([_PMIX_SETUP_WRAPPER_FINAL_PKGCONFIG],[
    AC_MSG_CHECKING([for $1 pkg-config LDFLAGS])
    $1_PKG_CONFIG_LDFLAGS=`echo "$$1_WRAPPER_EXTRA_LDFLAGS" | sed -e 's/@{libdir}/\${libdir}/g'`
    AC_SUBST([$1_PKG_CONFIG_LDFLAGS])
    AC_MSG_RESULT([$$1_PKG_CONFIG_LDFLAGS])
])


# PMIX_SETUP_WRAPPER_FINAL()
# ---------------------------
AC_DEFUN([PMIX_SETUP_WRAPPER_FINAL],[

    # Setup RPATH support, if desired
    WRAPPER_RPATH_SUPPORT=disabled
    AS_IF([test "$enable_wrapper_rpath" = "yes"],
          [PMIX_SETUP_RPATH])
    AS_IF([test "$enable_wrapper_rpath" = "yes" && test "$WRAPPER_RPATH_SUPPORT" = "disabled"],
          [AC_MSG_WARN([RPATH support requested but not available])
           AC_MSG_ERROR([Cannot continue])])

    # Note that we have to setup <package>_PKG_CONFIG_LDFLAGS for the
    # pkg-config files to parallel the
    # <package>_WRAPPER_EXTRA_LDFLAGS.  This is because pkg-config
    # will not understand the @{libdir} notation in
    # *_WRAPPER_EXTRA_LDFLAGS; we have to translate it to ${libdir}.

    # We now have all relevant flags.  Substitute them in everywhere.
   AC_MSG_CHECKING([for PMIX CPPFLAGS])
   if test "$WANT_INSTALL_HEADERS" = "1" ; then
       PMIX_WRAPPER_EXTRA_CPPFLAGS='-I${includedir}'
   fi
   PMIX_WRAPPER_EXTRA_CPPFLAGS="$PMIX_WRAPPER_EXTRA_CPPFLAGS $pmix_mca_wrapper_extra_cppflags $wrapper_extra_cppflags $with_wrapper_cppflags"
   PMIX_FLAGS_UNIQ(PMIX_WRAPPER_EXTRA_CPPFLAGS)
   AC_SUBST([PMIX_WRAPPER_EXTRA_CPPFLAGS])
   AC_MSG_RESULT([$PMIX_WRAPPER_EXTRA_CPPFLAGS])

   AC_MSG_CHECKING([for PMIX CFLAGS])
   PMIX_WRAPPER_EXTRA_CFLAGS="$wrapper_extra_cflags $with_wrapper_cflags"
   PMIX_FLAGS_UNIQ(PMIX_WRAPPER_EXTRA_CFLAGS)
   AC_SUBST([PMIX_WRAPPER_EXTRA_CFLAGS])
   AC_MSG_RESULT([$PMIX_WRAPPER_EXTRA_CFLAGS])

   AC_MSG_CHECKING([for PMIX CFLAGS_PREFIX])
   PMIX_WRAPPER_EXTRA_CFLAGS_PREFIX="$with_wrapper_cflags_prefix"
   PMIX_FLAGS_UNIQ(PMIX_WRAPPER_EXTRA_CFLAGS_PREFIX)
   AC_SUBST([PMIX_WRAPPER_EXTRA_CFLAGS_PREFIX])
   AC_MSG_RESULT([$PMIX_WRAPPER_EXTRA_CFLAGS_PREFIX])

   AC_MSG_CHECKING([for PMIX LDFLAGS])
   PMIX_WRAPPER_EXTRA_LDFLAGS="$pmix_mca_wrapper_extra_ldflags $wrapper_extra_ldflags $with_wrapper_ldflags"
   PMIX_FLAGS_UNIQ(PMIX_WRAPPER_EXTRA_LDFLAGS)
   RPATHIFY_LDFLAGS([PMIX_WRAPPER_EXTRA_LDFLAGS])
   AC_SUBST([PMIX_WRAPPER_EXTRA_LDFLAGS])
   AC_MSG_RESULT([$PMIX_WRAPPER_EXTRA_LDFLAGS])

   # Convert @{libdir} to ${libdir} for pkg-config
   _PMIX_SETUP_WRAPPER_FINAL_PKGCONFIG([PMIX])

   # wrapper_extra_libs doesn't really get populated until after the mca system runs
   # since most of the libs come from libtool.  So this is the first time we can
   # uniq them.  ROMIO in particular adds lots of things already in wrapper_extra_libs,
   # and this cleans the duplication up a bunch.  Always add everything the user
   # asked for, as they know better than us.
   AC_MSG_CHECKING([for PMIX LIBS])
   PMIX_WRAPPER_EXTRA_LIBS="$pmix_mca_wrapper_extra_libs"
   PMIX_FLAGS_APPEND_UNIQ([PMIX_WRAPPER_EXTRA_LIBS], [$wrapper_extra_libs])
   PMIX_WRAPPER_EXTRA_LIBS="$PMIX_WRAPPER_EXTRA_LIBS $with_wrapper_libs"
   PMIX_FLAGS_UNIQ(PMIX_WRAPPER_EXTRA_LIBS)
   AC_SUBST([PMIX_WRAPPER_EXTRA_LIBS])
   AC_MSG_RESULT([$PMIX_WRAPPER_EXTRA_LIBS])
])
