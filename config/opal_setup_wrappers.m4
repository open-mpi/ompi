dnl -*- autoconf -*-
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
dnl Copyright (c) 2009-2021 Cisco Systems, Inc.  All rights reserved.
dnl Copyright (c) 2015-2017 Research Organization for Information Science
dnl                         and Technology (RIST). All rights reserved.
dnl Copyright (c) 2016      IBM Corporation.  All rights reserved.
dnl Copyright (c) 2020      Triad National Security, LLC. All rights
dnl                         reserved.
dnl Copyright (c) 2021-2022 Amazon.com, Inc. or its affiliates.  All Rights reserved.
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
          [$1], [STATIC_LDFLAGS], [OPAL_FLAGS_APPEND_UNIQ([wrapper_extra_static_ldflags], [$2])],
          [$1], [LIBS], [OPAL_FLAGS_APPEND_MOVE([wrapper_extra_libs], [$2])],
          [$1], [STATIC_LIBS], [OPAL_FLAGS_APPEND_UNIQ([wrapper_extra_static_libs], [$2])],
          [$1], [PC_MODULES], [OPAL_APPEND_UNIQ([wrapper_extra_pkgconfig_modules], [$2])],
          [m4_fatal([Unknown wrapper flag type $1])])
    opal_show_verbose "Adding \"$2\" to \"$1\""
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
#   * Keep user flags separate as 1) they should have no influence
#     over build and 2) they don't go through the uniqification we do
#     with the other wrapper compiler options
#   * While the user (the person who runs configure) is allowed to set
#     <flag>_prefix, configure is not.  There's no known use case for
#     doing so, and we'd like to force the issue.
AC_DEFUN([OPAL_SETUP_WRAPPER_INIT],[
    dnl for OPAL_CC
    AC_REQUIRE([OPAL_SETUP_CC])

    opal_show_subtitle "Wrapper compiler setup"

    OPAL_VAR_SCOPE_PUSH([wrapper_cc_tmp])
    AC_ARG_WITH([wrapper_cc],
                [AS_HELP_STRING([--with-wrapper-cc=path],
                                [Set a different wrapper C compiler than the one used to build Open MPI])],
                [], [with_wrapper_cc="$OPAL_CC"])

    AC_MSG_CHECKING([for wrapper C compiler])

    if test "$with_wrapper_cc" = "yes" || test "$with_wrapper_cc" = "no" ; then
        AC_MSG_ERROR([--with-wrapper-cc must have an argument.])
    fi

    # Get the full path to the wrapper compiler. If it doesn't exist
    # assume that the path is not currently valid.
    wrapper_tmp="$(type -p "$with_wrapper_cc")"
    if test -z "$wrapper_tmp" ; then
        AC_MSG_WARN([could not find "$with_wrapper_cc" in path])
    fi
    WRAPPER_CC=$with_wrapper_cc

    AC_MSG_RESULT([$WRAPPER_CC])

    AC_SUBST([WRAPPER_CC])

    AC_ARG_WITH([wrapper-cflags],
                [AS_HELP_STRING([--with-wrapper-cflags],
                                [Extra flags to add to CFLAGS when using mpicc])])
    AS_IF([test "$with_wrapper_cflags" = "yes" || test "$with_wrapper_cflags" = "no"],
          [AC_MSG_ERROR([--with-wrapper-cflags must have an argument.])])

    AC_ARG_WITH([wrapper-cflags-prefix],
                [AS_HELP_STRING([--with-wrapper-cflags-prefix],
                                [Extra flags (before user flags) to add to CFLAGS when using mpicc])])
    AS_IF([test "$with_wrapper_cflags_prefix" = "yes" || test "$with_wrapper_cflags_prefix" = "no"],
          [AC_MSG_ERROR([--with-wrapper-cflags-prefix must have an argument.])])

    m4_ifdef([project_ompi], [
            AC_ARG_WITH([wrapper-cxxflags],
                [AS_HELP_STRING([--with-wrapper-cxxflags],
                                [Extra flags to add to CXXFLAGS when using mpiCC/mpic++])])
            AS_IF([test "$with_wrapper_cxxflags" = "yes" || test "$with_wrapper_cxxflags" = "no"],
                  [AC_MSG_ERROR([--with-wrapper-cxxflags must have an argument.])])

            AC_ARG_WITH([wrapper-cxxflags-prefix],
                [AS_HELP_STRING([--with-wrapper-cxxflags-prefix],
                                [Extra flags to add to CXXFLAGS when using mpiCC/mpic++])])
            AS_IF([test "$with_wrapper_cxxflags_prefix" = "yes" || test "$with_wrapper_cxxflags_prefix" = "no"],
                  [AC_MSG_ERROR([--with-wrapper-cxxflags-prefix must have an argument.])])

            AC_ARG_WITH([wrapper-fcflags],
                [AS_HELP_STRING([--with-wrapper-fcflags],
                        [Extra flags to add to FCFLAGS when using mpifort])])
            AS_IF([test "$with_wrapper_fcflags" = "yes" || test "$with_wrapper_fcflags" = "no"],
                [AC_MSG_ERROR([--with-wrapper-fcflags must have an argument.])])

            AC_ARG_WITH([wrapper-fcflags-prefix],
                [AS_HELP_STRING([--with-wrapper-fcflags-prefix],
                        [Extra flags (before user flags) to add to FCFLAGS when using mpifort])])
            AS_IF([test "$with_wrapper_fcflags_prefix" = "yes" || test "$with_wrapper_fcflags_prefix" = "no"],
                [AC_MSG_ERROR([--with-wrapper-fcflags-prefix must have an argument.])])])

    AC_ARG_WITH([wrapper-ldflags],
                [AS_HELP_STRING([--with-wrapper-ldflags],
                                [Extra flags to add to LDFLAGS when using wrapper compilers])])
    AS_IF([test "$with_wrapper_ldflags" = "yes" || test "$with_wrapper_ldflags" = "no"],
          [AC_MSG_ERROR([--with-wrapper-ldflags must have an argument.])])

    AC_ARG_WITH([wrapper-libs],
                [AS_HELP_STRING([--with-wrapper-libs],
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
    OPAL_VAR_SCOPE_POP
])

# OPAL_LIBTOOL_CONFIG(libtool-variable, result-variable,
#                     libtool-tag, extra-code)
# Retrieve information from the generated libtool
AC_DEFUN([OPAL_LIBTOOL_CONFIG],[
    OPAL_VAR_SCOPE_PUSH([rpath_script rpath_outfile])
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
$OPAL_TOP_BUILDDIR/libtool $3 --config > $rpath_outfile

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
    OPAL_VAR_SCOPE_POP
])

# Check to see whether the linker supports DT_RPATH.  We'll need to
# use config.rpath to find the flags that it needs, if it does (see
# comments in config.rpath for an explanation of where it came from).
AC_DEFUN([OPAL_SETUP_RPATH],[
    OPAL_VAR_SCOPE_PUSH([rpath_libdir_save])
    AC_MSG_CHECKING([if linker supports RPATH])
    OPAL_LIBTOOL_CONFIG([hardcode_libdir_flag_spec],[rpath_args],[],[libdir=LIBDIR])

    AS_IF([test -n "$rpath_args"],
          [WRAPPER_RPATH_SUPPORT=rpath
           OPAL_LIBTOOL_CONFIG([hardcode_libdir_flag_spec],[rpath_fc_args],[--tag=FC],[libdir=LIBDIR])
           AC_MSG_RESULT([yes ($rpath_args + $rpath_fc_args)])],
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
    OPAL_VAR_SCOPE_PUSH([LDFLAGS_save wl_fc])

    # Set the output in $runpath_args
    runpath_args=
    runpath_fc_args=
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
            AC_LANG_POP([C])
            m4_ifdef([project_ompi],
                     [OPAL_LIBTOOL_CONFIG([wl],[wl_fc],[--tag=FC],[])
                      LDFLAGS="$LDFLAGS_save ${wl_fc}--enable-new-dtags"
                      AC_LANG_PUSH([Fortran])
                      AC_MSG_CHECKING([if Fortran linker supports RUNPATH])
                      AC_LINK_IFELSE([AC_LANG_SOURCE([[program test 
end program]])],
                                     [runpath_fc_args="${wl_fc}--enable-new-dtags"
                                      AC_MSG_RESULT([yes (-Wl,--enable-new-dtags)])],
                                     [AC_MSG_RESULT([no])])
                      AC_LANG_POP([Fortran])])])

    LDFLAGS=$LDFLAGS_save

    OPAL_VAR_SCOPE_POP
])

# Called to find all -L arguments in the LDFLAGS and add in RPATH args
# for each of them.  Then also add in an RPATH for @{libdir} (which
# will be replaced by the wrapper compile to the installdir libdir at
# runtime), and the RUNPATH args, if we have them.
AC_DEFUN([RPATHIFY_LDFLAGS_INTERNAL],[
    OPAL_VAR_SCOPE_PUSH([rpath_out rpath_dir rpath_tmp])
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

           $1="${$1} $rpath_out"
          ])
    OPAL_VAR_SCOPE_POP
])

AC_DEFUN([RPATHIFY_LDFLAGS],[RPATHIFY_LDFLAGS_INTERNAL([$1], [rpath_args])])

AC_DEFUN([RPATHIFY_FC_LDFLAGS],[RPATHIFY_LDFLAGS_INTERNAL([$1], [rpath_fc_args])])


# OPAL_SETUP_WRAPPER_FINAL()
# ---------------------------
#
# Here are the situations that we need to cover with wrapper compilers
# and pkg-config files:
#
# 1) --enable-shared --disable-static (today's default): Any
#    application linking against libmpi will be a dynamically linked
#    application
# 2) --enable-shared --enable-static: An application linking against
#    libmpi will dynamically link against libmpi unless -static (or
#    similar) is passed, in which case it will static link against
#    libmpi (and the static versions of all of libmpi's dependencies).
# 3) --disable-shared --enable-static: Any application linking against
#    libmpi will link against libmpi.a.  That application will link
#    against the dynamic versions of libmpi's dependencies, unless
#    -static is passed.
#
# There is one situation we should explicitly handle in terms of
# wrapper compilers (someone could parse out all the right pkg-config
# or wrapper compiler options to get the right dependent libraries, of
# course):
#
# 1) --enable-shared --enable-static: An application links via
#    /usr/lib/libmpi.a instead of -lmpi.  We'll make no attempts to
#    recognize this case with the wrapper compiler
#
# So, we essentially have 5 cases above to cover with the wrapper
# compiler and pkg-config.  For the wrapper compiler, this means:
#
# 1) --enable-shared --disable-static: Regardless of the -static flag,
#    we only add the -L${libdir} -lmpi
# 2) --enable-shared --enable-static / no -static flag: we add
#    -L${libdir} -lmpi
# 3) --enable-shared --enable-static / -static flag: we add
#    -L${libdir} -lmpi plus the LDFLAGS and LIBS for our dependencies
#    AND their reported dependencies (ie the results of pkg-config --libs
#    --static for all our dependencies).
# 4) --disable-shared --enable-static / no -static flag: We add
#    -L${libdir} -lmpi plus the LDFLAGS and LIBS for our dependencies,
#    but not their dependencies (ie, the results of pkg-config --libs for
#    all our dependencies)
# 5) --disable-shared --enable-static / -static flag: We add
#    -L${libdir} -lmpi plus the LDFLAGS and LIBs for our dependencies
#    AND their reported dependencies (ie, the results of pkg-config
#    --libs --static for all our dependencies)
#
# For the pkg-config modules, this means:
#
# 1) --enable-shared --disable-static: We add -L${libdir} -lmpi to
#    Libs and Libs.private, Modules, and Modules.private are empty
# 2/3) --enable-shared --enable-static: We add -L${libdir} -lmpi to
#    Libs, Libs.private contains all the -L/-ls from our dependencies
#    that don't have pkg-config modules, Modules is empty, and
#    Modules.private contains all the modules for our dependencies.
# 4/5) --disable-shared --enable-static: We add -L${libdir} -lmpi to
#    Libs, Libs.private contains all the -L/-ls from our dependencies that
#    don't have pkg-config modules, Modules contains all the modules for
#    our dependencies, and Modules.private is empty.
#
# 2/3 means that `pkg-config --libs mpi` would return -L${libdir}
#  -lmpi and `pkg-config --libs --static mpi` would return
# -L${libdir} -lmpi -Lnon-pkg-config-dependency
# -lnon-pkg-config-dependency ...., plus all the `pkg-config --libs
# --static` results for all our pkg-config dependencies.
#
# 4/5 means that `pkg-config --libs mpi` would return -L${libdir}
# -lmpi -Lnon-pkg-config-dependency -lnon-pkg-config-dependency ....,
# plus all the `pkg-config --libs` results for all our pkg-config
# dependencies AND that `pkg-config --libs --static mpi` would return
# -L${libdir} -lmpi -Lnon-pkg-config-dependency
# -lnon-pkg-config-dependency ...., plus all the `pkg-config --libs
# --static` results for all our pkg-config dependencies.
AC_DEFUN([OPAL_SETUP_WRAPPER_FINAL],[
    OPAL_VAR_SCOPE_PUSH([wrapper_tmp_arg wrapper_finalize_opal_libs wrapper_finalize_ompi_libs])

    # Setup RPATH support, if desired
    WRAPPER_RPATH_SUPPORT=disabled
    AS_IF([test "$enable_wrapper_rpath" = "yes"],
          [OPAL_SETUP_RPATH])
    AS_IF([test "$enable_wrapper_rpath" = "yes" && test "$WRAPPER_RPATH_SUPPORT" = "disabled"],
          [AC_MSG_WARN([RPATH support requested but not available])
           AC_MSG_ERROR([Cannot continue])])

    AC_DEFINE_UNQUOTED(WRAPPER_RPATH_SUPPORT, "$WRAPPER_RPATH_SUPPORT",
        [Whether the wrapper compilers add rpath flags by default])

    # We now have all relevant flags.  Substitute them in everywhere.

    dnl Add LIBS into the extra wrapper libs, since this is as last
    dnl minute as we can get.  We do the temp variable bit because of
    dnl libevent and hwloc dependencies.  LIBS is going to contain
    dnl libevent/libev/hwloc libraries, but their dependencies are
    dnl already in wrapper_extra_libs.  We do not want to move -lhwloc
    dnl (for example) to the far right, right of its dependencies.  So
    dnl we start with our base libs, and add all the wrapper extra
    dnl bits to that.
    wapper_tmp_arg="${LIBS}"
    OPAL_FLAGS_APPEND_MOVE([wrapper_tmp_arg], [${wrapper_extra_libs}])
    wrapper_extra_libs="${wrapper_tmp_arg}"

    dnl We do not want ${includedir} to be expanded, as we want that
    dnl expansion to happen in the wrapper or pkg-config.
    m4_ifdef([project_opal], [
       AC_MSG_CHECKING([for OPAL wrapper CPPFLAGS])
       AS_IF([test "$WANT_INSTALL_HEADERS" = "1"],
             [OPAL_WRAPPER_CPPFLAGS='-I${includedir} -I${includedir}/openmpi'],
             [OPAL_WRAPPER_CPPFLAGS='-I${includedir}'])
       OPAL_FLAGS_APPEND_UNIQ([OPAL_WRAPPER_CPPFLAGS], [$opal_mca_wrapper_extra_cppflags $wrapper_extra_cppflags $with_wrapper_cppflags])
       AC_SUBST([OPAL_WRAPPER_CPPFLAGS])
       AC_MSG_RESULT([$OPAL_WRAPPER_CPPFLAGS])

       AC_MSG_CHECKING([for OPAL wrapper CFLAGS])
       OPAL_WRAPPER_CFLAGS="$wrapper_extra_cflags $with_wrapper_cflags"
       AC_SUBST([OPAL_WRAPPER_CFLAGS])
       AC_MSG_RESULT([$OPAL_WRAPPER_CFLAGS])

       AC_MSG_CHECKING([for OPAL wrapper CFLAGS_PREFIX])
       OPAL_WRAPPER_CFLAGS_PREFIX="$with_wrapper_cflags_prefix"
       AC_SUBST([OPAL_WRAPPER_CFLAGS_PREFIX])
       AC_MSG_RESULT([$OPAL_WRAPPER_CFLAGS_PREFIX])

       AC_MSG_CHECKING([for OPAL wrapper CXXFLAGS])
       OPAL_WRAPPER_CXXFLAGS="$wrapper_extra_cxxflags $with_wrapper_cxxflags"
       AC_SUBST([OPAL_WRAPPER_CXXFLAGS])
       AC_MSG_RESULT([$OPAL_WRAPPER_CXXFLAGS])

       AC_MSG_CHECKING([for OPAL wrapper CXXFLAGS_PREFIX])
       OPAL_WRAPPER_CXXFLAGS_PREFIX="$with_wrapper_cxxflags_prefix"
       AC_SUBST([OPAL_WRAPPER_CXXFLAGS_PREFIX])
       AC_MSG_RESULT([$OPAL_WRAPPER_CXXFLAGS_PREFIX])

       wrapper_finalize_opal_libs="-l${OPAL_LIB_NAME}"

       dnl No matter the configuration (see the 5 cases above), the base
       dnl flags should contain a -L${libdir} and -lopen-pal, so that those
       dnl are found.
       OPAL_WRAPPER_LDFLAGS='-L${libdir}'
       OPAL_WRAPPER_LIBS="${wrapper_finalize_opal_libs}"
       OPAL_WRAPPER_LIBS_STATIC=
       OPAL_WRAPPER_LDFLAGS_STATIC=

       AS_IF(dnl shared only case.  We add no flags beyond the base -L/-l
             [test "${enable_shared}" != "no" -a "${enable_static}" != "yes"],
             [],
             dnl building both shared and static libraries.  The base
             dnl case remains the same as the shared-only case (because
             dnl the app will link against the shared library, but the
             dnl static case is the full dependency tree.  Our full
             dnl dependency tree is both the wrapper_extra_libs and
             dnl wrapper_extra_static_libs, because wrapper_extra_libs
             dnl was not added to the normal case.
             [test "${enable_shared}" != "no" -a "${enable_static}" = "yes"],
             [OPAL_FLAGS_APPEND_UNIQ([OPAL_WRAPPER_LDFLAGS_STATIC], [${opal_mca_wrapper_extra_ldflags} ${wrapper_extra_ldflags}])
              OPAL_FLAGS_APPEND_MOVE([OPAL_WRAPPER_LIBS_STATIC], [${opal_mca_wrapper_extra_libs} ${wrapper_extra_libs}])
              OPAL_FLAGS_APPEND_UNIQ([OPAL_WRAPPER_LDFLAGS_STATIC], [${opal_mca_wrapper_extra_static_ldflags} ${wrapper_extra_static_ldflags}])
              OPAL_FLAGS_APPEND_MOVE([OPAL_WRAPPER_LIBS_STATIC], [${opal_mca_wrapper_extra_static_libs} ${wrapper_extra_static_libs}])],
             dnl building static only.  The base case is that we need to
             dnl list our dependencies, but not the full treee, because
             dnl we assume that our dependencies will be shared libraries
             dnl (unless they too were built static only, in which case
             dnl their dependencies will be our direct dependencies if
             dnl their modules are setup correctly).  The static case is
             dnl our full dependency tree, but we only need to list the
             dnl second leve explicitly, because the wrapper compiler
             dnl and/or pkg-config merge use the normal case data in the
             dnl static case.
             [OPAL_FLAGS_APPEND_UNIQ([OPAL_WRAPPER_LDFLAGS], [${opal_mca_wrapper_extra_ldflags} ${wrapper_extra_ldflags}])
              OPAL_FLAGS_APPEND_MOVE([OPAL_WRAPPER_LIBS], [${opal_mca_wrapper_extra_libs} ${wrapper_extra_libs}])
              OPAL_FLAGS_APPEND_UNIQ([OPAL_WRAPPER_LDFLAGS_STATIC], [${opal_mca_wrapper_extra_static_ldflags} ${wrapper_extra_static_ldflags}])
              OPAL_FLAGS_APPEND_MOVE([OPAL_WRAPPER_LIBS_STATIC], [${opal_mca_wrapper_extra_static_libs} ${wrapper_extra_static_libs}])])

       dnl Add the user-provided flags
       OPAL_FLAGS_APPEND_UNIQ([OPAL_WRAPPER_LDFLAGS], [${with_wrapper_ldflags}])
       OPAL_FLAGS_APPEND_MOVE([OPAL_WRAPPER_LIBS], [${with_wrapper_libs}])

       RPATHIFY_LDFLAGS([OPAL_WRAPPER_LDFLAGS])
       RPATHIFY_LDFLAGS([OPAL_WRAPPER_LDFLAGS_STATIC])

       OPAL_FLAGS_APPEND_UNIQ([OPAL_WRAPPER_LDFLAGS], [${runpath_args}])

       AC_MSG_CHECKING([for OPAL wrapper LDFLAGS])
       AC_SUBST([OPAL_WRAPPER_LDFLAGS])
       AC_MSG_RESULT([$OPAL_WRAPPER_LDFLAGS])

       AC_MSG_CHECKING([for OPAL wrapper static LDFLAGS])
       AC_SUBST([OPAL_WRAPPER_LDFLAGS_STATIC])
       AC_MSG_RESULT([$OPAL_WRAPPER_LDFLAGS_STATIC])

       AC_MSG_CHECKING([for OPAL wrapper LIBS])
       AC_SUBST([OPAL_WRAPPER_LIBS])
       AC_MSG_RESULT([$OPAL_WRAPPER_LIBS])

       AC_MSG_CHECKING([for OPAL wrapper static LIBS])
       AC_SUBST([OPAL_WRAPPER_LIBS_STATIC])
       AC_MSG_RESULT([$OPAL_WRAPPER_LIBS_STATIC])
    ])

    m4_ifdef([project_ompi], [
       AC_MSG_CHECKING([for OMPI wrapper CPPFLAGS])
       AS_IF([test "$WANT_INSTALL_HEADERS" = "1"],
             [OMPI_WRAPPER_CPPFLAGS='-I${includedir} -I${includedir}/openmpi'],
             [OMPI_WRAPPER_CPPFLAGS='-I${includedir}'])
       OPAL_FLAGS_APPEND_UNIQ([OMPI_WRAPPER_CPPFLAGS], [$ompi_mca_wrapper_extra_cppflags $wrapper_extra_cppflags $with_wrapper_cppflags])
       AC_SUBST([OMPI_WRAPPER_CPPFLAGS])
       AC_MSG_RESULT([$OMPI_WRAPPER_CPPFLAGS])

       AC_MSG_CHECKING([for OMPI wrapper CFLAGS])
       OMPI_WRAPPER_CFLAGS="$wrapper_extra_cflags $with_wrapper_cflags"
       AC_SUBST([OMPI_WRAPPER_CFLAGS])
       AC_MSG_RESULT([$OMPI_WRAPPER_CFLAGS])

       AC_MSG_CHECKING([for OMPI wrapper CFLAGS_PREFIX])
       OMPI_WRAPPER_CFLAGS_PREFIX="$with_wrapper_cflags_prefix"
       AC_SUBST([OMPI_WRAPPER_CFLAGS_PREFIX])
       AC_MSG_RESULT([$OMPI_WRAPPER_CFLAGS_PREFIX])

       AC_MSG_CHECKING([for OMPI wrapper CXXFLAGS])
       OMPI_WRAPPER_CXXFLAGS="$wrapper_extra_cxxflags $with_wrapper_cxxflags"
       AC_SUBST([OMPI_WRAPPER_CXXFLAGS])
       AC_MSG_RESULT([$OMPI_WRAPPER_CXXFLAGS])

       AC_MSG_CHECKING([for OMPI wrapper CXXFLAGS_PREFIX])
       OMPI_WRAPPER_CXXFLAGS_PREFIX="$with_wrapper_cxxflags_prefix"
       AC_SUBST([OMPI_WRAPPER_CXXFLAGS_PREFIX])
       AC_MSG_RESULT([$OMPI_WRAPPER_CXXFLAGS_PREFIX])

       AC_MSG_CHECKING([for OMPI wrapper FCFLAGS])
       OMPI_WRAPPER_FCFLAGS='-I${includedir}'" ${wrapper_extra_fcflags} ${with_wrapper_fcflags}"
       AS_IF([test -n "${OMPI_FC_MODULE_FLAG}"],
             [dnl deal with some interesting expansion behavior in OPAL_APPEND
              wrapper_tmp_arg="${OMPI_FC_MODULE_FLAG}${OMPI_FORTRAN_MODULEDIR}"
              OPAL_APPEND([OMPI_WRAPPER_FCFLAGS], [${wrapper_tmp_arg}])])
       AC_SUBST([OMPI_WRAPPER_FCFLAGS])
       AC_MSG_RESULT([$OMPI_WRAPPER_EXTRA_FCFLAGS])

       AC_MSG_CHECKING([for OMPI wrapper FCFLAGS_PREFIX])
       OMPI_WRAPPER_FCFLAGS_PREFIX="$with_wrapper_fcflags_prefix"
       AC_SUBST([OMPI_WRAPPER_FCFLAGS_PREFIX])
       AC_MSG_RESULT([$OMPI_WRAPPER_FCFLAGS_PREFIX])

       wrapper_finalize_ompi_libs="-l${OMPI_LIBMPI_NAME}"

       dnl No matter the configuration (see the 5 cases above), the base
       dnl flags should contain a -L${libdir} and -lmpi, so that those
       dnl are found.
       OMPI_WRAPPER_LDFLAGS='-L${libdir}'
       OMPI_WRAPPER_LIBS="${wrapper_finalize_ompi_libs}"
       OMPI_WRAPPER_LIBS_STATIC=
       OMPI_WRAPPER_LDFLAGS_STATIC=

       AS_IF(dnl shared only case.  We add no flags beyond the base -L/-l
             [test "${enable_shared}" != "no" -a "${enable_static}" != "yes"],
             [],
             dnl building both shared and static libraries.  The base
             dnl case remains the same as the shared-only case (because
             dnl the app will link against the shared library, but the
             dnl static case is the full dependency tree.  Our full
             dnl dependency tree is both the wrapper_extra_libs and
             dnl wrapper_extra_static_libs, because wrapper_extra_libs
             dnl was not added to the normal case.
             [test "${enable_shared}" != "no" -a "${enable_static}" = "yes"],
             [OPAL_FLAGS_APPEND_UNIQ([OMPI_WRAPPER_LDFLAGS_STATIC], [${ompi_mca_wrapper_extra_ldflags} ${wrapper_extra_ldflags}])
              OPAL_FLAGS_APPEND_MOVE([OMPI_WRAPPER_LIBS_STATIC], [${wrapper_finalize_opal_libs} ${ompi_mca_wrapper_extra_libs} ${wrapper_extra_libs}])
              OPAL_FLAGS_APPEND_UNIQ([OMPI_WRAPPER_LDFLAGS_STATIC], [${ompi_mca_wrapper_extra_static_ldflags} ${wrapper_extra_static_ldflags}])
              OPAL_FLAGS_APPEND_MOVE([OMPI_WRAPPER_LIBS_STATIC], [${ompi_mca_wrapper_extra_static_libs} ${wrapper_extra_static_libs}])],
             dnl building static only.  The base case is that we need to
             dnl list our dependencies, but not the full treee, because
             dnl we assume that our dependencies will be shared libraries
             dnl (unless they too were built static only, in which case
             dnl their dependencies will be our direct dependencies if
             dnl their modules are setup correctly).  The static case is
             dnl our full dependency tree, but we only need to list the
             dnl second leve explicitly, because the wrapper compiler
             dnl and/or pkg-config merge use the normal case data in the
             dnl static case.
             [OPAL_FLAGS_APPEND_UNIQ([OMPI_WRAPPER_LDFLAGS], [${ompi_mca_wrapper_extra_ldflags} ${wrapper_extra_ldflags}])
              OPAL_FLAGS_APPEND_MOVE([OMPI_WRAPPER_LIBS], [${wrapper_finalize_opal_libs} ${ompi_mca_wrapper_extra_libs} ${wrapper_extra_libs}])
              OPAL_FLAGS_APPEND_UNIQ([OMPI_WRAPPER_LDFLAGS_STATIC], [${ompi_mca_wrapper_extra_static_ldflags} ${wrapper_extra_static_ldflags}])
              OPAL_FLAGS_APPEND_MOVE([OMPI_WRAPPER_LIBS_STATIC], [${ompi_mca_wrapper_extra_static_libs} ${wrapper_extra_static_libs}])])

       dnl Add the user-provided flags
       OPAL_FLAGS_APPEND_UNIQ([OMPI_WRAPPER_LDFLAGS], [${with_wrapper_ldflags}])
       OPAL_FLAGS_APPEND_MOVE([OMPI_WRAPPER_LIBS], [${with_wrapper_libs}])

       dnl fortran FTW!
       OMPI_WRAPPER_FC_LIBS="${OMPI_FORTRAN_USEMPIF08_LIB} ${OMPI_FORTRAN_USEMPI_LIB} ${OMPI_FORTRAN_MPIFH_LINK} ${OMPI_WRAPPER_LIBS}"
       OMPI_WRAPPER_FC_LIBS_STATIC=${OMPI_WRAPPER_LIBS_STATIC}
       OMPI_WRAPPER_FC_LDFLAGS=$OMPI_WRAPPER_LDFLAGS
       OMPI_WRAPPER_FC_LDFLAGS_STATIC=$OMPI_WRAPPER_LDFLAGS

       RPATHIFY_LDFLAGS([OMPI_WRAPPER_LDFLAGS])
       RPATHIFY_LDFLAGS([OMPI_WRAPPER_LDFLAGS_STATIC])
       RPATHIFY_FC_LDFLAGS([OMPI_WRAPPER_FC_LDFLAGS])
       RPATHIFY_FC_LDFLAGS([OMPI_WRAPPER_FC_LDFLAGS_STATIC])

       OPAL_FLAGS_APPEND_UNIQ([OMPI_WRAPPER_LDFLAGS], [${runpath_args}])
       OPAL_FLAGS_APPEND_UNIQ([OMPI_WRAPPER_FC_LDFLAGS], [${runpath_fc_args}])

       AC_MSG_CHECKING([for OMPI wrapper LDFLAGS])
       AC_SUBST([OMPI_WRAPPER_LDFLAGS])
       AC_MSG_RESULT([$OMPI_WRAPPER_LDFLAGS])

       AC_MSG_CHECKING([for OMPI wrapper static LDFLAGS])
       AC_SUBST([OMPI_WRAPPER_LDFLAGS_STATIC])
       AC_MSG_RESULT([$OMPI_WRAPPER_LDFLAGS_STATIC])

       AC_MSG_CHECKING([for OMPI wrapper LIBS])
       AC_SUBST([OMPI_WRAPPER_LIBS])
       AC_MSG_RESULT([$OMPI_WRAPPER_LIBS])

       AC_MSG_CHECKING([for OMPI wrapper static LIBS])
       AC_SUBST([OMPI_WRAPPER_LIBS_STATIC])
       AC_MSG_RESULT([$OMPI_WRAPPER_LIBS_STATIC])

       AC_MSG_CHECKING([for OMPI wrapper Fortran LDFLAGS])
       AC_SUBST([OMPI_WRAPPER_FC_LDFLAGS])
       AC_MSG_RESULT([$OMPI_WRAPPER_FC_LDFLAGS])

       AC_MSG_CHECKING([for OMPI wrapper Fortran static LDFLAGS])
       AC_SUBST([OMPI_WRAPPER_FC_LDFLAGS_STATIC])
       AC_MSG_RESULT([$OMPI_WRAPPER_LDFLAGS_STATIC])

       AC_MSG_CHECKING([for OMPI wrapper Fortran LIBS])
       AC_SUBST([OMPI_WRAPPER_FC_LIBS])
       AC_MSG_RESULT([$OMPI_WRAPPER_FC_LIBS])

       AC_MSG_CHECKING([for OMPI wrapper Fortran static LIBS])
       AC_SUBST([OMPI_WRAPPER_FC_LIBS_STATIC])
       AC_MSG_RESULT([$OMPI_WRAPPER_FC_LIBS_STATIC])

       # language binding support.  C++ is a bit different, as the
       # compiler should work even if there is no MPI C++ bindings
       # support.  However, we do want it to fail if there is no C++
       # compiler.
       if test "$CXX" = "none"; then
          OMPI_WRAPPER_CXX_REQUIRED_FILE="not supported"
       else
          OMPI_WRAPPER_CXX_REQUIRED_FILE=""
       fi
       AC_SUBST([OMPI_WRAPPER_CXX_REQUIRED_FILE])

       if test "$OMPI_TRY_FORTRAN_BINDINGS" -gt "$OMPI_FORTRAN_NO_BINDINGS" ; then
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

       AC_DEFINE_UNQUOTED(OMPI_WRAPPER_CFLAGS, "$OMPI_WRAPPER_CFLAGS",
           [CFLAGS to pass through the wrapper compilers])
       AC_DEFINE_UNQUOTED(OMPI_WRAPPER_CXXFLAGS, "$OMPI_WRAPPER_CXXFLAGS",
           [CXXFLAGS to pass through the wrapper compilers])
       AC_DEFINE_UNQUOTED(OMPI_WRAPPER_FCFLAGS, "$OMPI_WRAPPER__FCFLAGS",
           [FCFLAGS to pass through the wrapper compilers])
       AC_DEFINE_UNQUOTED(OMPI_WRAPPER_LDFLAGS, "$OMPI_WRAPPER_LDFLAGS",
           [LDFLAGS to pass through the wrapper compilers])
       AC_DEFINE_UNQUOTED(OMPI_WRAPPER_LIBS, "$OMPI_WRAPPER_LIBS",
           [LIBS to pass through the wrapper compilers])

       dnl ####################################################################
       dnl  Setup variables for pkg-config files
       dnl
       dnl Add all our dependent libraries to libs.Private for users that want
       dnl to static build, unless we're only building static libraries, in
       dnl which case, add the dependent libraries to libs itself, since any
       dnl linking will require the full set of libraries.
       dnl ####################################################################
       AC_MSG_CHECKING([for OMPI pkg-config Cflags])
       OMPI_PC_CFLAGS="${OMPI_WRAPPER_CPPFLAGS} ${OMPI_WRAPPER_CFLAGS} ${OMPI_WRAPPER_CFLAGS_PREFIX}"
       OMPI_PC_CFLAGS=`echo ${OMPI_PC_CFLAGS} | sed -e 's/@{/\${/g'`
       AC_SUBST([OMPI_PC_CFLAGS])
       AC_MSG_RESULT([${OMPI_PC_CFLAGS}])

       AC_MSG_CHECKING([for OMPI pkg-config Libs])
       OMPI_PC_LIBS="${OMPI_WRAPPER_LDFLAGS} ${OMPI_WRAPPER_LIBS}"
       OMPI_PC_LIBS=`echo ${OMPI_PC_LIBS} | sed -e 's/@{/\${/g'`
       AC_SUBST([OMPI_PC_LIBS])
       AC_MSG_RESULT([${OMPI_PC_LIBS}])

       AC_MSG_CHECKING([for OMPI pkg-config Libs.private])
       OMPI_PC_LIBS_PRIVATE="${OMPI_WRAPPER_LDFLAGS_STATIC} ${OMPI_WRAPPER_LIBS_STATIC}"
       OMPI_PC_LIBS_PRIVATE=`echo ${OMPI_PC_LIBS_PRIVATE} | sed -e 's/@{/\${/g'`
       AC_SUBST([OMPI_PC_LIBS_PRIVATE])
       AC_MSG_RESULT([${OMPI_PC_LIBS_PRIVATE}])

       AC_MSG_CHECKING([for OMPI pkg-config Fortran Cflags])
       OMPI_PC_FC_CFLAGS="${OMPI_WRAPPER_FCFLAGS} ${OMPI_WRAPPER_FCFLAGS_PREFIX}"
       OMPI_PC_FC_CFLAGS=`echo ${OMPI_PC_FC_CFLAGS} | sed -e 's/@{/\${/g'`
       AC_SUBST([OMPI_PC_FC_CFLAGS])
       AC_MSG_RESULT([${OMPI_PC_FC_CFLAGS}])

       AC_MSG_CHECKING([for OMPI pkg-config Fortran Libs])
       OMPI_PC_FC_LIBS="${OMPI_WRAPPER_FC_LDFLAGS} ${OMPI_WRAPPER_FC_LIBS}"
       OMPI_PC_FC_LIBS=`echo ${OMPI_PC_FC_LIBS} | sed -e 's/@{/\${/g'`
       AC_SUBST([OMPI_PC_FC_LIBS])
       AC_MSG_RESULT([${OMPI_PC_FC_LIBS}])

       AC_MSG_CHECKING([for OMPI pkg-config Fortran Libs.private])
       OMPI_PC_FC_LIBS_PRIVATE="${OMPI_WRAPPER_FC_LDFLAGS_STATIC} ${OMPI_WRAPPER_FC_LIBS_STATIC}"
       OMPI_PC_FC_LIBS_PRIVATE=`echo ${OMPI_PC_FC_LIBS_PRIVATE} | sed -e 's/@{/\${/g'`
       AC_SUBST([OMPI_PC_FC_LIBS_PRIVATE])
       AC_MSG_RESULT([${OMPI_PC_FC_LIBS_PRIVATE}])

       OMPI_PC_MODULES=
       OMPI_PC_MODULES_PRIVATE=
       AS_IF([test "${enable_shared}" != "no" -a "${enable_static}" != "yes"],
             [],
             [test "${enable_shared}" != "no" -a "${enable_static}" = "yes"],
             [OMPI_PC_MODULES_PRIVATE="${wrapper_extra_pkgconfig_modules} ${ompi_mca_wrapper_extra_pc_modules}"],
             [OMPI_PC_MODULES="${wrapper_extra_pkgconfig_modules} ${ompi_mca_wrapper_extra_pc_modules}"])

       AC_MSG_CHECKING([for OMPI pkg-config Modules])
       AC_SUBST([OMPI_PC_MODULES])
       AC_MSG_RESULT([${OMPI_PC_MODULES}])

       AC_MSG_CHECKING([for OMPI pkg-config Modules.private])
       AC_SUBST([OMPI_PC_MODULES_PRIVATE])
       AC_MSG_RESULT([${OMPI_PC_MODULES_PRIVATE}])
    ])

    m4_ifdef([project_oshmem], [
       AC_MSG_CHECKING([for OSHMEM wrapper CPPFLAGS])
       AS_IF([test "$WANT_INSTALL_HEADERS" = "1"],
             [OSHMEM_WRAPPER_CPPFLAGS='-I${includedir} -I${includedir}/openmpi'],
             [OSHMEM_WRAPPER_CPPFLAGS='-I${includedir}'])
       OPAL_FLAGS_APPEND_UNIQ([OSHMEM_WRAPPER_CPPFLAGS], [$oshmem_mca_wrapper_extra_cppflags $wrapper_extra_cppflags $with_wrapper_cppflags])
       AC_SUBST([OSHMEM_WRAPPER_CPPFLAGS])
       AC_MSG_RESULT([$OSHMEM_WRAPPER_CPPFLAGS])

       AC_MSG_CHECKING([for OSHMEM wrapper CFLAGS])
       OSHMEM_WRAPPER_CFLAGS="$wrapper_extra_cflags $with_wrapper_cflags"
       AC_SUBST([OSHMEM_WRAPPER_CFLAGS])
       AC_MSG_RESULT([$OSHMEM_WRAPPER_CFLAGS])

       AC_MSG_CHECKING([for OSHMEM wrapper CFLAGS_PREFIX])
       OSHMEM_WRAPPER_CFLAGS_PREFIX="$with_wrapper_cflags_prefix"
       AC_SUBST([OSHMEM_WRAPPER_CFLAGS_PREFIX])
       AC_MSG_RESULT([$OSHMEM_WRAPPER_CFLAGS_PREFIX])

       AC_MSG_CHECKING([for OSHMEM wrapper CXXFLAGS])
       OSHMEM_WRAPPER_CXXFLAGS="$wrapper_extra_cxxflags $with_wrapper_cxxflags"
       AC_SUBST([OSHMEM_WRAPPER_CXXFLAGS])
       AC_MSG_RESULT([$OSHMEM_WRAPPER_CXXFLAGS])

       AC_MSG_CHECKING([for OSHMEM wrapper CXXFLAGS_PREFIX])
       OSHMEM_WRAPPER_CXXFLAGS_PREFIX="$with_wrapper_cxxflags_prefix"
       AC_SUBST([OSHMEM_WRAPPER_CXXFLAGS_PREFIX])
       AC_MSG_RESULT([$OSHMEM_WRAPPER_CXXFLAGS_PREFIX])

       AC_MSG_CHECKING([for OSHMEM wrapper FCFLAGS])
       OSHMEM_WRAPPER_FCFLAGS='-I${includedir}'" ${wrapper_extra_fcflags} ${with_wrapper_fcflags}"
       AS_IF([test -n "${OMPI_FC_MODULE_FLAG}"],
             [dnl deal with some interesting expansion behavior in OPAL_APPEND
              wrapper_tmp_arg="${OMPI_FC_MODULE_FLAG}${OMPI_FORTRAN_MODULEDIR}"
              OPAL_APPEND([OSHMEM_WRAPPER_FCFLAGS], [${wrapper_tmp_arg}])])
       AC_SUBST([OSHMEM_WRAPPER_FCFLAGS])
       AC_MSG_RESULT([$OSHMEM_WRAPPER_EXTRA_FCFLAGS])

       AC_MSG_CHECKING([for OSHMEM wrapper FCFLAGS_PREFIX])
       OSHMEM_WRAPPER_FCFLAGS_PREFIX="$with_wrapper_fcflags_prefix"
       AC_SUBST([OSHMEM_WRAPPER_FCFLAGS_PREFIX])
       AC_MSG_RESULT([$OSHMEM_WRAPPER_FCFLAGS_PREFIX])

       dnl No matter the configuration (see the 5 cases above), the
       dnl base flags should contain a -L${libdir} and -loshmem -lmpi,
       dnl so that those are found.
       OSHMEM_WRAPPER_LDFLAGS='-L${libdir}'
       OSHMEM_WRAPPER_LIBS="-loshmem ${wrapper_finalize_ompi_libs}"
       OSHMEM_WRAPPER_LIBS_STATIC=
       OSHMEM_WRAPPER_LDFLAGS_STATIC=

       AS_IF(dnl shared only case.  We add no flags beyond the base -L/-l
             [test "${enable_shared}" != "no" -a "${enable_static}" != "yes"],
             [],
             dnl building both shared and static libraries.  The base
             dnl case remains the same as the shared-only case (because
             dnl the app will link against the shared library, but the
             dnl static case is the full dependency tree.  Our full
             dnl dependency tree is both the wrapper_extra_libs and
             dnl wrapper_extra_static_libs, because wrapper_extra_libs
             dnl was not added to the normal case.
             [test "${enable_shared}" != "no" -a "${enable_static}" = "yes"],
             [OPAL_FLAGS_APPEND_UNIQ([OSHMEM_WRAPPER_LDFLAGS_STATIC], [${oshmem_mca_wrapper_extra_ldflags} ${wrapper_extra_ldflags}])
              OPAL_FLAGS_APPEND_MOVE([OSHMEM_WRAPPER_LIBS_STATIC], [${wrapper_finalize_opal_libs} ${oshmem_mca_wrapper_extra_libs} ${wrapper_extra_libs}])
              OPAL_FLAGS_APPEND_UNIQ([OSHMEM_WRAPPER_LDFLAGS_STATIC], [${oshmem_mca_wrapper_extra_static_ldflags} ${wrapper_extra_static_ldflags}])
              OPAL_FLAGS_APPEND_MOVE([OSHMEM_WRAPPER_LIBS_STATIC], [${oshmem_mca_wrapper_extra_static_libs} ${wrapper_extra_static_libs}])],
             dnl building static only.  The base case is that we need to
             dnl list our dependencies, but not the full treee, because
             dnl we assume that our dependencies will be shared libraries
             dnl (unless they too were built static only, in which case
             dnl their dependencies will be our direct dependencies if
             dnl their modules are setup correctly).  The static case is
             dnl our full dependency tree, but we only need to list the
             dnl second leve explicitly, because the wrapper coshmemler
             dnl and/or pkg-config merge use the normal case data in the
             dnl static case.
             [OPAL_FLAGS_APPEND_UNIQ([OSHMEM_WRAPPER_LDFLAGS], [${oshmem_mca_wrapper_extra_ldflags} ${wrapper_extra_ldflags}])
              OPAL_FLAGS_APPEND_MOVE([OSHMEM_WRAPPER_LIBS], [${wrapper_finalize_opal_libs} ${oshmem_mca_wrapper_extra_libs} ${wrapper_extra_libs}])
              OPAL_FLAGS_APPEND_UNIQ([OSHMEM_WRAPPER_LDFLAGS_STATIC], [${oshmem_mca_wrapper_extra_static_ldflags} ${wrapper_extra_static_ldflags}])
              OPAL_FLAGS_APPEND_MOVE([OSHMEM_WRAPPER_LIBS_STATIC], [${oshmem_mca_wrapper_extra_static_libs} ${wrapper_extra_static_libs}])])

       dnl Add the user-provided flags
       OPAL_FLAGS_APPEND_UNIQ([OSHMEM_WRAPPER_LDFLAGS], [${with_wrapper_ldflags}])
       OPAL_FLAGS_APPEND_MOVE([OSHMEM_WRAPPER_LIBS], [${with_wrapper_libs}])

       dnl fortran FTW!
       OSHMEM_WRAPPER_FC_LIBS="-loshmem ${OMPI_FORTRAN_MPIFH_LINK}"
       OPAL_FLAGS_APPEND_UNIQ([OSHMEM_WRAPPER_FC_LIBS], [${OSHMEM_WRAPPER_LIBS}])
       OSHMEM_WRAPPER_FC_LIBS_STATIC=${OSHMEM_WRAPPER_LIBS_STATIC}
       OSHMEM_WRAPPER_FC_LDFLAGS=$OSHMEM_WRAPPER_LDFLAGS
       OSHMEM_WRAPPER_FC_LDFLAGS_STATIC=$OSHMEM_WRAPPER_LDFLAGS

       RPATHIFY_LDFLAGS([OSHMEM_WRAPPER_LDFLAGS])
       RPATHIFY_LDFLAGS([OSHMEM_WRAPPER_LDFLAGS_STATIC])
       RPATHIFY_FC_LDFLAGS([OSHMEM_WRAPPER_FC_LDFLAGS])
       RPATHIFY_FC_LDFLAGS([OSHMEM_WRAPPER_FC_LDFLAGS_STATIC])

       OPAL_FLAGS_APPEND_UNIQ([OSHMEM_WRAPPER_LDFLAGS], [${runpath_args}])
       OPAL_FLAGS_APPEND_UNIQ([OSHMEM_WRAPPER_FC_LDFLAGS], [${runpath_fc_args}])

       AC_MSG_CHECKING([for OSHMEM wrapper LDFLAGS])
       AC_SUBST([OSHMEM_WRAPPER_LDFLAGS])
       AC_MSG_RESULT([$OSHMEM_WRAPPER_LDFLAGS])

       AC_MSG_CHECKING([for OSHMEM wrapper static LDFLAGS])
       AC_SUBST([OSHMEM_WRAPPER_LDFLAGS_STATIC])
       AC_MSG_RESULT([$OSHMEM_WRAPPER_LDFLAGS_STATIC])

       AC_MSG_CHECKING([for OSHMEM wrapper LIBS])
       AC_SUBST([OSHMEM_WRAPPER_LIBS])
       AC_MSG_RESULT([$OSHMEM_WRAPPER_LIBS])

       AC_MSG_CHECKING([for OSHMEM wrapper static LIBS])
       AC_SUBST([OSHMEM_WRAPPER_LIBS_STATIC])
       AC_MSG_RESULT([$OSHMEM_WRAPPER_LIBS_STATIC])

       AC_MSG_CHECKING([for OSHMEM wrapper Fortran LDFLAGS])
       AC_SUBST([OSHMEM_WRAPPER_FC_LDFLAGS])
       AC_MSG_RESULT([$OSHMEM_WRAPPER_FC_LDFLAGS])

       AC_MSG_CHECKING([for OSHMEM wrapper Fortran static LDFLAGS])
       AC_SUBST([OSHMEM_WRAPPER_FC_LDFLAGS_STATIC])
       AC_MSG_RESULT([$OSHMEM_WRAPPER_FC_LDFLAGS_STATIC])

       AC_MSG_CHECKING([for OSHMEM wrapper Fortran LIBS])
       AC_SUBST([OSHMEM_WRAPPER_FC_LIBS])
       AC_MSG_RESULT([$OSHMEM_WRAPPER_FC_LIBS])

       AC_MSG_CHECKING([for OSHMEM wrapper Fortran static LIBS])
       AC_SUBST([OSHMEM_WRAPPER_FC_LIBS_STATIC])
       AC_MSG_RESULT([$OSHMEM_WRAPPER_FC_LIBS_STATIC])

       # language binding support.  C++ is a bit different, as the
       # coshmemler should work even if there is no MPI C++ bindings
       # support.  However, we do want it to fail if there is no C++
       # coshmemler.
       if test "$CXX" = "none"; then
          OSHMEM_WRAPPER_CXX_REQUIRED_FILE="not supported"
       else
          OSHMEM_WRAPPER_CXX_REQUIRED_FILE=""
       fi
       AC_SUBST([OSHMEM_WRAPPER_CXX_REQUIRED_FILE])

       if test "$OMPI_TRY_FORTRAN_BINDINGS" -gt "$OMPI_FORTRAN_NO_BINDINGS" ; then
          OSHMEM_WRAPPER_FORTRAN_REQUIRED_FILE=""
       else
          OSHMEM_WRAPPER_FORTRAN_REQUIRED_FILE="not supported"
       fi
       AC_SUBST([OSHMEM_WRAPPER_FORTRAN_REQUIRED_FILE])

       AC_DEFINE_UNQUOTED(OSHMEM_WRAPPER_CFLAGS, "$OSHMEM_WRAPPER_CFLAGS",
           [CFLAGS to pass through the wrapper compilers])
       AC_DEFINE_UNQUOTED(OSHMEM_WRAPPER_CXXFLAGS, "$OSHMEM_WRAPPER_CXXFLAGS",
           [CXXFLAGS to pass through the wrapper compilers])
       AC_DEFINE_UNQUOTED(OSHMEM_WRAPPER_FCFLAGS, "$OSHMEM_WRAPPER__FCFLAGS",
           [FCFLAGS to pass through the wrapper compilers])
       AC_DEFINE_UNQUOTED(OSHMEM_WRAPPER_LDFLAGS, "$OSHMEM_WRAPPER_LDFLAGS",
           [LDFLAGS to pass through the wrapper compilers])
       AC_DEFINE_UNQUOTED(OSHMEM_WRAPPER_LIBS, "$OSHMEM_WRAPPER_LIBS",
           [LIBS to pass through the wrapper compilers])

       dnl ####################################################################
       dnl  Setup variables for pkg-config files
       dnl
       dnl Add all our dependent libraries to libs.Private for users that want
       dnl to static build, unless we're only building static libraries, in
       dnl which case, add the dependent libraries to libs itself, since any
       dnl linking will require the full set of libraries.
       dnl ####################################################################
       AC_MSG_CHECKING([for OSHMEM pkg-config Cflags])
       OSHMEM_PC_CFLAGS="${OSHMEM_WRAPPER_CPPFLAGS} ${OSHMEM_WRAPPER_CFLAGS} ${OSHMEM_WRAPPER_CFLAGS_PREFIX}"
       OSHMEM_PC_CFLAGS=`echo ${OSHMEM_PC_CFLAGS} | sed -e 's/@{/\${/g'`
       AC_SUBST([OSHMEM_PC_CFLAGS])
       AC_MSG_RESULT([${OSHMEM_PC_CFLAGS}])

       AC_MSG_CHECKING([for OSHMEM pkg-config Libs])
       OSHMEM_PC_LIBS="${OSHMEM_WRAPPER_LDFLAGS} ${OSHMEM_WRAPPER_LIBS}"
       OSHMEM_PC_LIBS=`echo ${OSHMEM_PC_LIBS} | sed -e 's/@{/\${/g'`
       AC_SUBST([OSHMEM_PC_LIBS])
       AC_MSG_RESULT([${OSHMEM_PC_LIBS}])

       AC_MSG_CHECKING([for OSHMEM pkg-config Libs.private])
       OSHMEM_PC_LIBS_PRIVATE="${OSHMEM_WRAPPER_LDFLAGS_STATIC} ${OSHMEM_WRAPPER_LIBS_STATIC}"
       OSHMEM_PC_LIBS_PRIVATE=`echo ${OSHMEM_PC_LIBS_PRIVATE} | sed -e 's/@{/\${/g'`
       AC_SUBST([OSHMEM_PC_LIBS_PRIVATE])
       AC_MSG_RESULT([${OSHMEM_PC_LIBS_PRIVATE}])

       AC_MSG_CHECKING([for OSHMEM pkg-config Fortran Cflags])
       OSHMEM_PC_CFLAGS="${OSHMEM_WRAPPER_FCFLAGS} ${OSHMEM_WRAPPER_FCFLAGS_PREFIX}"
       OSHMEM_PC_CFLAGS=`echo ${OSHMEM_PC_CFLAGS} | sed -e 's/@{/\${/g'`
       AC_SUBST([OSHMEM_PC_FC_CFLAGS])
       AC_MSG_RESULT([${OSHMEM_PC_FC_CFLAGS}])

       AC_MSG_CHECKING([for OSHMEM pkg-config Fortran Libs])
       OSHMEM_PC_FC_LIBS="${OSHMEM_WRAPPER_FC_LDFLAGS} ${OSHMEM_WRAPPER_FC_LIBS}"
       OSHMEM_PC_FC_LIBS=`echo ${OSHMEM_PC_FC_LIBS} | sed -e 's/@{/\${/g'`
       AC_SUBST([OSHMEM_PC_FC_LIBS])
       AC_MSG_RESULT([${OSHMEM_PC_FC_LIBS}])

       AC_MSG_CHECKING([for OSHMEM pkg-config Fortran Libs.private])
       OSHMEM_PC_FC_LIBS_PRIVATE="${OSHMEM_WRAPPER_FC_LDFLAGS_STATIC} ${OSHMEM_WRAPPER_FC_LIBS_STATIC}"
       OSHMEM_PC_FC_LIBS_PRIVATE=`echo ${OSHMEM_PC_FC_LIBS_PRIVATE} | sed -e 's/@{/\${/g'`
       AC_SUBST([OSHMEM_PC_FC_LIBS_PRIVATE])
       AC_MSG_RESULT([${OSHMEM_PC_FC_LIBS_PRIVATE}])

       OSHMEM_PC_MODULES=
       OSHMEM_PC_MODULES_PRIVATE=
       AS_IF([test "${enable_shared}" != "no" -a "${enable_static}" != "yes"],
             [],
             [test "${enable_shared}" != "no" -a "${enable_static}" = "yes"],
             [OSHMEM_PC_MODULES_PRIVATE="${wrapper_extra_pkgconfig_modules} ${oshmem_mca_wrapper_extra_pc_modules}"],
             [OSHMEM_PC_MODULES="${wrapper_extra_pkgconfig_modules} ${oshmem_mca_wrapper_extra_pc_modules}"])

       AC_MSG_CHECKING([for OSHMEM pkg-config Modules])
       AC_SUBST([OSHMEM_PC_MODULES])
       AC_MSG_RESULT([${OSHMEM_PC_MODULES}])

       AC_MSG_CHECKING([for OSHMEM pkg-config Modules.private])
       AC_SUBST([OSHMEM_PC_MODULES_PRIVATE])
       AC_MSG_RESULT([${OSHMEM_PC_MODULES_PRIVATE}])
    ])

    OPAL_VAR_SCOPE_POP
])
