dnl -*- shell-script -*-
dnl
dnl Copyright (c) 2004-2009 The Trustees of Indiana University and Indiana
dnl                         University Research and Technology
dnl                         Corporation.  All rights reserved.
dnl Copyright (c) 2009-2010 Cisco Systems, Inc.  All rights reserved.
dnl $COPYRIGHT$
dnl 
dnl Additional copyrights may follow
dnl 
dnl $HEADER$
dnl

######################################################################
#
# OMPI_EXT
#
# configure the Interface Extensions [similar to MCA version].  Works hand in
# hand with Open MPI's autogen.pl, requiring it's specially formatted lists
# of frameworks, components, etc.
#
# USAGE:
#   OMPI_EXT()
#
######################################################################
AC_DEFUN([OMPI_EXT],[
    dnl for OPAL_CONFIGURE_USER env variable
    AC_REQUIRE([OPAL_CONFIGURE_SETUP])

    # Note that we do not build DSO's here -- we *only* build convenience
    # libraries that get slurped into higher-level libraries
    #
    # [default -- no option given] = No extensions built
    # --enable-mpi-ext=[,]*EXTENSION[,]*
    #
    AC_ARG_ENABLE(mpi-ext,
        AC_HELP_STRING([--enable-mpi-ext[=LIST]],
                       [Comma-separated list of extensions that should be built.  Possible values: ompi_mpiext_list.  Example: "--enable-mpi-ext=foo,bar" will enable building the MPI extensions "foo" and "bar".  If LIST is empty or the special value "all", then all available MPI extensions will be built (default: none).]))

    # print some nice messages about what we're about to do...
    AC_MSG_CHECKING([for available MPI Extensions])
    AC_MSG_RESULT([ompi_mpiext_list])

    AC_MSG_CHECKING([which MPI extension should be enabled])
    if test "$enable_mpi_ext" = "yes" -o "$enable_mpi_ext" = "all"; then
        msg="All Extensions"
        str="`echo ENABLE_EXT_ALL=1`"
        eval $str
    else
        ifs_save="$IFS"
        IFS="${IFS}$PATH_SEPARATOR,"
        msg=
        for item in $enable_mpi_ext; do
            type="`echo $item | cut -s -f1 -d-`"
            if test -z $type ; then
                type=$item
            fi
            str="`echo ENABLE_${type}=1 | sed s/-/_/g`"
            eval $str
            msg="$item $msg"
        done
        IFS="$ifs_save"
    fi
    AC_MSG_RESULT([$msg])
    unset msg

    m4_ifdef([ompi_mpiext_list], [],
             [m4_fatal([Could not find MPI Extensions list.  Aborting.])])

    EXT_NO_CONFIG_CONFIG_FILES()
    EXT_CONFIGURE()

    AC_SUBST(EXT_C_HEADERS)
    AC_SUBST(EXT_CXX_HEADERS)
    AC_SUBST(EXT_F77_HEADERS)
    AC_SUBST(EXT_F90_HEADERS)
    AC_SUBST(EXT_C_LIBS)
    AC_SUBST(EXT_CXX_LIBS)
    AC_SUBST(EXT_F77_LIBS)
    AC_SUBST(EXT_F90_LIBS)
])


######################################################################
#
# EXT_CONFIGURE
#
# USAGE:
#   EXT_CONFIGURE()
#
######################################################################
AC_DEFUN([EXT_CONFIGURE],[
    OPAL_VAR_SCOPE_PUSH([all_components outfile outfile_real])

    all_components=
    static_components=
    static_ltlibs=

    outdir=ompi/include

    # first create the output include directory
    mkdir -p $outdir

    # remove any previously generated #include files
    mpi_ext_h=$outdir/mpi-ext.h
    rm -f $mpi_ext_h

    # Create the final mpi-ext.h file.
    cat > $mpi_ext_h <<EOF
/*
 * \$HEADER\$
 */

#ifndef OMPI_MPI_EXT_H
#define OMPI_MPI_EXT_H 1

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

#define OMPI_HAVE_MPI_EXT 1

EOF

    #
    # XXX: Left todo: Add header files for other languages
    #

    # remove any previously generated #include files
    outfile_real=ompi/mpiext/static-components.h
    outfile=$outfile_real.new
    rm -f $outfile $outfile.struct $outfile.extern
    $MKDIR_P ompi/mpiext
    touch $outfile.struct $outfile.extern

    m4_foreach(extension, [ompi_mpiext_list],
               [m4_ifval(extension,
               [EXT_CONFIGURE_M4_CONFIG_COMPONENT(extension,
                                                  [all_components],
                                                  [static_components],
                                                  [static_ltlibs])])])

    # Create the final mpi-ext.h file.
    cat >> $mpi_ext_h <<EOF

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif /* OMPI_MPI_EXT_H */

EOF

    #
    # XXX: Left todo: Close header files for other languages
    #


    # Create the final .h file that will be included in the type's
    # top-level glue.  This lists all the static components.  We don't
    # need to do this for "common".
    if test "$2" != "common"; then
        cat > $outfile <<EOF
/*
 * \$HEADER\$
 */
#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

`cat $outfile.extern`

const ompi_mpiext_component_t *ompi_mpiext_components[[]] = {
`cat $outfile.struct`
  NULL
};

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

EOF
        # Only replace the header file if a) it doesn't previously
        # exist, or b) the contents are different.  Do this to not
        # trigger recompilation of certain .c files just because the
        # timestamp changed on $outfile_real (similar to the way AC
        # handles AC_CONFIG_HEADER files).
        diff $outfile $outfile_real > /dev/null 2>&1
        if test "$?" != "0"; then
            mv $outfile $outfile_real
        else
            rm -f $outfile
        fi
    fi
    rm -f $outfile.struct $outfile.extern 

    OMPI_EXT_MAKE_DIR_LIST(OMPI_MPIEXT_ALL_SUBDIRS, [$all_components])
    OMPI_EXT_MAKE_DIR_LIST(OMPI_MPIEXT_SUBDIRS, [$static_components])

    comps=`echo $static_components | sed -e 's/^[ \t]*//;s/[ \t]*$//;s/ /, /g'`
    AC_DEFINE_UNQUOTED([OMPI_MPIEXT_COMPONENTS], ["$comps"], [MPI Extensions included in libmpi])

    OMPI_MPIEXT_LIBS="${static_ltlibs}"
    AC_SUBST(OMPI_MPIEXT_LIBS)

    OPAL_VAR_SCOPE_POP
])


######################################################################
#
# EXT_CONFIGURE_M4_CONFIG_COMPONENT
#
#
# USAGE:
#   EXT_CONFIGURE_PROJECT(component_name
#                         all_components_variable, 
#                         static_components_variable, 
#                         static_ltlibs_variable)
#
######################################################################
AC_DEFUN([EXT_CONFIGURE_M4_CONFIG_COMPONENT],[
    ompi_show_subsubsubtitle "MPI Extension $1"

    EXT_COMPONENT_BUILD_CHECK($1, [should_build=1], [should_build=0])

    # try to configure the component.  pay no attention to
    # --enable-dist, since we'll always have makefiles.
    m4_ifdef([OMPI_MPIEXT_$1_CONFIG], [],
             [m4_fatal([Could not find OMPI_MPIEXT_]$1[_CONFIG macro for ]$1[ component])])

    OMPI_MPIEXT_$1_CONFIG([should_build=${should_build}], 
                          [should_build=0])

    AS_IF([test "$should_build" = "1"],
          [EXT_PROCESS_COMPONENT($1, $2, $4)
           # add component to static component list
           $3="$$3 $1" ],
          [EXT_PROCESS_DEAD_COMPONENT($1)
           # add component to all component list
           $2="$$2 $1"])
])

######################################################################
#
# EXT_PROCESS_COMPONENT
#
# does all setup work for given component.  It should be known before
# calling that this component can build properly (and exists)
#
# USAGE:
#   EXT_CONFIGURE_ALL_CONFIG_COMPONENTS(component_name
#                         all_components_variable (2), 
#                         static_ltlibs_variable (3),
#                         compile_mode_variable (4))
#
#   NOTE: component_name may not be determined until runtime....
#
# M4 directive to disable language support in configure.m4
#   Need to build a list of .la for each lang. to pull into final library
# List ext_c_headers, ext_c_libs {same for other lang.}
# C:   framework_component_c{.h, .la} 
# CXX: framework_component_cxx{.h, .la} 
# F77: framework_component_f77{.h, .la} 
# F90: framework_component_f90{.h, .la} ??? 
######################################################################
AC_DEFUN([EXT_PROCESS_COMPONENT],[
    AC_REQUIRE([AC_PROG_GREP])

    component=$1

    # Output pretty results
    AC_MSG_CHECKING([if MPI extension $component can compile])
    AC_MSG_RESULT([yes])

    # Save the list of headers and convenience libraries that this component will output
    # There *must* be C bindings
    EXT_C_HEADERS="$EXT_C_HEADERS mpiext/$component/mpiext_${component}_c.h"
    EXT_C_LIBS="$EXT_C_LIBS mpiext/$component/libext_mpiext_${component}.la"

    component_header="mpiext_${component}_c.h"
    tmp[=]m4_translit([$1],[a-z],[A-Z])
    component_define="OMPI_HAVE_MPI_EXT_${tmp}"

    cat >> $mpi_ext_h <<EOF
/* Enabled Extension: $component */
#define $component_define 1
#include "openmpi/ompi/mpiext/$component/$component_header"

EOF

    #
    # XXX: Need to add conditional logic for components that do not supply
    # XXX: some or all of the other 3 interfaces [C++, F77, F90]. If they
    # XXX: did provide those bindings, then add the header file to the relevant
    # XXX: language binding's header file.
    #
    EXT_CXX_HEADERS="$EXT_CXX_HEADERS mpiext/$component/mpiext_${component}_cxx.h"
    EXT_CXX_LIBS="$EXT_CXX_LIBS mpiext/$component/libext_mpiext_${component}_cxx.la"

    EXT_F77_HEADERS="$EXT_F77_HEADERS mpiext/$component/mpiext_${component}_f77.h"
    EXT_F77_LIBS="$EXT_F77_LIBS mpiext/$component/libext_mpiext_${component}_f77.la"

    EXT_F90_HEADERS="$EXT_F90_HEADERS mpiext/$component/mpiext_${component}_f90.h"
    EXT_F90_LIBS="$EXT_F90_LIBS mpiext/$component/libext_mpiext_${component}_f90.la"

    # Add this subdir to the mast list of all EXT component subdirs
    $2="$$2 ${component}"
    $3="mpiext/${component}/libext_mpiext_${component}.la $$3"

    m4_ifdef([OMPI_MPIEXT_]$1[_NEED_INIT],
             [echo "extern const ompi_mpiext_component_t ompi_mpiext_${component};" >> $outfile.extern
              echo "  &ompi_mpiext_${component}, " >> $outfile.struct])

    # now add the flags that were set in the environment variables
    # framework_component_FOO (for example, the flags set by
    # m4_configure components)
    #
    # Check for flags passed up from the component.  If we're
    # compiling statically, then take all flags passed up from the
    # component.
    m4_foreach(flags, [LDFLAGS, LIBS],
        [[str="line=\$mpiext_${component}_WRAPPER_EXTRA_]flags["]
          eval "$str"
          if test -n "$line" ; then
             $1[_WRAPPER_EXTRA_]flags[="$]$1[_WRAPPER_EXTRA_]flags[ $line"]
          fi
          ])dnl
])


######################################################################
#
# EXT_PROCESS_DEAD_COMPONENT
#
# process a component that can not be built.  Do the last minute checks
# to make sure the user isn't doing something stupid.
#
# USAGE:
#   EXT_PROCESS_DEAD_COMPONENT(component_name)
#
#   NOTE: component_name may not be determined until runtime....
#
######################################################################
AC_DEFUN([EXT_PROCESS_DEAD_COMPONENT],[
    AC_MSG_CHECKING([if MPI Extension $1 can compile])
    AC_MSG_RESULT([no])
])



######################################################################
#
# EXT_COMPONENT_BUILD_CHECK
#
# checks the standard rules of component building to see if the 
# given component should be built.
#
# USAGE:
#    EXT_COMPONENT_BUILD_CHECK(component, 
#                              action-if-build, action-if-not-build)
#
######################################################################
AC_DEFUN([EXT_COMPONENT_BUILD_CHECK],[
    AC_REQUIRE([AC_PROG_GREP])

    component=$1
    component_path="$srcdir/ompi/mpiext/$component"
    want_component=0

    # build if:
    # - the component type is direct and we are that component
    # - there is no ompi_ignore file
    # - there is an ompi_ignore, but there is an empty ompi_unignore
    # - there is an ompi_ignore, but username is in ompi_unignore
    if test -d $component_path ; then
        # decide if we want the component to be built or not.  This
        # is spread out because some of the logic is a little complex
        # and test's syntax isn't exactly the greatest.  We want to
        # build the component by default.
        want_component=1
        if test -f $component_path/.ompi_ignore ; then
            # If there is an ompi_ignore file, don't build
            # the component.  Note that this decision can be
            # overridden by the unignore logic below.
            want_component=0
        fi
        if test -f $component_path/.ompi_unignore ; then
            # if there is an empty ompi_unignore, that is
            # equivalent to having your userid in the unignore file.
            # If userid is in the file, unignore the ignore file.
            if test ! -s $component_path/.ompi_unignore ; then
                want_component=1
            elif test ! -z "`$GREP $OPAL_CONFIGURE_USER $component_path/.ompi_unignore`" ; then
                want_component=1
            fi
        fi
    fi

    # if we asked for everything, then allow it to build if able
    str="ENABLED_COMPONENT_CHECK=\$ENABLE_EXT_ALL"
    eval $str
    if test ! "$ENABLED_COMPONENT_CHECK" = "1" ; then
        # if we were explicitly disabled, don't build :)
        str="ENABLED_COMPONENT_CHECK=\$ENABLE_${component}"
        eval $str
        if test ! "$ENABLED_COMPONENT_CHECK" = "1" ; then
            want_component=0
        fi
    fi

    AS_IF([test "$want_component" = "1"], [$2], [$3])
])


# OMPI_EXT_MAKE_DIR_LIST(subst'ed variable, shell list)
# -------------------------------------------------------------------------
AC_DEFUN([OMPI_EXT_MAKE_DIR_LIST],[
    $1=
    for item in $2 ; do
       $1="$$1 mpiext/$item"
    done
    AC_SUBST($1)
])
