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
dnl Copyright (c) 2010-2013 Cisco Systems, Inc.  All rights reserved.
dnl $COPYRIGHT$
dnl 
dnl Additional copyrights may follow
dnl 
dnl $HEADER$
dnl

# OMPI_EVAL_ARG(arg)
# ------------------
# evaluates and returns argument
AC_DEFUN([OMPI_EVAL_ARG], [$1])

######################################################################
#
# OMPI_MCA
#
# configure the MCA (modular component architecture).  Works hand in hand
# with Open MPI's autogen.pl, requiring it's specially formatted lists
# of frameworks, components, etc.
#
# USAGE:
#   OMPI_MCA()
#
######################################################################
AC_DEFUN([OMPI_MCA],[
    dnl for OPAL_CONFIGURE_USER env variable
    AC_REQUIRE([OPAL_CONFIGURE_SETUP])

    # Set a special flag so that we can detect if the user calls
    # OPAL_WRAPPER_FLAGS_ADD and error.
    m4_define([mca_component_configure_active], [1])

    # Find which components should be built as run-time loadable components
    # Acceptable combinations:
    #
    # [default -- no option given]
    # --enable-mca-dso
    # --enable-mca-dso=[.+,]*COMPONENT_TYPE[.+,]*
    # --enable-mca-dso=[.+,]*COMPONENT_TYPE-COMPONENT_NAME[.+,]*
    # --disable-mca-dso
    #
    AC_ARG_ENABLE([mca-no-build],
        [AC_HELP_STRING([--enable-mca-no-build=LIST],
                        [Comma-separated list of <type>-<component> pairs 
                         that will not be built.  Example: "--enable-mca-no-build=maffinity,btl-portals" will disable building all maffinity components and the "portals" btl components.])])
    AC_ARG_ENABLE(mca-dso,
        AC_HELP_STRING([--enable-mca-dso=LIST],
                       [Comma-separated list of types and/or
                        type-component pairs that will be built as
                        run-time loadable components (as opposed to
                        statically linked in), if supported on this
                        platform.  The default is to build all components
                        as DSOs.]))
    AC_ARG_ENABLE(mca-static,
        AC_HELP_STRING([--enable-mca-static=LIST],
                       [Comma-separated list of types and/or
                        type-component pairs that will be built statically
                        linked into the library.  The default (if DSOs are
                        supported) is to build all components as DSOs.
                        Enabling a component as static disables it
                        building as a DSO.]))
    AC_ARG_ENABLE(mca-direct,
        AC_HELP_STRING([--enable-mca-direct=LIST],
                       [Comma-separated list of type-component pairs that
                        will be hard coded as the one component to use for
                        a given component type, saving the (small)
                        overhead of the component architecture.  LIST must
                        not be empty and implies given component pairs are
                        build as static components.]))

    AC_MSG_CHECKING([which components should be disabled])
    if test "$enable_mca_no_build" = "yes"; then
        AC_MSG_RESULT([yes])
        AC_MSG_ERROR([*** The enable-mca-no-build flag requires an explicit list
*** of type-component pairs.  For example, --enable-mca-direct=pml-ob1])
    else
        ifs_save="$IFS"
        IFS="${IFS}$PATH_SEPARATOR,"
        msg=
        for item in $enable_mca_no_build; do
            type="`echo $item | cut -s -f1 -d-`"
            comp="`echo $item | cut -s -f2- -d-`"
            if test -z $type ; then
                type=$item
            fi
            if test -z $comp ; then
                str="`echo DISABLE_${type}=1 | sed s/-/_/g`"
                eval $str
                msg="$item $msg"
            else
                str="`echo DISABLE_${type}_${comp}=1 | sed s/-/_/g`"
                eval $str
                msg="$item $msg"
            fi
        done
        IFS="$ifs_save"
    fi
    AC_MSG_RESULT([$msg])
    unset msg

    #
    # First, add all the mca-direct components / types into the mca-static
    # lists and create a list of component types that are direct compile,
    # in the form DIRECT_[type]=[component]
    #
    AC_MSG_CHECKING([which components should be direct-linked into the library])
    if test "$enable_mca_direct" = "yes" ; then
        AC_MSG_RESULT([yes])
        AC_MSG_ERROR([*** The enable-mca-direct flag requires an explicit list of
*** type-component pairs.  For example, --enable-mca-direct=pml-ob1,coll-basic])
    elif test ! -z "$enable_mca_direct" -a "$enable_mca_direct" != "" ; then
        #
        # we need to add this into the static list, unless the static list
        # is everything
        #
        if test "$enable_mca_static" = "no" ; then
            AC_MSG_WARN([*** Re-enabling static component support for direct call])
            enable_mca_static="$enable_mca_direct"
        elif test -z "$enable_mca_static" ; then
            enable_mca_static="$enable_mca_direct"
        elif test "$enable_mca_static" != "yes" ; then
            enable_mca_static="$enable_mca_direct,$enable_mca_static"
        fi

        ifs_save="$IFS"
        IFS="${IFS}$PATH_SEPARATOR,"
        msg=
        for item in $enable_mca_direct; do
            type="`echo $item | cut -f1 -d-`"
            comp="`echo $item | cut -f2- -d-`"
            if test -z $type -o -z $comp ; then
                AC_MSG_ERROR([*** The enable-mca-direct flag requires a
*** list of type-component pairs.  Invalid input detected.])
            else
                str="`echo DIRECT_$type=$comp | sed s/-/_/g`"
                eval $str
                msg="$item $msg"
            fi
        done
        IFS="$ifs_save"
    fi
    AC_MSG_RESULT([$msg])
    unset msg

    #
    # Second, set the DSO_all and STATIC_all variables.  conflict
    # resolution (prefer static) is done in the big loop below
    #
    AC_MSG_CHECKING([which components should be run-time loadable])
    if test "$enable_static" != "no"; then
        DSO_all=0
        msg=none
    elif test -z "$enable_mca_dso" -o "$enable_mca_dso" = "yes"; then
        DSO_all=1
        msg=all
    elif test "$enable_mca_dso" = "no"; then
        DSO_all=0
        msg=none
    else
        DSO_all=0
        ifs_save="$IFS"
        IFS="${IFS}$PATH_SEPARATOR,"
        msg=
        for item in $enable_mca_dso; do
            str="`echo DSO_$item=1 | sed s/-/_/g`"
            eval $str
            msg="$item $msg"
        done
        IFS="$ifs_save"
    fi
    AC_MSG_RESULT([$msg])
    unset msg
    if test "$enable_static" != "no"; then
        AC_MSG_WARN([*** Shared libraries have been disabled (--disable-shared)])
        AC_MSG_WARN([*** Building MCA components as DSOs automatically disabled])
    fi

    AC_MSG_CHECKING([which components should be static])
    if test "$enable_mca_static" = "yes"; then
        STATIC_all=1
        msg=all
    elif test -z "$enable_mca_static" -o "$enable_mca_static" = "no"; then
        STATIC_all=0
        msg=none
    else
        STATIC_all=0
        ifs_save="$IFS"
        IFS="${IFS}$PATH_SEPARATOR,"
        msg=
        for item in $enable_mca_static; do
            str="`echo STATIC_$item=1 | sed s/-/_/g`"
            eval $str
            msg="$item $msg"
        done
        IFS="$ifs_save"
    fi
    AC_MSG_RESULT([$msg])
    unset msg

    AC_MSG_CHECKING([for projects containing MCA frameworks])
    AC_MSG_RESULT([mca_project_list])

    # if there isn't a project list, abort
    m4_ifdef([mca_project_list], [],
             [m4_fatal([Could not find project list - please rerun autogen.pl!])])

    # now configre all the projects, frameworks, and components.  Most
    # of the hard stuff is in here
    MCA_PROJECT_SUBDIRS=
    m4_foreach(mca_project, [mca_project_list], 
               [# BWB: Until projects have seperate configure scripts
                # and can skip running if not desired, just avoid recursing
                # into their sub directory if the project is disabled
                if test "mca_project" = "ompi" || test "mca_project" = "opal" || test "mca_project" = "orte" || test "mca_project" = "oshmem" ; then
                   MCA_PROJECT_SUBDIRS="$MCA_PROJECT_SUBDIRS mca_project"
                fi
                MCA_CONFIGURE_PROJECT(mca_project)])

    # BWB - fix me...  need to automate this somehow
    MCA_SETUP_DIRECT_CALL(ompi, pml)
    MCA_SETUP_DIRECT_CALL(ompi, mtl)

    AC_SUBST(MCA_PROJECT_SUBDIRS)

    m4_undefine([mca_component_configure_active])
])


######################################################################
#
# MCA_CONFIGURE_PROJECT
#
# Configure all frameworks inside the given project name.  Assumes that
# the frameworks are located in [project_name]/mca/[frameworks] and that
# there is an m4_defined list named mca_[project]_framework_list with
# the list of frameworks.
#
# USAGE:
#   MCA_CONFIGURE_PROJECT(project_name)
#
######################################################################
AC_DEFUN([MCA_CONFIGURE_PROJECT],[
    # can't use a variable rename here because these need to be evaled
    # at auto* time.

    ompi_show_subtitle "Configuring MCA for $1"

    AC_MSG_CHECKING([for frameworks for $1])
    AC_MSG_RESULT([mca_$1_framework_list])

    # iterate through the list of frameworks.  There is something
    # funky with m4 foreach if the list is defined, but empty.  It
    # will call the 3rd argument once with an empty value for the
    # first argument.  Protect against calling MCA_CONFIGURE_FRAMEWORK
    # with an empty second argument.  Grrr....
    # if there isn't a project list, abort
    #
    # Also setup two variables for Makefiles:
    #  MCA_project_FRAMEWORKS     - list of frameworks in that project
    #  MCA_project_FRAMEWORK_LIBS - list of libraries (or variables pointing
    #                               to more libraries) that must be included
    #                               in the project's main library
    m4_ifdef([mca_$1_framework_list], [], 
             [m4_fatal([Could not find mca_$1_framework_list - please rerun autogen.pl])])

    MCA_$1_FRAMEWORKS=
    MCA_$1_FRAMEWORKS_SUBDIRS=
    MCA_$1_FRAMEWORK_COMPONENT_ALL_SUBDIRS=
    MCA_$1_FRAMEWORK_COMPONENT_DSO_SUBDIRS=
    MCA_$1_FRAMEWORK_COMPONENT_STATIC_SUBDIRS=
    MCA_$1_FRAMEWORK_LIBS=

    m4_foreach(mca_framework, [mca_$1_framework_list],
               [m4_ifval(mca_framework, 
                         [dnl common has to go up front
                          m4_if(mca_framework, [common],
                                [MCA_$1_FRAMEWORKS="mca_framework $MCA_$1_FRAMEWORKS"
                                 MCA_$1_FRAMEWORKS_SUBDIRS="[mca/]mca_framework $MCA_$1_FRAMEWORKS_SUBDIRS"
                                 MCA_$1_FRAMEWORK_COMPONENT_ALL_SUBDIRS="[\$(MCA_]$1[_]mca_framework[_ALL_SUBDIRS)] $MCA_$1_FRAMEWORK_COMPONENT_ALL_SUBDIRS"
                                 MCA_$1_FRAMEWORK_COMPONENT_DSO_SUBDIRS="[\$(MCA_]$1[_]mca_framework[_DSO_SUBDIRS)] $MCA_$1_FRAMEWORK_COMPONENT_DSO_SUBDIRS"
                                 MCA_$1_FRAMEWORK_COMPONENT_STATIC_SUBDIRS="[\$(MCA_]$1[_]mca_framework[_STATIC_SUBDIRS)] $MCA_$1_FRAMEWORK_COMPONENT_STATIC_SUBDIRS"
                                ], [
                                 MCA_$1_FRAMEWORKS="$MCA_$1_FRAMEWORKS mca_framework"
                                 MCA_$1_FRAMEWORKS_SUBDIRS="$MCA_$1_FRAMEWORKS_SUBDIRS [mca/]mca_framework"
                                 MCA_$1_FRAMEWORK_COMPONENT_ALL_SUBDIRS="$MCA_$1_FRAMEWORK_COMPONENT_ALL_SUBDIRS [\$(MCA_]$1[_]mca_framework[_ALL_SUBDIRS)]"
                                 MCA_$1_FRAMEWORK_COMPONENT_DSO_SUBDIRS="$MCA_$1_FRAMEWORK_COMPONENT_DSO_SUBDIRS [\$(MCA_]$1[_]mca_framework[_DSO_SUBDIRS)]"
                                 MCA_$1_FRAMEWORK_COMPONENT_STATIC_SUBDIRS="$MCA_$1_FRAMEWORK_COMPONENT_STATIC_SUBDIRS [\$(MCA_]$1[_]mca_framework[_STATIC_SUBDIRS)]"
                                 MCA_$1_FRAMEWORK_LIBS="$MCA_$1_FRAMEWORK_LIBS [mca/]mca_framework[/libmca_]mca_framework[.la]"])
                          MCA_$1_FRAMEWORK_LIBS="$MCA_$1_FRAMEWORK_LIBS [\$(MCA_]$1[_]mca_framework[_STATIC_LTLIBS)]"
                          m4_ifdef([MCA_]$1[_]mca_framework[_CONFIG],
                                   [MCA_]$1[_]mca_framework[_CONFIG]($1, mca_framework),
                                   [MCA_CONFIGURE_FRAMEWORK($1, mca_framework, 1)])])])

    # note that mca_wrapper_extra_* is a running list, and we take checkpoints at the end of our project
    $1_mca_wrapper_extra_cppflags="$mca_wrapper_extra_cppflags"
    $1_mca_wrapper_extra_ldflags="$mca_wrapper_extra_ldflags"
    $1_mca_wrapper_extra_libs="$mca_wrapper_extra_libs"

    AC_SUBST(MCA_$1_FRAMEWORKS)
    AC_SUBST(MCA_$1_FRAMEWORKS_SUBDIRS)
    AC_SUBST(MCA_$1_FRAMEWORK_COMPONENT_ALL_SUBDIRS)
    AC_SUBST(MCA_$1_FRAMEWORK_COMPONENT_DSO_SUBDIRS)
    AC_SUBST(MCA_$1_FRAMEWORK_COMPONENT_STATIC_SUBDIRS)
    AC_SUBST(MCA_$1_FRAMEWORK_LIBS)
])

# MCA_ORDER_COMPONENT_LIST(project_name, framework_name)
AC_DEFUN([MCA_ORDER_COMPONENT_LIST], [
    m4_foreach(mca_component, [mca_$1_$2_m4_config_component_list],
               [m4_ifval(mca_component,
                    [m4_ifdef([MCA_]$1[_]$2[_]mca_component[_PRIORITY], [], 
                         [m4_fatal([MCA_$1_$2_]mca_component[_PRIORITY not found, but required.])])])])
    m4_define([component_list], 
              [esyscmd([config/ompi_mca_priority_sort.pl] m4_foreach([mca_component], [mca_$1_$2_m4_config_component_list],
                        [m4_ifval(mca_component, [mca_component ]OMPI_EVAL_ARG([MCA_]$1[_]$2[_]mca_component[_PRIORITY ]))]))])
])

AC_DEFUN([MCA_CHECK_IGNORED_PRIORITY], [
    m4_foreach(mca_component, [mca_$1_$2_m4_config_component_list],
               [m4_ifval(mca_component,
                    [m4_ifdef([MCA_]$1[_]$2[_]mca_component[_PRIORITY],
                         [m4_warn([unsupported], [MCA_]$1[_]$2[_]mca_component[_PRIORITY found, but ignored.])])])])
])


######################################################################
#
# MCA_CONFIGURE_FRAMEWORK
#
# Configure the given framework and all components inside the
# framework.  Assumes that the framework is located in
# [project_name]/mca/[framework], and that all components are
# available under the framework directory.  Will configure all
# no-configure and builtin components, then search for components with
# configure scripts.  Assumes that no component is marked as builtin
# AND has a configure script.
#
# USAGE:
#   MCA_CONFIGURE_PROJECT(project_name, framework_name, allow_succeed)
#
######################################################################
AC_DEFUN([MCA_CONFIGURE_FRAMEWORK],[
    ompi_show_subsubtitle "Configuring MCA framework $2"

    m4_ifdef([mca_$1_$2_no_config_component_list], [], 
             [m4_fatal([Could not find mca_$1_$2_no_config_component_list - please rerun autogen.pl])])
    m4_ifdef([mca_$1_$2_m4_config_component_list], [], 
             [m4_fatal([Could not find mca_$1_$2_m4_config_component_list - please rerun autogen.pl])])

    # setup for framework
    all_components=
    static_components=
    dso_components=
    static_ltlibs=

    # Ensure that the directory where the #include file is to live
    # exists.  Need to do this for VPATH builds, because the directory
    # may not exist yet.  For the "common" type, it's not really a
    # component, so it doesn't have a base.
    m4_if([$2], [common], [outdir=$1/mca/common], [outdir=$1/mca/$2/base])
    AS_MKDIR_P([$outdir])

    # emit Makefile rule
    AC_CONFIG_FILES([$1/mca/$2/Makefile])

    # remove any previously generated #include files
    outfile_real=$outdir/static-components.h
    outfile=$outfile_real.new
    rm -f $outfile $outfile.struct $outfile.extern
    touch $outfile.struct $outfile.extern

    # print some nice messages about what we're about to do...
    AC_MSG_CHECKING([for no configure components in framework $2])
    AC_MSG_RESULT([mca_$1_$2_no_config_component_list])
    AC_MSG_CHECKING([for m4 configure components in framework $2])
    AC_MSG_RESULT([mca_$1_$2_m4_config_component_list])

    # If there are components in the no configure list, but we're
    # doing one of the "special" selection logics, abort with a
    # reasonable message.
    m4_if(OMPI_EVAL_ARG([MCA_$1_$2_CONFIGURE_MODE]), [STOP_AT_FIRST],
          [m4_ifval(mca_$1_$2_no_config_component_list,
                   [m4_fatal([Framework $2 using STOP_AT_FIRST but at least one component has no configure.m4])])])
    m4_if(OMPI_EVAL_ARG([MCA_$1_$2_CONFIGURE_MODE]), [STOP_AT_FIRST_PRIORITY],
          [m4_ifval(mca_$1_$2_no_config_component_list,
                   [m4_fatal([Framework $2 using STOP_AT_FIRST_PRIORITY but at least one component has no configure.m4])])])
    m4_if(OMPI_EVAL_ARG([MCA_$1_$2_CONFIGURE_MODE]), [PRIORITY],
          [m4_ifval(mca_$1_$2_no_config_component_list,
                   [m4_fatal([Framework $2 using PRIORITY but at least one component has no configure.m4])])])
    # run the configure logic for the no-config components
    m4_foreach(mca_component, [mca_$1_$2_no_config_component_list],
               [m4_ifval(mca_component,
                  [MCA_CONFIGURE_NO_CONFIG_COMPONENT($1, $2, mca_component, 
                                                     [all_components],
                                                     [static_components],
                                                     [dso_components],
                                                     [static_ltlibs],
                                                     [$3])])])

    # configure components that use built-in configuration scripts
    m4_ifdef([component_list], [m4_undefine([component_list])])
    m4_if(OMPI_EVAL_ARG([MCA_$1_$2_CONFIGURE_MODE]), [STOP_AT_FIRST], [MCA_ORDER_COMPONENT_LIST($1, $2)],
          [m4_if(OMPI_EVAL_ARG([MCA_$1_$2_CONFIGURE_MODE]), [STOP_AT_FIRST_PRIORITY], [MCA_ORDER_COMPONENT_LIST($1, $2)],
                [m4_if(OMPI_EVAL_ARG([MCA_$1_$2_CONFIGURE_MODE]), [PRIORITY], [MCA_ORDER_COMPONENT_LIST($1, $2)],
                       [m4_define([component_list], [mca_$1_$2_m4_config_component_list])])])])

    best_mca_component_priority=0
    components_looking_for_succeed=$3
    components_last_result=0
    m4_foreach(mca_component, [component_list],
               [m4_ifval(mca_component,
                  [m4_if(OMPI_EVAL_ARG([MCA_$1_$2_CONFIGURE_MODE]), [STOP_AT_FIRST_PRIORITY],
                         [AS_IF([test $best_mca_component_priority -gt MCA_$1_$2_]mca_component[_PRIORITY], [components_looking_for_succeed=0])])
                   MCA_CONFIGURE_M4_CONFIG_COMPONENT($1, $2, mca_component, 
                                                     [all_components],
                                                     [static_components],
                                                     [dso_components],
                                                     [static_ltlibs],
                                                     [$components_looking_for_succeed],
                                                     [components_last_result=1],
                                                     [components_last_result=0])
                   m4_if(OMPI_EVAL_ARG([MCA_$1_$2_CONFIGURE_MODE]), [STOP_AT_FIRST],
                         [AS_IF([test $components_last_result -eq 1], [components_looking_for_succeed=0])])
                   m4_if(OMPI_EVAL_ARG([MCA_$1_$2_CONFIGURE_MODE]), [STOP_AT_FIRST_PRIORITY],
                         [AS_IF([test $components_last_result -eq 1], [best_mca_component_priority=]OMPI_EVAL_ARG([MCA_$1_$2_]mca_component[_PRIORITY]))])
                   ])])

    # configure components that provide their own configure script.
    # It would be really hard to run these for "find first that
    # works", so we don't :)
    m4_if(OMPI_EVAL_ARG([MCA_$1_]$2[_CONFIGURE_MODE]), [STOP_AT_FIRST], [],
        [m4_if(OMPI_EVAL_ARG([MCA_$1_]$2[_CONFIGURE_MODE]), [STOP_AT_FIRST_PRIORITY], [],
             [m4_if(OMPI_EVAL_ARG([MCA_$1_]$2[_CONFIGURE_MODE]), [PRIORITY], [],
                 [MCA_CHECK_IGNORED_PRIORITY($1, $2)
                  AS_IF([test "$3" != "0"],
                        [MCA_CONFIGURE_ALL_CONFIG_COMPONENTS($1, $2, [all_components],
                                               [static_components], [dso_components],
                                               [static_ltlibs])])])])])

    MCA_$1_$2_ALL_COMPONENTS="$all_components"
    MCA_$1_$2_STATIC_COMPONENTS="$static_components"
    MCA_$1_$2_DSO_COMPONENTS="$dso_components"
    MCA_$1_$2_STATIC_LTLIBS="$static_ltlibs"

    AC_SUBST(MCA_$1_$2_ALL_COMPONENTS)
    AC_SUBST(MCA_$1_$2_STATIC_COMPONENTS)
    AC_SUBST(MCA_$1_$2_DSO_COMPONENTS)
    AC_SUBST(MCA_$1_$2_STATIC_LTLIBS)

    OMPI_MCA_MAKE_DIR_LIST(MCA_$1_$2_ALL_SUBDIRS, $2, [$all_components])
    OMPI_MCA_MAKE_DIR_LIST(MCA_$1_$2_STATIC_SUBDIRS, $2, [$static_components])
    OMPI_MCA_MAKE_DIR_LIST(MCA_$1_$2_DSO_SUBDIRS, $2, [$dso_components])

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

const mca_base_component_t *mca_$2_base_static_components[[]] = {
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

    unset all_components static_components dso_components outfile outfile_real
])


######################################################################
#
# MCA_CONFIGURE_NO_CONFIG_COMPONENT
#
# Configure the given framework and all components inside the framework.
# Assumes that the framework is located in [project_name]/mca/[framework],
# and that all components are available under the framework directory.
# Will configure all builtin components, then search for components with
# configure scripts.  Assumes that no component is marked as builtin
# AND has a configure script.
#
# USAGE:
#   MCA_CONFIGURE_PROJECT(project_name, framework_name, component_name
#                         all_components_variable, 
#                         static_components_variable,
#                         dso_components_variable,
#                         static_ltlibs_variable,
#                         allowed_to_succeed)
#
######################################################################
AC_DEFUN([MCA_CONFIGURE_NO_CONFIG_COMPONENT],[
    ompi_show_subsubsubtitle "MCA component $2:$3 (no configuration)"

    MCA_COMPONENT_BUILD_CHECK($1, $2, $3, 
                              [should_build=$8], [should_build=0])
    MCA_COMPONENT_COMPILE_MODE($1, $2, $3, compile_mode)

    if test "$should_build" = "1" ; then
        MCA_PROCESS_COMPONENT($1, $2, $3, $4, $5, $6, $7, $compile_mode)
    else
        MCA_PROCESS_DEAD_COMPONENT($1, $2, $3)
        # add component to all component list
        $4="$$4 $3"
    fi

    # set the AM_CONDITIONAL on how we should build
    if test "$compile_mode" = "dso" ; then
        BUILD_$1_$2_$3_DSO=1
    else
        BUILD_$1_$2_$3_DSO=0
    fi
    AM_CONDITIONAL(MCA_BUILD_$1_$2_$3_DSO, test "$BUILD_$1_$2_$3_DSO" = "1")

    AC_CONFIG_FILES([$1/mca/$2/$3/Makefile])

    unset compile_mode
])


######################################################################
#
# MCA_CONFIGURE_M4_CONFIG_COMPONENT
#
#
# USAGE:
#   MCA_CONFIGURE_PROJECT(project_name, framework_name, component_name
#                         all_components_variable, 
#                         static_components_variable,
#                         dso_components_variable,
#                         static_ltlibs_variable,
#                         allowed_to_succeed,
#                         [eval if should build], 
#                         [eval if should not build])
#
######################################################################
AC_DEFUN([MCA_CONFIGURE_M4_CONFIG_COMPONENT],[
    m4_ifdef([MCA_$1_$2_$3_PRIORITY],
        [ompi_show_subsubsubtitle "MCA component $2:$3 (m4 configuration macro, priority MCA_$1_$2_$3_PRIORITY)"],
        [ompi_show_subsubsubtitle "MCA component $2:$3 (m4 configuration macro)"])

    MCA_COMPONENT_BUILD_CHECK($1, $2, $3, [should_build=$8], [should_build=0])
    # Allow the component to override the build mode if it really wants to.
    # It is, of course, free to end up calling MCA_COMPONENT_COMPILE_MODE
    m4_ifdef([MCA_$1_$2_$3_COMPILE_MODE],
             [MCA_$1_$2_$3_COMPILE_MODE($1, $2, $3, compile_mode)],
             [MCA_COMPONENT_COMPILE_MODE($1, $2, $3, compile_mode)])

    # try to configure the component.  pay no attention to
    # --enable-dist, since we'll always have makefiles.
    m4_ifdef([MCA_$1_$2_$3_CONFIG],
             [MCA_$1_$2_$3_CONFIG([should_build=$should_build], 
                                  [should_build=0])],
             [m4_fatal([MCA_$1_$2_$3_CONFIG macro not found])])

    AS_IF([test "$should_build" = "1"],
          [MCA_PROCESS_COMPONENT($1, $2, $3, $4, $5, $6, $7, $compile_mode)],
          [MCA_PROCESS_DEAD_COMPONENT($1, $2, $3)
           # add component to all component list
           $4="$$4 $3"])

    m4_ifdef([MCA_$1_$2_$3_POST_CONFIG],
             [ MCA_$1_$2_$3_POST_CONFIG($should_build)])

    # set the AM_CONDITIONAL on how we should build
    AS_IF([test "$compile_mode" = "dso"], 
          [BUILD_$1_$2_$3_DSO=1],
          [BUILD_$1_$2_$3_DSO=0])
    AM_CONDITIONAL(MCA_BUILD_$1_$2_$3_DSO, test "$BUILD_$1_$2_$3_DSO" = "1")

    AS_IF([test "$should_build" = "1"], [$9], [$10])

    unset compile_mode
])


######################################################################
#
# MCA_CONFIGURE_ALL_CONFIG_COMPONENTS
#
# configure all components in the given framework that have configure
# scripts and should be configured according to the usual rules...
#
# USAGE:
#   MCA_CONFIGURE_ALL_CONFIG_COMPONENTS(project_name, 
#                         framework_name,
#                         all_components_variable, 
#                         static_components_variable,
#                         dso_components_variable,
#                         static_ltlibs_variable)
#
######################################################################
AC_DEFUN([MCA_CONFIGURE_ALL_CONFIG_COMPONENTS],[
    for component_path in $srcdir/$1/mca/$2/* ; do
        component="`basename $component_path`"
        if test -d $component_path -a -x $component_path/configure ; then
            ompi_show_subsubsubtitle "MCA component $2:$component (need to configure)"

            MCA_COMPONENT_BUILD_CHECK($1, $2, $component, 
                                      [should_build=1], [should_build=0])
            MCA_COMPONENT_COMPILE_MODE($1, $2, $component, compile_mode)

            if test "$should_build" = "1" ; then
                OMPI_CONFIG_SUBDIR([$1/mca/$2/$component],
                                   [$ompi_subdir_args],
                                   [should_build=1], [should_build=0])
            fi

            if test "$should_build" = "1" ; then
                # do some extra work to pass flags back from the
                # top-level configure, the way a configure.m4
                # component would.
                infile="$srcdir/$1/mca/$2/$3/post_configure.sh"
                if test -f $infile; then

                    # First check for the ABORT tag
                    line="`$GREP ABORT= $infile | cut -d= -f2-`"
                    if test -n "$line" -a "$line" != "no"; then
                        AC_MSG_WARN([MCA component configure script told me to abort])
                        AC_MSG_ERROR([cannot continue])
                    fi

                    m4_foreach(flags, [LDFLAGS, LIBS],
                        [[line="`$GREP WRAPPER_EXTRA_]flags[= $infile | cut -d= -f2-`"]
                            eval "line=$line"
                            if test -n "$line"; then
                                $2[_]$3[_WRAPPER_EXTRA_]flags[="$line"]
                            fi
                        ])dnl
                fi

                MCA_PROCESS_COMPONENT($1, $2, $component, $3, $4, $5, $6, $compile_mode)
            else
                MCA_PROCESS_DEAD_COMPONENT($1, $2, $component)
            fi
        fi
    done
])


# MCA_COMPONENT_COMPILE_MODE(project_name (1), framework_name (2),
#                            component_name (3), compile_mode_variable (4))
# -------------------------------------------------------------------------
# set compile_mode_variable to the compile mode for the given component
#
#   NOTE: component_name may not be determined until runtime....
AC_DEFUN([MCA_COMPONENT_COMPILE_MODE],[
    SHARED_FRAMEWORK="$DSO_$2"
    AS_LITERAL_IF([$3],
        [SHARED_COMPONENT="$DSO_$2_$3"],
        [str="SHARED_COMPONENT=\$DSO_$2_$3"
         eval $str])

    STATIC_FRAMEWORK="$STATIC_$2"
    AS_LITERAL_IF([$3],
        [STATIC_COMPONENT="$STATIC_$2_$3"],
        [str="STATIC_COMPONENT=\$STATIC_$2_$3"
         eval $str])

    shared_mode_override=static

    # Setup for either shared or static
    if test "$STATIC_FRAMEWORK" = "1" -o \
        "$STATIC_COMPONENT" = "1" -o \
        "$STATIC_all" = "1" ; then
        $4="static"
    elif test "$shared_mode_override" = "dso" -o \
        "$SHARED_FRAMEWORK" = "1" -o \
        "$SHARED_COMPONENT" = "1" -o \
        "$DSO_all" = "1"; then
        $4="dso"
    else
        $4="static"
    fi

    AC_MSG_CHECKING([for MCA component $2:$3 compile mode])
    if test "$DIRECT_$2" = "$3" ; then
        AC_MSG_RESULT([$$4 - direct])
    else
        AC_MSG_RESULT([$$4])
    fi
])


# MCA_PROCESS_COMPONENT(project_name(1), framework_name (2), component_name (3),
#                        all_components_variable (4), static_components_variable (5)
#                        dso_components_variable (6), static_ltlibs_variable (7),
#                        compile_mode_variable (8))
#---------------------------------------------------------------------
# Final setup work for a given component.  It should be known before
# calling that this component can build properly (and exists)
#
#   NOTE: component_name may not be determined until runtime....
AC_DEFUN([MCA_PROCESS_COMPONENT],[
    AC_REQUIRE([AC_PROG_GREP])

    # See if it dropped an output file for us to pick up some
    # shell variables in.  
    infile="$srcdir/$1/mca/$2/$3/post_configure.sh"

    # Add this subdir to the mast list of all MCA component subdirs
    $4="$$4 $3"

    if test "$8" = "dso" ; then
        $6="$$6 $3"
    else
        $7="mca/$2/$3/libmca_$2_$3.la $$7"
        echo "extern const mca_base_component_t mca_$2_$3_component;" >> $outfile.extern
        echo "  &mca_$2_$3_component, " >> $outfile.struct
        $5="$$5 $3"
    fi

    # Output pretty results
    AC_MSG_CHECKING([if MCA component $2:$3 can compile])
    AC_MSG_RESULT([yes])
    
    dnl BWB: FIX ME: We still use the post_configure.sh for frameworks that use the direct call infrastructure.
    dnl All other uses we can ignore here, because config_components will have read it in and set all the
    dnl proper environment variables.  At some point, we should handle the direct call stuff the same way we
    dnl handle the headers for static components like timers in opal, ie, have a framework level configure.m4 that
    dnl does the right thing
    if test -f $infile; then
        # check for direct call header to include.  This will be
        # AC_SUBSTed later.
        if test "$DIRECT_$2" = "$3" ; then
            if test "`$GREP DIRECT_CALL_HEADER $infile`" != "" ; then
                line="`$GREP DIRECT_CALL_HEADER $infile | cut -d= -f2-`"
                str="MCA_$1_$2_DIRECT_CALL_HEADER=$line"
                eval $str
            else
AC_MSG_ERROR([*** $2 component $3 was supposed to be direct-called, but
*** does not appear to support direct calling.
*** Aborting])
            fi
        fi
    else
        # were we supposed to have found something in the 
        # post_configure.sh, but the file didn't exist?
        if test "$DIRECT_$2" = "$3" ; then
AC_MSG_ERROR([*** $2 component $3 was supposed to be direct-called, but
*** does not appear to support direct calling.
*** Aborting])
        fi
    fi

    # if the component is building, add it's WRAPPER_EXTRA_LDFLAGS and
    # WRAPPER_EXTRA_LIBS.  If the component doesn't specify it's
    # WRAPPER_EXTRA_LIBS and WRAPPER_EXTRA_LDFLAGS, try using LDFLAGS and LIBS if
    # component didn't have it's own configure script (in which case,
    # we know it didn't set LDFLAGS and LIBS because it can't) Don't
    # have to do this if the component is building dynamically,
    # because it will link against these (without a dependency from
    # libmpi.so to these flags)
    if test "$8" = "static"; then
        AS_LITERAL_IF([$3], 
            [m4_foreach(flags, [LDFLAGS, LIBS],
                    [AS_IF([test "$$2_$3_WRAPPER_EXTRA_]flags[" = ""],
                           [OPAL_FLAGS_APPEND_UNIQ([mca_wrapper_extra_]m4_tolower(flags), [$$2_$3_]flags)],
                           [OPAL_FLAGS_APPEND_UNIQ([mca_wrapper_extra_]m4_tolower(flags), [$$2_$3_WRAPPER_EXTRA_]flags)])
                        ])],
            [m4_foreach(flags, [LDFLAGS, LIBS],
                    [[str="line=\$$2_$3_WRAPPER_EXTRA_]flags["]
                      eval "$str"
                      OPAL_FLAGS_APPEND_UNIQ([mca_wrapper_extra_]m4_tolower(flags), [$line])])])
    fi

    # if needed, copy over WRAPPER_EXTRA_CPPFLAGS.  Since a configure script
    # component can never be used in a STOP_AT_FIRST framework, we
    # don't have to implement the else clause in the literal check...
    AS_LITERAL_IF([$3],
        [AS_IF([test "$$2_$3_WRAPPER_EXTRA_CPPFLAGS" != ""], 
           [m4_if(OMPI_EVAL_ARG([MCA_$1_$2_CONFIGURE_MODE]), [STOP_AT_FIRST], [stop_at_first=1], [stop_at_first=0])
            AS_IF([test "$8" = "static" -a "$stop_at_first" = "1"],
              [AS_IF([test "$with_devel_headers" = "yes"], 
                     [OPAL_FLAGS_APPEND_UNIQ([mca_wrapper_extra_cppflags], [$$2_$3_WRAPPER_EXTRA_CPPFLAGS])])],
              [AC_MSG_WARN([ignoring $2_$3_WRAPPER_EXTRA_CPPFLAGS ($$2_$3_WRAPPER_EXTRA_CPPFLAGS): component conditions not met])])])])
])


# MCA_PROCESS_DEAD_COMPONENT(project_name (1), framework_name (2),
#                            component_name (3))
# ----------------------------------------------------------------
# Finall setup work for a component that can not be built.  Do the
# last minute checks to make sure the user isn't doing something
# stupid.
#
#   NOTE: component_name may not be determined until runtime....
AC_DEFUN([MCA_PROCESS_DEAD_COMPONENT],[
    AC_MSG_CHECKING([if MCA component $2:$3 can compile])
    AC_MSG_RESULT([no])

    # If this component was requested as the default for this
    # type, then abort.
    if test "$with_$2" = "$3" ; then
        AC_MSG_WARN([MCA component "$3" failed to configure properly])
        AC_MSG_WARN([This component was selected as the default])
        AC_MSG_ERROR([Cannot continue])
    fi

    if test ! -z "$DIRECT_$2" ; then
        if test "$DIRECT_$2" = "$3" ; then
            AC_MSG_WARN([MCA component "$3" failed to configure properly])
            AC_MSG_WARN([This component was selected as the default (direct call)])
            AC_MSG_ERROR([Cannot continue])
        fi
    fi
])


# MCA_COMPONENT_BUILD_CHECK(project_name (1), framework_name(2), 
#                           component_name (3), action-if-build (4)
#                           action-if-not-build (5)
# -----------------------------------------------------------------
# checks the standard rules of component building to see if the 
# given component should be built.
#
# Note: component_name may not be determined until runtime....
AC_DEFUN([MCA_COMPONENT_BUILD_CHECK],[
    AC_REQUIRE([AC_PROG_GREP])

    component_path="$srcdir/$1/mca/$2/$3"
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
        # if this component type is direct and we are not it, we don't want
        # to be built.  Otherwise, we do want to be built.
        if test ! -z "$DIRECT_$2" ; then
            if test "$DIRECT_$2" = "$3" ; then
                want_component=1
            else
                want_component=0
            fi
        fi
    fi

    # if we were explicitly disabled, don't build :)
    AS_IF([test "$DISABLE_$2" = "1"], [want_component=0])
    AS_LITERAL_IF([$3],
        [AS_IF([test "$DISABLE_$2_$3" = "1"], [want_component=0])],
        [str="DISABLED_COMPONENT_CHECK=\$DISABLE_$2_$3"
         eval $str
         if test "$DISABLED_COMPONENT_CHECK" = "1" ; then
             want_component=0
         fi])

    AS_IF([test "$want_component" = "1"], [$4], [$5])
])


# MCA_SETUP_DIRECT_CALL(project_name (1), framework_name  (2))
# -------------------------------------------------------------
AC_DEFUN([MCA_SETUP_DIRECT_CALL],[
    if test ! -z "$DIRECT_$2" ; then
        MCA_$1_$2_DIRECT_CALL_COMPONENT=$DIRECT_$2
        MCA_$1_$2_DIRECT_CALL=1
    else
        MCA_$1_$2_DIRECT_CALL_COMPONENT=
        MCA_$1_$2_DIRECT_CALL=0
    fi

    AC_SUBST(MCA_$1_$2_DIRECT_CALL_HEADER)
    AC_DEFINE_UNQUOTED([MCA_$1_$2_DIRECT_CALL], [$MCA_$1_$2_DIRECT_CALL],
            [Defined to 1 if $1:$2 should use direct calls instead of components])
    AC_DEFINE_UNQUOTED([MCA_$1_$2_DIRECT_CALL_COMPONENT], [$MCA_$1_$2_DIRECT_CALL_COMPONENT],
            [name of component to use for direct calls, if MCA_$1_$2_DIRECT_CALL is 1])
    AC_DEFINE_UNQUOTED([MCA_$1_$2_DIRECT_CALL_HEADER],
                       ["[$MCA_]$1[_]$2[_DIRECT_CALL_HEADER]"],
                       [Header $1:$2 includes to be direct called])
])


# OMPI_MCA_MAKE_DIR_LIST(subst'ed variable, framework, shell list)
# -------------------------------------------------------------------------
AC_DEFUN([OMPI_MCA_MAKE_DIR_LIST],[
    $1=
    for item in $3 ; do
       $1="$$1 mca/$2/$item"
    done
    AC_SUBST($1)
])
