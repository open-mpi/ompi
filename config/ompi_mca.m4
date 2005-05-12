dnl -*- shell-script -*-
dnl
dnl Copyright (c) 2004-2005 The Trustees of Indiana University.
dnl                         All rights reserved.
dnl Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
dnl                         All rights reserved.
dnl Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
dnl                         University of Stuttgart.  All rights reserved.
dnl Copyright (c) 2004-2005 The Regents of the University of California.
dnl                         All rights reserved.
dnl $COPYRIGHT$
dnl 
dnl Additional copyrights may follow
dnl 
dnl $HEADER$
dnl

AC_DEFUN([OMPI_MCA],[

# Find which components should be built as run-time loadable components
# Acceptable combinations:
#
# [default -- no option given]
# --enable-mca-dso
# --enable-mca-dso=[.+,]*COMPONENT_TYPE[.+,]*
# --enable-mca-dso=[.+,]*COMPONENT_TYPE-COMPONENT_NAME[.+,]*
# --disable-mca-dso
#

AC_ARG_ENABLE(mca-dso,
    AC_HELP_STRING([--enable-mca-dso=LIST],
                   [comma-separated list of types and/or
                     type-component pairs that will be built as
                     run-time loadable components (as opposed to
                     statically linked in), if supported on this
                     platform.  The default is to build all components
                    as DSOs; the --disable-mca-dso[=LIST] form can be
                    used to disable building all or some
                    types/components as DSOs]))
AC_ARG_ENABLE(mca-static,
    AC_HELP_STRING([--enable-mca-static=LIST],
                   [comma-separated list of types and/or
                    type-component pairs that will be built statically
                    linked into the library.  The default (if DSOs are
                    supported) is to build all components as DSOs.
                    Enabling a component as static disables it
                    building as a DSO.]))
AC_ARG_ENABLE(mca-direct,
    AC_HELP_STRING([--enable-mca-direct=LIST],
                   [comma-separated list of type-component pairs that
                    will be hard coded as the one component to use for
                    a given component type, saving the (small)
                    overhead of the component architecture.  LIST must
                    not be empty and implies given component pairs are
                    build as static components.]))

#
# First, add all the mca-direct components / types into the mca-static
# lists and create a list of component types that are direct compile,
# in the form DIRECT_[type]=[component]
#
AC_MSG_CHECKING([which components should be direct-linked into the library])
if test "$enable_mca_direct" = "yes" ; then
    AC_MSG_RESULT([yes])
    AC_MSG_ERROR([*** The enable-mca-direct flag requires an explicit list of
*** type-component pairs.  For example, --enable-mca-direct=pml-teg,coll-basic])
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
if test "$enable_shared" = "no"; then
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
if test "$enable_shared" = "no"; then
    AC_MSG_WARN([*** Shared libraries have been disabled (--disable-shared])
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


# The list of MCA types (it's fixed)

AC_MSG_CHECKING([for MCA types])
found_types="common allocator coll errmgr gpr io iof mpool ns oob pls pml ptl ras rds rmaps rmgr rml schema soh topo"
AC_MSG_RESULT([$found_types])

# Get the list of all the non-configure MCA components that were found by
# autogen.sh.

# config/mca_no_configure_components.m4
MCA_FIND_NO_CONFIGURE_COMPONENTS

# Now determine the configurable components in each of the types.  This
# is a little redundant and could be combined into
# MCA_FIND_NO_CONFIGURE_COMPONENTS, but we separate it out for clarity.
# The extern statements and array of pointers to the component global
# structs are written to a file for each type that is #include'd in
# the file for each type.

for type in $found_types; do
    all_components=
    static_components=
    dso_components=
    static_ltlibs=

    # Ensure that the directory where the #include file is to live
    # exists.  Need to do this for VPATH builds, because the directory
    # may not exist yet.  For the "common" type, it's not really a
    # component, so it doesn't have a base.

    if test "$type" = "common"; then
        outdir=src/mca/common
    else
        outdir=src/mca/$type/base
    fi
    total_dir="."
    for dir_part in `IFS='/\\'; set X $outdir; shift; echo "$[@]"`; do
        total_dir=$total_dir/$dir_part
        test -d "$total_dir" ||
        mkdir "$total_dir" ||
        AC_MSG_ERROR([cannot create $total_dir])
    done

    # Also ensure that the dynamic-mca base directory exists

    total_dir="."
    dyndir=src/dynamic-mca/$type
    for dir_part in `IFS='/\\'; set X $dyndir; shift; echo "$[@]"`; do
        total_dir=$total_dir/$dir_part
        test -d "$total_dir" ||
        mkdir "$total_dir" ||
        AC_MSG_ERROR([cannot create $total_dir])
    done

    # Remove any previous generated #include files.

    outfile_real=$outdir/static-components.h
    outfile=$outfile_real.new
    rm -f $outfile $outfile.struct $outfile.extern \
       $outfile.all $outfile.static $outfile.dyanmic
    touch $outfile.struct $outfile.extern \
       $outfile.all $outfile.static $outfile.dso

    # Manual conversion of $type to its generic name (e.g., crmpi->cr,
    # crompi->cr).
    # JMS Fix this

    case "$type" in
    crmpi)
        generic_type="cr"
        ;;
    crompi)
        generic_type="cr"
        ;;
    *)
        generic_type="$type"
        ;;
    esac

    # set the direct / no direct flag
    str="DIRECT_COMPONENT=\$DIRECT_${type}"
    eval $str
    if test ! -z "$DIRECT_COMPONENT" ; then
        str="MCA_${type}_DIRECT_CALL_COMPONENT=$DIRECT_COMPONENT"
        eval $str
        str="MCA_${type}_DIRECT_CALL=1"
        eval $str
    else
        str="MCA_${type}_DIRECT_CALL_COMPONENT="
        eval $str
        str="MCA_${type}_DIRECT_CALL=0"
        eval $str
    fi

    # Iterate through the list of no-configure components

    foo="found_components=\$MCA_${type}_NO_CONFIGURE_SUBDIRS"
    eval $foo

    for component in $found_components; do
        m=`basename "$component"`

        # build if:
        # - the component type is direct and we are that component
        # - there is no ompi_ignore file
        # - there is an ompi_ignore, but there is an empty ompi_unignore
        # - there is an ompi_ignore, but username is in ompi_unignore
        if test -d $srcdir/$component ; then
            # decide if we want the component to be built or not.  This
            # is spread out because some of the logic is a little complex
            # and test's syntax isn't exactly the greatest.  We want to
            # build the component by default.
            want_component=1
            if test -f $srcdir/$component/.ompi_ignore ; then
                # If there is an ompi_ignore file, don't build
                # the component.  Note that this decision can be
                # overriden by the unignore logic below.
                want_component=0
            fi
            if test -f $srcdir/$component/.ompi_unignore ; then
                # if there is an empty ompi_unignore, that is
                # equivalent to having your userid in the unignore file.
                # If userid is in the file, unignore the ignore file.
                if test ! -s $srcdir/$component/.ompi_unignore ; then
                    want_component=1
                elif test ! -z "`grep $USER $srcdir/$component/.ompi_unignore`" ; then
                    want_component=1
                fi
            fi
            # if this component type is direct and we are not it, we don't want
            # to be built.  Otherwise, we do want to be built.
            if test ! -z "$DIRECT_COMPONENT" ; then
                if test "$DIRECT_COMPONENT" = "$m" ; then
                    want_component=1
                else
                    want_component=0
                fi
            fi
            if test "$want_component" = "1" ; then
                ompi_show_subtitle "MCA component $type:$m (no configure script)"

                # Remove any possible sym link in the mca-dynamic tree
            
                rm -f src/dynamic-mca/$type/$m

                # Now process the component

                MCA_PROCESS_COMPONENT(1, 1, $type, $m)

                # Note that the AM_CONDITIONAL for this component is set in
                # config/mca_no_configure_components.m4 -- which is generated by
                # autogen.sh because we cannot have a variable
                # AM_CONDITIONAL name (which we need here).  Since
                # autogen.sh knows the name that is necessary, it just
                # generated the AM_CONDITIONAL directly.  Here, we fill in
                # the variable that is used in that AM_CONDITIONAL.

                if test "$compile_mode" = "dso"; then
                    value=1
                else
                    value=0
                fi
                foo="BUILD_${type}_${m}_DSO=$value"
                eval $foo
                
                # double check that we can build direct if that was requested
                # DIRECT_CALL_HEADER *must* be defined by the component 
                # (in its post configure) if it
                # can be direct built, so we use that as a keyword to tell us
                # whether the component was successfully setup or not
                if test "$DIRECT_COMPONENT" = "$m" -a \
                        -z "$MCA_${type}_DIRECT_CALL_HEADER" ; then
AC_MSG_ERROR([${type} component ${m} was requested to be directly linked
into libmpi, but does not support such a configuration.  Please choose
another ${type} component for direct compilation or allow all components
of type ${type} to be loaded at runtime.])
                fi
            fi
        fi
    done

    # Find all configureable components, run their configure scripts,
    # etc.

    for component in $srcdir/src/mca/$type/*; do
        FOUND=0
        HAPPY=0
        m="`basename $component`"
        # build if:
        # - the component type is direct and we are that component
        # - there is no ompi_ignore file
        # - there is an ompi_ignore, but there is an empty ompi_unignore
        # - there is an ompi_ignore, but username is in ompi_unignore
        if test -d $component -a -x $component/configure ; then
            want_component=1
            if test -f $srcdir/$component/.ompi_ignore ; then
                want_component=0
            fi
            if test -f $srcdir/$component/.ompi_unignore ; then
                if test ! -s $srcdir/$component/.ompi_unignore ; then
                    want_component=1
                elif test ! -z "`grep $USER $srcdir/$component/.ompi_unignore`" ; then
                    want_component=1
                fi
            fi
            # if this component type is direct and we are not it, we don't want
            # to be built.  Otherwise, we do want to be built.
            if test ! -z "$DIRECT_COMPONENT" ; then
                if test "$DIRECT_COMPONENT" = "$m" ; then
                    # BWB - need some check in here to make sure component
                    # can be built direct!
                    want_component=1
                else
                    want_component=0
                fi
            fi
            if test "$want_component" = "1" ; then
                ompi_show_subtitle "MCA component $type:$m (need to configure)"

                # We found one!

                FOUND=1

                # Remove any possible sym link in the mca-dynamic tree

                rm -f src/dyanmic-mca/$type/$m

                # Configure the component subdirectory

                OMPI_CONFIG_SUBDIR([src/mca/$type/$m],
                                    [$ompi_subdir_args], 
                                    [HAPPY=1], [HAPPY=0])
            fi
        fi

        # Process this component

        MCA_PROCESS_COMPONENT($FOUND, $HAPPY, $type, $m)
        # double check that we can build direct if that was requested
        if test "$DIRECT_COMPONENT" = "$m" -a \
               -z "$MCA_${type}_DIRECT_CALL_HEADER" ; then
AC_MSG_ERROR([${type} component ${m} was requested to be directly linked
into libmpi, but does not support such a configuration.  Please choose
another ${type} component for direct compilation or allow all components
of type ${type} to be loaded at runtime.])
        fi

    done

    # m4 weirdness: must also do the echo after the sort, or we get a
    # string with newlines in it

    all_components="`sort $outfile.all`"
    all_components="`echo $all_components`"
    static_components="`sort $outfile.static`"
    static_components="`echo $static_components`"
    dso_components="`sort $outfile.dso`"
    dso_components="`echo $dso_components`"
    rm -f $outfile $outfile.all $outfile.static $outfile.dso

    # Create the final .h file that will be included in the type's
    # top-level glue.  This lists all the static components.  We don't
    # need to do this for "common".

    if test "$type" != "common"; then
        cat > $outfile <<EOF
/*
 * \$HEADER\$
 */

`cat $outfile.extern`

const mca_base_component_t *mca_${type}_base_static_components[[]] = {
`cat $outfile.struct`
  NULL
};
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

    # Save the results for the Makefile.am's.  Note the whacky shell
    # script escaping that is necessary because $components may be
    # multiple words, but we also need to substitute on ${type}...

    foo="MCA_${type}_ALL_SUBDIRS"'="$all_components"'
    eval "$foo"
    foo="MCA_${type}_STATIC_SUBDIRS"'="$static_components"'
    eval "$foo"
    foo="MCA_${type}_DSO_SUBDIRS"'="$dso_components"'
    eval "$foo"
    foo="MCA_${type}_STATIC_LTLIBS"'="$static_ltlibs"'
    eval "$foo"
done
unset foo type m components structs outfile outdir total_dir file \
    all_components static_components dso_components static_ltlibs

# Grumble.  It seems that AC_SUBST and AC_DEFINE don't let you
# substitue on a variable name that contains a variable (e.g.,
# OMPI_MCA_$type_SUBDIRS).  So we have to do this manually.  :-(

# Common types

AC_SUBST(MCA_common_ALL_SUBDIRS)
AC_SUBST(MCA_common_STATIC_SUBDIRS)
AC_SUBST(MCA_common_DSO_SUBDIRS)
AC_SUBST(MCA_common_STATIC_LTLIBS)

# ORTE types

AC_SUBST(MCA_oob_ALL_SUBDIRS)
AC_SUBST(MCA_oob_STATIC_SUBDIRS)
AC_SUBST(MCA_oob_DSO_SUBDIRS)
AC_SUBST(MCA_oob_STATIC_LTLIBS)

AC_SUBST(MCA_dps_ALL_SUBDIRS)
AC_SUBST(MCA_dps_STATIC_SUBDIRS)
AC_SUBST(MCA_dps_DSO_SUBDIRS)
AC_SUBST(MCA_dps_STATIC_LTLIBS)

AC_SUBST(MCA_errmgr_ALL_SUBDIRS)
AC_SUBST(MCA_errmgr_STATIC_SUBDIRS)
AC_SUBST(MCA_errmgr_DSO_SUBDIRS)
AC_SUBST(MCA_errmgr_STATIC_LTLIBS)

AC_SUBST(MCA_gpr_ALL_SUBDIRS)
AC_SUBST(MCA_gpr_STATIC_SUBDIRS)
AC_SUBST(MCA_gpr_DSO_SUBDIRS)
AC_SUBST(MCA_gpr_STATIC_LTLIBS)

AC_SUBST(MCA_ns_ALL_SUBDIRS)
AC_SUBST(MCA_ns_STATIC_SUBDIRS)
AC_SUBST(MCA_ns_DSO_SUBDIRS)
AC_SUBST(MCA_ns_STATIC_LTLIBS)

AC_SUBST(MCA_ras_ALL_SUBDIRS)
AC_SUBST(MCA_ras_STATIC_SUBDIRS)
AC_SUBST(MCA_ras_DSO_SUBDIRS)
AC_SUBST(MCA_ras_STATIC_LTLIBS)

AC_SUBST(MCA_rds_ALL_SUBDIRS)
AC_SUBST(MCA_rds_STATIC_SUBDIRS)
AC_SUBST(MCA_rds_DSO_SUBDIRS)
AC_SUBST(MCA_rds_STATIC_LTLIBS)

AC_SUBST(MCA_rmaps_ALL_SUBDIRS)
AC_SUBST(MCA_rmaps_STATIC_SUBDIRS)
AC_SUBST(MCA_rmaps_DSO_SUBDIRS)
AC_SUBST(MCA_rmaps_STATIC_LTLIBS)

AC_SUBST(MCA_rmgr_ALL_SUBDIRS)
AC_SUBST(MCA_rmgr_STATIC_SUBDIRS)
AC_SUBST(MCA_rmgr_DSO_SUBDIRS)
AC_SUBST(MCA_rmgr_STATIC_LTLIBS)

AC_SUBST(MCA_pls_ALL_SUBDIRS)
AC_SUBST(MCA_pls_STATIC_SUBDIRS)
AC_SUBST(MCA_pls_DSO_SUBDIRS)
AC_SUBST(MCA_pls_STATIC_LTLIBS)

AC_SUBST(MCA_rml_ALL_SUBDIRS)
AC_SUBST(MCA_rml_STATIC_SUBDIRS)
AC_SUBST(MCA_rml_DSO_SUBDIRS)
AC_SUBST(MCA_rml_STATIC_LTLIBS)

AC_SUBST(MCA_schema_ALL_SUBDIRS)
AC_SUBST(MCA_schema_STATIC_SUBDIRS)
AC_SUBST(MCA_schema_DSO_SUBDIRS)
AC_SUBST(MCA_schema_STATIC_LTLIBS)

AC_SUBST(MCA_soh_ALL_SUBDIRS)
AC_SUBST(MCA_soh_STATIC_SUBDIRS)
AC_SUBST(MCA_soh_DSO_SUBDIRS)
AC_SUBST(MCA_soh_STATIC_LTLIBS)

AC_SUBST(MCA_svc_ALL_SUBDIRS)
AC_SUBST(MCA_svc_STATIC_SUBDIRS)
AC_SUBST(MCA_svc_DSO_SUBDIRS)
AC_SUBST(MCA_svc_STATIC_LTLIBS)

# MPI types

AC_SUBST(MCA_allocator_ALL_SUBDIRS)
AC_SUBST(MCA_allocator_STATIC_SUBDIRS)
AC_SUBST(MCA_allocator_DSO_SUBDIRS)
AC_SUBST(MCA_allocator_STATIC_LTLIBS)

AC_SUBST(MCA_coll_ALL_SUBDIRS)
AC_SUBST(MCA_coll_STATIC_SUBDIRS)
AC_SUBST(MCA_coll_DSO_SUBDIRS)
AC_SUBST(MCA_coll_STATIC_LTLIBS)

AC_SUBST(MCA_io_ALL_SUBDIRS)
AC_SUBST(MCA_io_STATIC_SUBDIRS)
AC_SUBST(MCA_io_DSO_SUBDIRS)
AC_SUBST(MCA_io_STATIC_LTLIBS)

AC_SUBST(MCA_iof_ALL_SUBDIRS)
AC_SUBST(MCA_iof_STATIC_SUBDIRS)
AC_SUBST(MCA_iof_DSO_SUBDIRS)
AC_SUBST(MCA_iof_STATIC_LTLIBS)

AC_SUBST(MCA_mpool_ALL_SUBDIRS)
AC_SUBST(MCA_mpool_STATIC_SUBDIRS)
AC_SUBST(MCA_mpool_DSO_SUBDIRS)
AC_SUBST(MCA_mpool_STATIC_LTLIBS)

AC_SUBST(MCA_pml_ALL_SUBDIRS)
AC_SUBST(MCA_pml_STATIC_SUBDIRS)
AC_SUBST(MCA_pml_DSO_SUBDIRS)
AC_SUBST(MCA_pml_STATIC_LTLIBS)
OMPI_SETUP_DIRECT_CALL(pml)

AC_SUBST(MCA_ptl_ALL_SUBDIRS)
AC_SUBST(MCA_ptl_STATIC_SUBDIRS)
AC_SUBST(MCA_ptl_DSO_SUBDIRS)
AC_SUBST(MCA_ptl_STATIC_LTLIBS)

AC_SUBST(MCA_schema_ALL_SUBDIRS)
AC_SUBST(MCA_schema_STATIC_SUBDIRS)
AC_SUBST(MCA_schema_DSO_SUBDIRS)
AC_SUBST(MCA_schema_STATIC_LTLIBS)

AC_SUBST(MCA_topo_ALL_SUBDIRS)
AC_SUBST(MCA_topo_STATIC_SUBDIRS)
AC_SUBST(MCA_topo_DSO_SUBDIRS)
AC_SUBST(MCA_topo_STATIC_LTLIBS)

# Finally, now that we've filled in all the test variables, get all
# the AM_CONDITIONALs that indicate whether to build components as shared
# or static.

# config/mca_no_configure_components.m4
MCA_AMC_NO_CONFIGURE_COMPONENTS])

dnl -----------------------------------------------------------------------
dnl -----------------------------------------------------------------------
dnl -----------------------------------------------------------------------

AC_DEFUN([MCA_PROCESS_COMPONENT],[
FOUND=$1
HAPPY=$2
type=$3
m=$4

# See if it dropped an output file for us to pick up some
# shell variables in.  

infile="src/mca/$type/$m/post_configure.sh"

# Did we find a valid component, and did its configure run
# successfully?

if test "$HAPPY" = "1"; then

    # Add this subdir to the mast list of all MCA component subdirs

    echo $m >> $outfile.all

    # Is this component going to built staic or shared?

    str="SHARED_TYPE=\$DSO_$type"
    eval $str
    str="SHARED_GENERIC_TYPE=\$DSO_$generic_type"
    eval $str
    str="SHARED_COMPONENT=\$DSO_${type}_$m"
    eval $str


    str="STATIC_TYPE=\$STATIC_$type"
    eval $str
    str="STATIC_GENERIC_TYPE=\$STATIC_$generic_type"
    eval $str
    str="STATIC_COMPONENT=\$STATIC_${type}_$m"
    eval $str

    shared_mode_override=static

    # Setup for either shared or static
    if test "$STATIC_TYPE" = "1" -o \
        "$STATIC_GENERIC_TYPE" = "1" -o \
        "$STATIC_COMPONENT" = "1" -o \
        "$STATIC_all" = "1" ; then
        compile_mode="static"
    elif test "$shared_mode_override" = "dso" -o \
        "$SHARED_TYPE" = "1" -o \
        "$SHARED_GENERIC_TYPE" = "1" -o \
        "$SHARED_COMPONENT" = "1" -o \
        "$DSO_all" = "1"; then
        compile_mode="dso"
    else
        compile_mode="static"
    fi

    if test "$compile_mode" = "dso" ; then
        echo $m >> $outfile.dso
        rm -f "src/dynamic-mca/$type/$m"
        $LN_S "$OMPI_TOP_BUILDDIR/src/mca/$type/$m" \
            "src/dynamic-mca/$type/$m"
    else
        static_ltlibs="mca/$type/$m/libmca_${type}_${m}.la $static_ltlibs"
        echo "extern const mca_base_component_t mca_${type}_${m}_component;" >> $outfile.extern
        echo "  &mca_${type}_${m}_component, " >> $outfile.struct
        echo $m >> $outfile.static
    fi

    # Output pretty results

    AC_MSG_CHECKING([if MCA component $type:$m can compile])
    AC_MSG_RESULT([yes])
    AC_MSG_CHECKING([for MCA component $type:$m compile mode])
    if test "$DIRECT_COMPONENT" = "$m" ; then
        AC_MSG_RESULT([$compile_mode - direct])
    else
        AC_MSG_RESULT([$compile_mode])
    fi
    
    # If there's an output file, add the values to
    # scope_EXTRA_flags.

    if test -f $infile; then

        # First check for the ABORT tag

        line="`grep ABORT= $infile | cut -d= -f2-`"
        if test -n "$line" -a "$line" != "no"; then
            AC_MSG_WARN([MCA component configure script told me to abort])
            AC_MSG_ERROR([cannot continue])
        fi

        # If we're not compiling statically, then only take the
        # "ALWAYS" tags (a uniq will be performed at the end -- no
        # need to worry about duplicate flags here)

        for flags in LDFLAGS LIBS; do
            var_in="LIBMPI_ALWAYS_EXTRA_${flags}"
            var_out="LIBMPI_EXTRA_${flags}"
            line="`grep $var_in= $infile | cut -d= -f2-`"
            eval "line=$line"
            if test -n "$line"; then
                str="$var_out="'"$'"$var_out $var_in $line"'"'
                eval $str
            fi
        done

        for flags in CFLAGS CXXFLAGS FFLAGS FCFLAGS LDFLAGS LIBS; do
            var_in="WRAPPER_ALWAYS_EXTRA_${flags}"
            var_out="WRAPPER_EXTRA_${flags}"
            line="`grep $var_in= $infile | cut -d= -f2-`"
            eval "line=$line"
            if test -n "$line"; then
                str="$var_out="'"$'"$var_out $var_in $line"'"'
                eval $str
            fi
        done

        # Check for flags passed up from the component.  If we're
        # compiling statically, then take all flags passed up from the
        # component.

        if test "$compile_mode" = "static"; then
            for flags in LDFLAGS LIBS; do
                var="LIBMPI_EXTRA_${flags}"
                line="`grep $var= $infile | cut -d= -f2-`"
                eval "line=$line"
                if test -n "$line"; then
                    str="$var="'"$'"$var $line"'"'
                    eval $str
                fi
            done

            for flags in CFLAGS CXXFLAGS FFLAGS FCFLAGS LDFLAGS LIBS; do
                var="WRAPPER_EXTRA_${flags}"
                line="`grep $var= $infile | cut -d= -f2-`"
                eval "line=$line"
                if test -n "$line"; then
                    str="$var="'"$'"$var $line"'"'
                    eval $str
                fi
            done
        fi
        dnl check for direct call header to include.  This will be
        dnl AC_SUBSTed later.
        if test "$DIRECT_COMPONENT" = "$m" ; then
            if test "`grep DIRECT_CALL_HEADER $infile`" != "" ; then
                line="`grep DIRECT_CALL_HEADER $infile | cut -d= -f2-`"
                str="MCA_${type}_DIRECT_CALL_HEADER=\\\"$line\\\""
                eval $str
            else
AC_MSG_ERROR([*** ${type} component ${m} was supposed to be direct-called, but
*** does not appear to support direct calling.
*** Aborting])
            fi
        fi
    else
        # were we supposed to have found something in the 
        # post_configure.sh, but the file didn't exist?
        if test "$DIRECT_COMPONENT" = "$m" ; then
AC_MSG_ERROR([*** ${type} component ${m} was supposed to be direct-called, but
*** does not appear to support direct calling.
*** Aborting])
        fi
    fi
elif test "$FOUND" = "1"; then
    AC_MSG_CHECKING([if MCA component $type:$m can compile])
    AC_MSG_RESULT([no])

    # If this component was requested as the default for this
    # type, then abort.

    str="foo="'"$'"with_$type"'"'
    eval $str
    str="bar="'"$'"with_$generic_type"'"'
    eval $str
    if test "$foo" = "$m" -o "$bar" = "$m"; then
        AC_MSG_WARN([MCA component "$m" failed to configure properly])
        AC_MSG_WARN([This component was selected as the default])
        AC_MSG_ERROR([Cannot continue])
        exit 1
    fi
fi
])


AC_DEFUN([OMPI_SETUP_DIRECT_CALL],[
        AC_SUBST(MCA_$1_DIRECT_CALL_HEADER)
        AC_DEFINE_UNQUOTED([MCA_$1_DIRECT_CALL], [$MCA_$1_DIRECT_CALL],
            [Defined to 1 if $1 should use direct calls instead of components])
        AC_DEFINE_UNQUOTED([MCA_$1_DIRECT_CALL_COMPONENT], [$MCA_$1_DIRECT_CALL_COMPONENT],
            [name of component to use for direct calls, if MCA_$1_DIRECT_CALL is 1])
        OMPI_WRITE_DIRECT_CALL_HEADER($1)
])


AC_DEFUN([OMPI_WRITE_DIRECT_CALL_HEADER],[
    AC_CONFIG_FILES(src/mca/$1/$1_direct_call.h.template)
    AC_CONFIG_COMMANDS($1-direct,
[if test -f "src/mca/$1/$1_direct_call"; then
  diff "src/mca/$1/$1_direct_call.h" "src/mca/$1/$1_direct_call.h.template" > /dev/null 2>&1
  if test "$?" != "0"; then
    cp "src/mca/$1/$1_direct_call.h.template" "src/mca/$1/$1_direct_call.h"
    echo "config.status: regenerating src/mca/$1/$1_direct_call.h"
  else
    echo "config.status: src/mca/$1/$1_direct_call.h unchanged"
  fi
else
  cp "src/mca/$1/$1_direct_call.h.template" "src/mca/$1/$1_direct_call.h"
  echo "config.status: creating src/mca/$1/$1_direct_call.h"
fi
rm src/mca/$1/$1_direct_call.h.template])
])
