dnl -*- shell-script -*-
dnl
dnl Copyright (c) 2004-2005 The Trustees of Indiana University.
dnl                         All rights reserved.
dnl Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
dnl                         All rights reserved.
dnl Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
dnl                         University of Stuttgart.  All rights reserved.
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

AC_MSG_CHECKING([which components should be run-time loadable])
AC_ARG_ENABLE(mca-dso,
    AC_HELP_STRING([--enable-mca-dso=LIST],
		   [comma-separated list of types and/or type-component pairs that will be built as run-time loadable components (as opposed to statically linked in), if supported on this platform.  The default is to build all components as DSOs; the --disable-mca-dso[=LIST] form can be used to disable building all or some types/components as DSOs]))

# First, check to see if we're only building static libraries.  If so,
# then override everything and only build components as static
# libraries.

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

# The list of MCA types (it's fixed)

AC_MSG_CHECKING([for MCA types])
found_types="common allocator coll errmgr gpr io iof llm mpool ns one oob op pcm pcmclient pml ptl soh svc topo"
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

    # Iterate through the list of no-configure components

    foo="found_components=\$MCA_${type}_NO_CONFIGURE_SUBDIRS"
    eval $foo

    for component in $found_components; do
        m=`basename "$component"`

        # build if:
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

# OMPI types

AC_SUBST(MCA_oob_ALL_SUBDIRS)
AC_SUBST(MCA_oob_STATIC_SUBDIRS)
AC_SUBST(MCA_oob_DSO_SUBDIRS)
AC_SUBST(MCA_oob_STATIC_LTLIBS)

AC_SUBST(MCA_pcm_ALL_SUBDIRS)
AC_SUBST(MCA_pcm_STATIC_SUBDIRS)
AC_SUBST(MCA_pcm_DSO_SUBDIRS)
AC_SUBST(MCA_pcm_STATIC_LTLIBS)

AC_SUBST(MCA_pcmclient_ALL_SUBDIRS)
AC_SUBST(MCA_pcmclient_STATIC_SUBDIRS)
AC_SUBST(MCA_pcmclient_DSO_SUBDIRS)
AC_SUBST(MCA_pcmclient_STATIC_LTLIBS)

AC_SUBST(MCA_errmgr_ALL_SUBDIRS)
AC_SUBST(MCA_errmgr_STATIC_SUBDIRS)
AC_SUBST(MCA_errmgr_DSO_SUBDIRS)
AC_SUBST(MCA_errmgr_STATIC_LTLIBS)

AC_SUBST(MCA_gpr_ALL_SUBDIRS)
AC_SUBST(MCA_gpr_STATIC_SUBDIRS)
AC_SUBST(MCA_gpr_DSO_SUBDIRS)
AC_SUBST(MCA_gpr_STATIC_LTLIBS)

AC_SUBST(MCA_llm_ALL_SUBDIRS)
AC_SUBST(MCA_llm_STATIC_SUBDIRS)
AC_SUBST(MCA_llm_DSO_SUBDIRS)
AC_SUBST(MCA_llm_STATIC_LTLIBS)

AC_SUBST(MCA_ns_ALL_SUBDIRS)
AC_SUBST(MCA_ns_STATIC_SUBDIRS)
AC_SUBST(MCA_ns_DSO_SUBDIRS)
AC_SUBST(MCA_ns_STATIC_LTLIBS)

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

AC_SUBST(MCA_one_ALL_SUBDIRS)
AC_SUBST(MCA_one_STATIC_SUBDIRS)
AC_SUBST(MCA_one_DSO_SUBDIRS)
AC_SUBST(MCA_one_STATIC_LTLIBS)

AC_SUBST(MCA_pml_ALL_SUBDIRS)
AC_SUBST(MCA_pml_STATIC_SUBDIRS)
AC_SUBST(MCA_pml_DSO_SUBDIRS)
AC_SUBST(MCA_pml_STATIC_LTLIBS)

AC_SUBST(MCA_ptl_ALL_SUBDIRS)
AC_SUBST(MCA_ptl_STATIC_SUBDIRS)
AC_SUBST(MCA_ptl_DSO_SUBDIRS)
AC_SUBST(MCA_ptl_STATIC_LTLIBS)

AC_SUBST(MCA_soh_ALL_SUBDIRS)
AC_SUBST(MCA_soh_STATIC_SUBDIRS)
AC_SUBST(MCA_soh_DSO_SUBDIRS)
AC_SUBST(MCA_soh_STATIC_LTLIBS)

AC_SUBST(MCA_svc_ALL_SUBDIRS)
AC_SUBST(MCA_svc_STATIC_SUBDIRS)
AC_SUBST(MCA_svc_DSO_SUBDIRS)
AC_SUBST(MCA_svc_STATIC_LTLIBS)

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

    shared_mode_override=static

    # Setup for either shared or static

    if test "$shared_mode_override" = "dso" -o \
	"$SHARED_TYPE" = "1" -o \
	"$SHARED_GENERIC_TYPE" = "1" -o \
	"$SHARED_COMPONENT" = "1" -o \
	"$DSO_all" = "1"; then
	compile_mode="dso"
	echo $m >> $outfile.dso
	rm -f "src/dynamic-mca/$type/$m"
	$LN_S "$OMPI_TOP_BUILDDIR/src/mca/$type/$m" \
	    "src/dynamic-mca/$type/$m"
    else
	static_ltlibs="mca/$type/$m/libmca_${type}_${m}.la $static_ltlibs"
	echo "extern const mca_base_component_t mca_${type}_${m}_component;" >> $outfile.extern
	echo "  &mca_${type}_${m}_component, " >> $outfile.struct
	compile_mode="static"
	echo $m >> $outfile.static
    fi

    # Output pretty results

    AC_MSG_CHECKING([if MCA component $type:$m can compile])
    AC_MSG_RESULT([yes])
    AC_MSG_CHECKING([for MCA component $type:$m compile mode])
    AC_MSG_RESULT([$compile_mode])

    # If there's an output file, add the values to
    # scope_EXTRA_flags.

    if test -f $infile; then

	# First check for the ABORT tag

	line="`grep ABORT= $infile | cut -d= -f2-`"
	if test -n "$line" -a "$line" != "no"; then
	    AC_MSG_WARN([MCA component configure script told me to abort])
	    AC_MSG_ERROR([cannot continue])
	fi

	# Now check for LIBMPI tags

        for flags in CFLAGS CXXFLAGS FFLAGS LDFLAGS LIBS; do
            var="LIBMPI_EXTRA_${flags}"
            line="`grep $var= $infile | cut -d= -f2-`"
	    eval "line=$line"
            if test -n "$line"; then
                str="$var="'"$'"$var $line"'"'
                eval $str
            fi
        done

        # Finally check for WRAPPER flags, but only if this component
        # is compiling statically

        if test "$compile_mode" = "static"; then
            for flags in LDFLAGS LIBS; do
                var="WRAPPER_EXTRA_${flags}"
                line="`grep $var= $infile | cut -d= -f2-`"
		eval "line=$line"
                if test -n "$line"; then
                    str="$var="'"$'"$var $line"'"'
                    eval $str
                fi
            done
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
