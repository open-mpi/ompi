#! /bin/bash 
#
# $HEADER$
#
# This script is run on developer copies of LAM/MPI -- *not*
# distribution tarballs.


##############################################################################
#
# User-definable parameters (search path and minimum supported versions)
#
##############################################################################

lam_aclocal_search="aclocal"
lam_autoheader_search="autoheader"
lam_autoconf_search="autoconf"
lam_libtoolize_search="libtoolize glibtoolize"
lam_automake_search="automake"

lam_automake_version="1.7"
lam_autoconf_version="2.58"
lam_libtool_version="1.5"


##############################################################################
#
# Global variables - should not need to modify defaults
#
##############################################################################

lam_aclocal_version="$lam_automake_version"
lam_autoheader_version="$lam_autoconf_version"
lam_libtoolize_version="$lam_libtool_version"

# program names to execute
lam_aclocal=""
lam_autoheader=""
lam_autoconf=""
lam_libtoolize=""
lam_automake=""

mca_no_configure_modules_file="config/mca_no_configure_modules.m4"
mca_no_config_list_file="mca_no_config_list"
mca_no_config_amc_file="mca_no_config_amc"


############################################################################
#
# Version check - does major,minor,release check (hopefully ignoring
# beta et al)
#
# INPUT:
#    - minimum version allowable
#    - version we found
#
# OUTPUT:
#    - 0 version is ok
#    - 1 version is not ok
#
# SIDE EFFECTS:
#    none
#
##############################################################################
check_version() {
    local min_version="$1"
    local version="$2"

    local min_major_version="`echo $min_version | cut -f1 -d.`"
    local min_minor_version="`echo $min_version | cut -f2 -d.`"
    local min_release_version="`echo $min_version | cut -f3 -d.`"
    if test "$min_release_version" = "" ; then
        min_release_version=0
    fi

    local major_version="`echo $version | cut -f1 -d.`"
    local minor_version="`echo $version | cut -f2 -d.`"
    local release_version="`echo $version | cut -f3 -d.`"
    if test "$release_version" = "" ; then
        release_version=0
    fi

    if test $min_major_version -lt $major_version ; then
        return 0
    elif test $min_major_version -gt $major_version ; then
        return 1
    fi

    if test $min_minor_version -lt $minor_version ; then
        return 0
    elif test $min_minor_version -gt $minor_version ; then
        return 1
    fi

    if test $min_release_version -gt $release_version ; then
        return 1
    fi

    return 0
}


##############################################################################
#
# find app - find a version of the given application that is new
# enough for use
#
# INPUT:
#    - name of application (eg aclocal)
#
# OUTPUT:
#    none
#
# SIDE EFFECTS:
#    - sets application_name variable to working executable name
#    - aborts on error finding application
#
##############################################################################
find_app() {
    local app_name="$1"

    local version="0.0.0"
    local min_version="99.99.99"
    local found=0

    eval "min_version=\"\$lam_${app_name}_version\""

    eval "search_path=\"\$lam_${app_name}_search\""
    for i in $search_path ; do
        version="`${i} --version 2>&1`"
        if test "$?" != 0 ; then
            continue
        fi

        version="`echo $version | cut -f2 -d')'`"
        version="`echo $version | cut -f1 -d' '`"

        if check_version $min_version $version ; then
            eval "lam_${app_name}=\"${i}\""
            found=1
            break
        fi
    done

    if test "$found" = "0" ; then
	cat <<EOF
I could not find a recent enough copy of ${app_name}.
I am gonna abort.  :-(

Please make sure you are using at least the following versions of the
GNU tools:

GNU Autoconf $lam_autoconf_version
GNU Automake $lam_automake_version
GNU Libtool  $lam_libtool_version

EOF
        exit 1
    fi
}


##############################################################################
#
# run_and_check - run the right GNU tool, printing warning on failure
#
# INPUT:
#    - name of application (eg aclocal)
#    - program arguments
#
# OUTPUT:
#    none
#
# SIDE EFFECTS:
#    - aborts on error running application
#
##############################################################################
run_and_check() {
    local rac_progs="$*"
    echo "[Running] $rac_progs"
    eval $rac_progs
    if test "$?" != 0; then
	cat <<EOF

It seems that the execution of "$rac_progs" has failed.
I am gonna abort.  :-(

This may be caused by an older version of one of the required
packages.  Please make sure you are using at least the following
versions:

GNU Autoconf $lam_autoconf_version
GNU Automake $lam_automake_version
GNU Libtool  $lam_libtool_version

EOF
	exit 1
    fi
}

##############################################################################
#
# find_and_delete -  look for standard files in a number of common places
#     (e.g., ./config.guess, config/config.guess, dist/config.guess), and
#     delete it.  If it's not found there, look for AC_CONFIG_AUX_DIR in
#     the configure.in script and try there.  If it's not there, oh well.
#
# INPUT:
#    - file to delete
#
# OUTPUT:
#    none
#
# SIDE EFFECTS:
#    - files may disappear
#
##############################################################################
find_and_delete() {
    local fad_file="$1"

    local fad_cfile
    local auxdir

    # Look for the file in "standard" places

    if test -f $fad_file; then
	rm -f $fad_file
    elif test -d config/$fad_file; then
	rm -f config/$fad_file
    elif test -d dist/$fad_file; then
	rm -f dist/$fad_file
    else

	# Didn't find it -- look for an AC_CONFIG_AUX_DIR line in
	# configure.[in|ac]

	if test -f configure.in; then
	    fad_cfile=configure.in
	elif test -f configure.ac; then
	    fad_cfile=configure.ac
	else
	    echo "--> Errr... there's no configure.in or configure.ac file!"
	fi
	if test -n "$fad_cfile"; then
	    auxdir="`grep AC_CONFIG_AUX_DIR $fad_cfile | cut -d\( -f 2 | cut -d\) -f 1`"
	fi	
	if test -f "$auxdir/$fad_file"; then
	    rm -f "$auxdir/$fad_file"
	fi
    fi
}


##############################################################################
#
# run_gnu_tools - run the GNU tools in a given directory
#
# INPUT:
#    - directory to run in
#    - LAM top directory
#
# OUTPUT:
#    none
#
# SIDE EFFECTS:
#    - assumes that the directory is ready to have the GNU tools run
#      in it (i.e., there's some form of configure.*)
#    - may preprocess the directory before running the GNU tools
#      (e.g., generale Makefile.am's from configure.params, etc.)
#
##############################################################################
run_gnu_tools() {
    rgt_dir="$1"
    rgt_lam_topdir="$2"

    # Sanity check to ensure that there's a configure.in or
    # configure.ac file here, or if there's a configure.params
    # file and we need to run make_configure.pl.

    if test -f configure.params -a -f configure.stub -a \
	-x "$rgt_lam_topdir/config/mca_make_configure.pl"; then
        cat <<EOF
*** Found configure.stub
*** Running mca_make_configure.pl
EOF
	"$rgt_lam_topdir/config/mca_make_configure.pl" \
	    --lamdir "$rgt_lam_topdir" \
	    --moduledir "`pwd`"
        if test "$?" != "0"; then
           echo "*** autogen.sh failed to complete!"
           exit 1
        fi
	happy=1
	file=configure.ac
    elif test -f configure.in; then
	happy=1
	file=configure.in
    elif test -f configure.ac; then
	happy=1
	file=configure.ac
    else
        cat <<EOF
--> Err... there's no configure.in or configure.ac file in this directory"
--> Confused; aborting in despair
EOF
	exit 1
    fi
    unset happy

    # Find and delete the GNU helper script files

    find_and_delete config.guess
    find_and_delete config.sub
    find_and_delete depcomp
    find_and_delete install-sh
    find_and_delete ltconfig
    find_and_delete ltmain.sh
    find_and_delete missing
    find_and_delete mkinstalldirs
    find_and_delete libtool

    # Run the GNU tools

    echo "*** Running GNU tools"

    run_and_check $lam_aclocal
    if test "`grep AC_CONFIG_HEADER $file`" != "" -o \
	"`grep AM_CONFIG_HEADER $file`" != ""; then
	run_and_check $lam_autoheader
    fi
    run_and_check $lam_autoconf

    # We only need the libltdl stuff for the top-level
    # configure, not any of the MCA modules.

    if test -f src/include/mpi.h; then
	rm -rf libltdl src/mca/libltdl src/mca/ltdl.h
	run_and_check $lam_libtoolize --automake --copy --ltdl
	mv libltdl src/mca

	echo "Adjusting libltdl for LAM :-("

	echo "  -- adding sym link for src/mca/ltdl.h"
        cd src/mca
	ln -s libltdl/ltdl.h ltdl.h
        cd ../..

	echo "  -- patching for argz bugfix in libtool 1.5"
	cd src/mca/libltdl
	patch -N -p0 <<EOF
--- ltdl.c.old  2003-11-26 16:42:17.000000000 -0500
+++ ltdl.c      2003-12-03 17:06:27.000000000 -0500
@@ -682,7 +682,7 @@
   /* This probably indicates a programmer error, but to preserve
      semantics, scan back to the start of an entry if BEFORE points
      into the middle of it.  */
-  while ((before >= *pargz) && (before[-1] != LT_EOS_CHAR))
+  while ((before > *pargz) && (before[-1] != LT_EOS_CHAR))
     --before;

   {
EOF
	cd ../../..
	echo "  -- patching configure for broken -c/-o compiler test"
	sed -e 's/chmod -w \./#LAM\/MPI FIX: chmod -w ./' \
	    configure > configure.new
	mv configure.new configure
	chmod a+x configure
    else
	run_and_check $lam_libtoolize --automake --copy
    fi
    run_and_check $lam_automake --foreign -a --copy --include-deps
}


##############################################################################
#
# process_dir - look at the files present in a given directory, and do
# one of the following:
#    - skip/ignore it
#    - run custom autogen.sh in it
#    - run the GNU tools in it
#    - get a list of Makefile.am's to add to the top-level configure
#
# INPUT:
#    - directory to run in
#    - LAM top directory
#
# OUTPUT:
#    none
#
# SIDE EFFECTS:
#    - skips directories with .lam_no_gnu .lam_ignore
#    - uses provided autogen.sh if available
#
##############################################################################
process_dir() {
    pd_dir="$1"
    pd_lam_topdir="$2"
    pd_cur_dir="`pwd`"

    if test -d "$pd_dir"; then
	cd "$pd_dir"

	# See if the package doesn't want us to set it up

	if test -f .lam_no_gnu; then
	    cat <<EOF

*** Found .lam_no_gnu file -- skipping GNU setup in:
***   `pwd`

EOF
	elif test -f .lam_ignore; then
	    cat <<EOF

*** Found .lam_ignore file -- skipping entire tree:
***   `pwd`

EOF
	elif test "$pd_dir" != "." -a -x autogen.sh; then
	    cat <<EOF

*** Found custom autogen.sh file in:
***   `pwd`

EOF
	    ./autogen.sh
        elif test -f configure.ac -o -f configure.in; then
            # If we have configure.ac or configure.in, run the GNU
            # tools here

	    cat <<EOF

*** Found configure.(in|ac)
***   `pwd`

EOF
            run_gnu_tools "$pd_dir" "$pd_lam_topdir"

        elif test -f configure.params -a -f configure.stub; then
	    cat <<EOF

*** Found configure.params and configure.stub
***   `pwd`

EOF
            run_gnu_tools "$pd_dir" "$pd_lam_topdir"

        elif test -f configure.params; then
	    cat <<EOF

*** Found configure.params
***   `pwd`

EOF
            . ./configure.params
            if test -z "$PARAM_CONFIG_FILES"; then
                cat <<EOF
*** No PARAM_CONFIG_FILES!
*** Nothing to do -- skipping this directory
EOF
            else
                pd_module_name="`basename $pd_dir`"
                pd_module_type="`dirname $pd_dir`"
                pd_module_type="`basename $pd_module_type`"
                pd_get_ver="../../../../../config/lam_get_version.sh"
                pd_ver_file="`grep PARAM_VERSION_FILE configure.params`"
                if test -z "$pd_ver_file"; then
                    pd_ver_file="VERSION"
                else
                    pd_ver_file="`echo $pd_ver_file | cut -d= -f1`"
                fi

                # Write out to two files (they're merged at the end)

                pd_list_file="$pd_lam_topdir/$mca_no_config_list_file"
                pd_amc_file="$pd_lam_topdir/$mca_no_config_amc_file"

                cat >> $pd_list_file <<EOF
dnl ----------------------------------------------------------------

dnl No-configure module: 
dnl    $pd_dir

EOF
                cat >> $pd_amc_file <<EOF
dnl ----------------------------------------------------------------

dnl No-configure module: 
dnl    $pd_dir

EOF
                for file in $PARAM_CONFIG_FILES; do
                    echo "AC_CONFIG_FILES([$pd_dir/$file])" >> $pd_list_file
                done

                # Get all the version numbers

                pd_ver="`sh $pd_get_ver $pd_ver_file --all`"
                pd_ver_full="`echo $pd_ver | cut -d: -f1`"
                pd_ver_major="`echo $pd_ver | cut -d: -f2`"
                pd_ver_minor="`echo $pd_ver | cut -d: -f3`"
                pd_ver_release="`echo $pd_ver | cut -d: -f4`"
                pd_ver_alpha="`echo $pd_ver | cut -d: -f5`"
                pd_ver_beta="`echo $pd_ver | cut -d: -f6`"
                pd_ver_cvs="`echo $pd_ver | cut -d: -f7`"
                cat >> $pd_list_file <<EOF

MCA_${pd_module_type}_NO_CONFIGURE_SUBDIRS="$pd_dir \$MCA_${pd_module_type}_NO_CONFIGURE_SUBDIRS"

dnl Since AM_CONDITIONAL does not accept a variable name as its first
dnl argument, we generate it here, and the variable used in the test
dnl will be filled in later.

dnl Similarly, AC_DEFINE_UNQUOTED doesn't take a variable first
dnl argument.  So we have to figure it out here.

AC_DEFINE_UNQUOTED(MCA_${pd_module_type}_${pd_module_name}_MAJOR_VERSION, 
    $pd_ver_major,
    [Major LAM MCA $pd_module_type $pd_module_name version])
AC_DEFINE_UNQUOTED(MCA_${pd_module_type}_${pd_module_name}_MINOR_VERSION, 
    $pd_ver_minor,
    [Minor LAM MCA $pd_module_type $pd_module_name version])
AC_DEFINE_UNQUOTED(MCA_${pd_module_type}_${pd_module_name}_RELEASE_VERSION, 
    $pd_ver_release,
    [Release LAM MCA $pd_module_type $pd_module_name version])
AC_DEFINE_UNQUOTED(MCA_${pd_module_type}_${pd_module_name}_ALPHA_VERSION, 
    $pd_ver_alpha,
    [Alpha LAM MCA $pd_module_type $pd_module_name version])
AC_DEFINE_UNQUOTED(MCA_${pd_module_type}_${pd_module_name}_BETA_VERSION, 
    $pd_ver_beta,
    [Beta LAM MCA $pd_module_type $pd_module_name version])
AC_DEFINE_UNQUOTED(MCA_${pd_module_type}_${pd_module_name}_CVS_VERSION, 
    $pd_ver_cvs,
    [CVS LAM MCA $pd_module_type $pd_module_name version])
AC_DEFINE_UNQUOTED(MCA_${pd_module_type}_${pd_module_name}_FULL_VERSION, 
    "$pd_ver_full",
    [Full LAM MCA $pd_module_type $pd_module_name version])

EOF
                cat >> $pd_amc_file <<EOF
AM_CONDITIONAL(LAM_BUILD_${pd_module_type}_${pd_module_name}_LOADABLE_MODULE,
    test "\$BUILD_${pd_module_type}_${pd_module_name}_LOADABLE_MODULE" = "1")

EOF

                cat <<EOF
--> Adding to top-level configure no-configure subdirs:
-->   $pd_dir
--> Adding to top-level configure AC_CONFIG_FILES list:
-->   $PARAM_CONFIG_FILES
EOF
            fi
        else
	    cat <<EOF

*** Nothing found; directory skipped
***   `pwd`

EOF
        fi

        # Go back to the topdir

        cd "$pd_cur_dir"
    fi
    unset pd_dir pd_lam_topdir pd_cur_dir pd_module_type
}


##############################################################################
#
# run_global - run the config in the top LAM dir and all MCA modules
#
# INPUT:
#    none
#
# OUTPUT:
#    none
#
# SIDE EFFECTS:
#
##############################################################################
run_global() {
    # [Re-]Create the mca_module_list file

    rm -f "$mca_no_configure_modules_file" "$mca_no_config_list_file" \
        "$mca_no_config_amc_file"
    touch "$mca_no_configure_modules_file" "$mca_no_config_list_file" \
        "$mca_no_config_amc_file"

    # Now run the config in every directory in src/mca/[lam|mpi]/*/*
    # that has a configure.in or configure.ac script

    rg_cwd="`pwd`"
    echo $rg_cwd
    for type in src/mca/lam/* src/mca/mpi/*; do
	if test -d "$type"; then
	    for module in "$type"/*; do
		if test -d "$module"; then
		    if test -f "$module/configure.in" -o \
			-f "$module/configure.params" -o \
			-f "$module/configure.ac"; then
			process_dir "$module" "$rg_cwd"
		    fi
		fi
	    done
	fi
    done

    # Fill in the final m4 file

    cat > "$mca_no_configure_modules_file" <<EOF
dnl
dnl \$HEADER
dnl

dnl This file is automatically created by autogen.sh; it should not
dnl be edited by hand!!

dnl List all the no-configure modules that we found, and AC_DEFINE
dnl their versions

AC_DEFUN([MCA_FIND_NO_CONFIGURE_MODULES],[
MCA_cofs_NO_CONFIGURE_SUBDIRS=""
MCA_pcm_NO_CONFIGURE_SUBDIRS=""
MCA_registry_NO_CONFIGURE_SUBDIRS=""

MCA_coll_NO_CONFIGURE_SUBDIRS=""
MCA_io_NO_CONFIGURE_SUBDIRS=""
MCA_one_NO_CONFIGURE_SUBDIRS=""
MCA_pml_NO_CONFIGURE_SUBDIRS=""
MCA_ptl_NO_CONFIGURE_SUBDIRS=""
MCA_topo_NO_CONFIGURE_SUBDIRS=""

`cat $mca_no_config_list_file`
])dnl

dnl Separately have the AM_CONDITIONALS as to whether we build the
dnl modules static or shared.  This must be done separately from the
dnl list because we have to do it late in the configure script, after
dnl all the test variable values have been set.

AC_DEFUN([MCA_AMC_NO_CONFIGURE_MODULES],[
`cat $mca_no_config_amc_file`
])dnl
EOF
    # Remove temp files

    rm -f $mca_no_config_list_file $mca_no_config_amc_file

    # Finally, after we found all the no-configure MCA modules, run
    # the config in the top-level directory

    process_dir . .

    unset type module
}


##############################################################################
#
# main - do the real work...
#
##############################################################################

# announce
echo "[Checking] command line parameters"

# Check the command line to see if we should run the whole shebang, or
# just in this current directory.

want_local=no
lamdir=
for arg in $*; do
    case $arg in
    -l) want_local=yes ;;
    -lamdir|--lamdir|-lam|--lam) lamdir="$1" ;;
    *) ;;
    esac
done

# announce
echo "[Checking] prerequisites"

# sanity check to make sure user isn't being stupid
if test ! -d CVS -a ! -d .svn ; then
    cat <<EOF

This doesn't look like a developer copy of LAM/MPI.  You probably do not
want to run autogen.sh - it is normally not needed for a release source
tree.  Giving you 5 seconds to reconsider and kill me.

EOF
    sleep 5
fi

# figure out if we're at the top level of the LAM tree, a module's
# top-level directory, or somewhere else.
if test -f VERSION -a -f configure.ac -a -f src/include/mpi.h ; then
    # Top level of LAM tree
    lamdir="`pwd`"
elif test -f configure.in -o -f configure.ac -o -f configure.params ; then
    # Top level of a module directory
    want_local=yes
    if test -z "$lamdir"; then
        lamdir="../../../../.."
    fi
else
    cat <<EOF

You must run this script from either the top level of the LAM
directory tree or the top-level of an MCA module directory tree.

EOF
    exit 1
fi

# find all the apps we are going to run
find_app "aclocal"
find_app "autoheader"
find_app "autoconf"
find_app "libtoolize"
find_app "automake"

# do the work
if test "$want_local" = "yes"; then
    process_dir . $lamdir
else
    run_global
fi

# All done
exit 0
