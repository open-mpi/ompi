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

lam_automake_version="1.6"
lam_autoconf_version="2.57"
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
    echo "$rac_progs"
    eval $rac_progs
    if test "$?" != 0; then
	cat <<EOF

It seems that the execution of "$rac_progs" has failed.
I am gonna abort.  :-(

This may be caused by an older version of one of the required
packages.  Please make sure you are using at least the following
versions:

GNU Autoconf 2.57
GNU Automake 1.7.8
GNU Libtool  1.5

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
# main - do the real work...
#
##############################################################################

# sanity check to make sure user isn't being stupid
if test ! -d CVS ; then
    cat <<EOF

This doesn't look like a developer copy of LAM/MPI.  You probably do not
want to run autogen.sh - it is normally not needed for a release source
tree.  Giving you 2 seconds to reconsider and kill me.

EOF
    sleep 2
fi


# make sure we are at the top of the tree
if test -f VERSION -a -f configure.ac -a -f src/mpi/datatype/d_get_name.c ; then
    bad=0
else
    cat <<EOF

You must run this script from the top-level LAM directory.

EOF
    exit 1
fi


# find all the apps we are going to run
find_app "aclocal"
find_app "autoheader"
find_app "autoconf"
find_app "libtoolize"
find_app "automake"

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
run_and_check $lam_aclocal
if test "`grep AC_CONFIG_HEADER configure.ac`" != "" -o \
    "`grep AM_CONFIG_HEADER configure.ac`" != ""; then
    run_and_check $lam_autoheader
fi
run_and_check $lam_autoconf
echo "  -- patching configure for broken -c/-o compiler test"
sed -e 's/chmod -w \./#LAM\/MPI FIX: chmod -w ./' \
    configure > configure.new
mv configure.new configure
chmod a+x configure

run_and_check $lam_libtoolize --automake --copy
run_and_check $lam_automake --foreign -a --copy --include-deps
	
# All done
exit 0
