#! /bin/bash 
#
# Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2004-2005 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2004-2005 The Regents of the University of California.
#                         All rights reserved.
# Copyright (c) 2007-2009 Cisco, Inc.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#
# This script is run on developer copies of PLPA -- *not* distribution
# tarballs.

#set -x

##############################################################################
#
# User-definable parameters (search path and minimum supported versions)
# 
# Note: use ';' to separate parameters
##############################################################################

ompi_aclocal_search="aclocal"
if test ! -z "$ACLOCAL"; then
    ompi_aclocal_search="$ACLOCAL"
fi
ompi_autoheader_search="autoheader"
if test ! -z "$AUTOHEADER"; then
    ompi_autoheader_search="$AUTOHEADER"
fi
ompi_autoconf_search="autoconf"
if test ! -z "$AUTOCONF"; then
    ompi_autoconf_search="$AUTOCONF"
fi
ompi_libtoolize_search="libtoolize;glibtoolize"
if test ! -z "$LIBTOOLIZE"; then
    ompi_libtoolize_search="$LIBTOOLIZE"
fi
ompi_automake_search="automake"
if test ! -z "$AUTOMAKE"; then
    ompi_automake_search="$AUTOMAKE"
fi

ompi_automake_version="1.9.6"
ompi_autoconf_version="2.59"
ompi_libtool_version="1.5.22"


##############################################################################
#
# Global variables - should not need to modify defaults
#
##############################################################################

ompi_aclocal_version="$ompi_automake_version"
ompi_autoheader_version="$ompi_autoconf_version"
ompi_libtoolize_version="$ompi_libtool_version"

# program names to execute
ompi_aclocal=""
ompi_autoheader=""
ompi_autoconf=""
ompi_libtoolize=""
ompi_automake=""

mca_no_configure_components_file="config/mca_no_configure_components.m4"
mca_no_config_list_file="mca_no_config_list"
mca_no_config_env_file="mca_no_config_env"
mca_m4_include_file="mca_m4_config_include.m4"
mca_m4_config_env_file="mca_m4_config_env"
autogen_subdir_file="autogen.subdirs"

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
    local tmpIFS=$IFS

    eval "min_version=\"\$ompi_${app_name}_version\""
    eval "search_path=\"\$ompi_${app_name}_search\""
    IFS=";"
    for i in $search_path ; do
        IFS="$tmpIFS"
        version="`${i} --version 2>&1`"
        if test "$?" != 0 ; then
            IFS=";"
            continue
        fi

        version="`echo $version | cut -f2 -d')'`"
        version="`echo $version | cut -f1 -d' '`"

        if check_version $min_version $version ; then
            eval "ompi_${app_name}=\"${i}\""
            found=1
            break
        fi
    done

    IFS="$tmpIFS"

    if test "$found" = "0" ; then
	cat <<EOF
I could not find a recent enough copy of ${app_name}.
I am gonna abort.  :-(

Please make sure you are using at least the following versions of the
GNU tools:

GNU Autoconf $ompi_autoconf_version
GNU Automake $ompi_automake_version
    NOTE: You may need Automake 1.8.5 (or higher) in order to run
    "make dist" successfully
GNU Libtool  $ompi_libtool_version

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

-------------------------------------------------------------------------
It seems that the execution of "$rac_progs" has failed.  See above for
the specific error message that caused it to abort.

This *MAY* be caused by an older version of one of the required
packages.  Please make sure you are using at least the following
versions:

GNU Autoconf $ompi_autoconf_version
GNU Automake $ompi_automake_version
GNU Libtool  $ompi_libtool_version
-------------------------------------------------------------------------

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
#    none
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

    # Find and delete the GNU helper script files

    find_and_delete config.guess
    find_and_delete config.sub
    find_and_delete depcomp
    find_and_delete compile
    find_and_delete install-sh
    find_and_delete ltconfig
    find_and_delete ltmain.sh
    find_and_delete missing
    find_and_delete mkinstalldirs
    find_and_delete libtool
    find_and_delete configure

    # Run the GNU tools

    echo "*** Running GNU tools"

    run_and_check $ompi_aclocal
    run_and_check $ompi_autoheader
    run_and_check $ompi_autoconf
    run_and_check $ompi_libtoolize --automake --copy
    run_and_check $ompi_automake --foreign -a --copy --include-deps
}


##############################################################################
#
# main - do the real work...
#
##############################################################################

# announce
echo "[Checking] prerequisites"

# sanity check to make sure user isn't being stupid
if test ! -d .svn -a ! -d .hg ; then
    cat <<EOF

This doesn't look like a developer copy of PLPA.  You probably do not
want to run autogen.sh - it is normally not needed for a release
source tree.  Giving you 5 seconds to reconsider and kill me.

EOF
    sleep 5
fi


if test -f VERSION -a -f configure.ac -a -f src/libplpa/plpa.h.in ; then
    # Top level of PLPA tree
    uptime > /dev/null
else
    cat <<EOF

You must run this script from either the top level of the PLPA
directory tree or the top-level of an MCA component directory tree.

EOF
    exit 1
fi

# find all the apps we are going to run
find_app "aclocal"
find_app "autoheader"
find_app "autoconf"
find_app "libtoolize"
find_app "automake"

run_gnu_tools

# All done
exit 0
