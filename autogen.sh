#! /bin/sh 
#
# Copyright (c) 2003 The Trustees of Indiana University.  
#                    All rights reserved.
#
# This file is part of the LAM/MPI software package.  For license
# information, see the LICENSE file in the top level directory of the
# LAM/MPI source distribution.
#
# $Id: autogen.sh,v 1.1 2003/11/22 16:36:20 jsquyres Exp $
#
# This script is run on developer copies of LAM/MPI -- *not*
# distribution tarballs.
#
# Some helper functions
#

#
# Subroutine to check for the existence of various standard GNU tools
#
test_for_existence() {
    tfe_prog="$1"
    tfe_foo="`$tfe_prog --version`"
    if test "$?" != 0; then
	cat <<EOF

You must have GNU autoconf, automake, and libtool installed to build
the developer's version of LAM/MPI.  You can obtain these packages
from ftp://ftp.gnu.org/gnu/.

EOF
	# Stupid emacs: '
	exit 1
    fi
    unset tfe_prog tfe_foo
}


#
# Subroutine to execite the standard GNU tools, and if they fail,
# print out a warning.
#
run_and_check() {
    rac_progs="$*"
    echo "$rac_progs"
    eval $rac_progs
    if test "$?" != 0; then
	cat <<EOF

It seems that the execution of "$progs" has failed.
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
    unset rac_progs
}

#
# Subroutine to look for standard files in a number of common places
# (e.g., ./config.guess, config/config.guess, dist/config.guess), and
# delete it.  If it's not found there, look for AC_CONFIG_AUX_DIR in
# the configure.in script and try there.  If it's not there, oh well.
#
find_and_delete() {
    fad_file="$1"

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
	unset fad_cfile
    fi
    unset fad_file
}


##########################################################################
# Main
##########################################################################

if test -f VERSION -a -f configure.ac -a -f src/mpi/datatype/get_name.c ; then
    bad=0
else
    cat <<EOF

You must run this script from the top-level LAM directory.

EOF
    exit 1
fi

test_for_existence autoconf
test_for_existence automake
test_for_existence libtool

# See if the package doesn't want us to set it up

cat <<EOF

*** Running GNU tools in directory: 
***   `pwd`

EOF

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

run_and_check aclocal
if test "`grep AC_CONFIG_HEADER configure.ac`" != "" -o \
    "`grep AM_CONFIG_HEADER configure.ac`" != ""; then
    run_and_check autoheader
fi
run_and_check autoconf
run_and_check libtoolize --automake --copy
run_and_check automake --foreign -a --copy --include-deps
	
# All done

exit 0
