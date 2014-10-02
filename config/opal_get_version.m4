dnl -*- shell-script -*-
dnl
dnl Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
dnl                         University Research and Technology
dnl                         Corporation.  All rights reserved.
dnl Copyright (c) 2004-2005 The University of Tennessee and The University
dnl                         of Tennessee Research Foundation.  All rights
dnl                         reserved.
dnl Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
dnl                         University of Stuttgart.  All rights reserved.
dnl Copyright (c) 2004-2005 The Regents of the University of California.
dnl                         All rights reserved.
dnl Copyright (c) 2008-2014 Cisco Systems, Inc.  All rights reserved.
dnl $COPYRIGHT$
dnl
dnl Additional copyrights may follow
dnl
dnl $HEADER$
dnl

dnl
dnl This file is also used as input to opal_get_version.sh.
dnl

# OPAL_GET_VERSION(version_file, variable_prefix)
# -----------------------------------------------
# parse version_file for version information, setting
# the following shell variables:
#
#  prefix_VERSION
#  prefix_BASE_VERSION
#  prefix_MAJOR_VERSION
#  prefix_MINOR_VERSION
#  prefix_RELEASE_VERSION
#  prefix_GREEK_VERSION
#  prefix_REPO_REV
#  prefix_TARBALL_VERSION
#  prefix_RELEASE_DATE
m4_define([OPAL_GET_VERSION],[
    dnl quote eval to suppress macro expansion with non-GNU m4
    if test -f "$1"; then
        srcdir=`dirname $1`
        ompi_vers=`sed -n "
	t clear
	: clear
	s/^major/$2_MAJOR_VERSION/
	s/^minor/$2_MINOR_VERSION/
	s/^release/$2_RELEASE_VERSION/
	s/^greek/$2_GREEK_VERSION/
	s/^repo_rev/$2_REPO_REV/
	s/^tarball_version/$2_TARBALL_VERSION/
	s/^date/$2_RELEASE_DATE/
	t print
	b
	: print
	p" < "$1"`
	[eval] "$ompi_vers"

        # Only print release version if it isn't 0
        if test $$2_RELEASE_VERSION -ne 0 ; then
            $2_VERSION="$$2_MAJOR_VERSION.$$2_MINOR_VERSION.$$2_RELEASE_VERSION"
        else
            $2_VERSION="$$2_MAJOR_VERSION.$$2_MINOR_VERSION"
        fi
        $2_VERSION="${$2_VERSION}${$2_GREEK_VERSION}"

        if test "$$2_TARBALL_VERSION" = ""; then
            $2_TARBALL_VERSION=$$2_VERSION
        fi

        m4_ifdef([AC_MSG_CHECKING],
                 [AC_MSG_CHECKING([for repo version])])

        # If repo_rev was not set in the VERSION file, then get it now
        if test "$$2_REPO_REV" = ""; then
            # See if we can find the "git" command.
            git_happy=0
            git --version > /dev/null 2>&1
            if test $? -eq 0; then
                git_happy=1
            fi

            # If we're in a git repo and we found the git command, use
            # git describe to get the repo rev
            if test -d "$srcdir/.git" && test $git_happy -eq 1; then
                $2_REPO_REV=`git describe --tags --always`
            else
                $2_REPO_REV="date`date '+%Y-%m-%d'`"
            fi
        fi

        m4_ifdef([AC_MSG_RESULT],
                 [AC_MSG_RESULT([$$2_REPO_REV])])
    fi
])
