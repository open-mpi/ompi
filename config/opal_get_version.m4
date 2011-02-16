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
dnl Copyright (c) 2008-2011 Cisco Systems, Inc.  All rights reserved.
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
#  prefix_WANT_REPO_REV
#  prefix_REPO_REV
#  prefix_RELEASE_DATE
m4_define([OPAL_GET_VERSION],[
    : ${ompi_ver_need_repo_rev=1}
    : ${svnversion_result=-1}

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
	s/^want_repo_rev/$2_WANT_REPO_REV/
	s/^repo_rev/$2_REPO_REV/
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
        $2_BASE_VERSION=$$2_VERSION

        if test $$2_WANT_REPO_REV -eq 1 && test $ompi_ver_need_repo_rev -eq 1 ; then
            if test "$svnversion_result" != "-1" ; then
                $2_REPO_REV=$svnversion_result
            fi
            if test "$$2_REPO_REV" = "-1" ; then
                m4_ifdef([AC_MSG_CHECKING],
                         [AC_MSG_CHECKING([for repo version])])
                d=`date '+%m-%d-%Y'`
                if test -d "$srcdir/.svn" ; then
                    $2_REPO_REV=r`svnversion "$srcdir"`
                    if test $? != 0; then
                        # The following is too long for Fortran
                        # $2_REPO_REV="unknown svn version (svnversion not found); $d"
                        $2_REPO_REV="? (no svnversion); $d"
                    fi
                elif test -d "$srcdir/.hg" ; then
                    # Check to see if we can find the hg command
                    # remember that $? reflects the status of the
                    # *last* command in a pipe change, so if "hg .. 
                    # cut ..." runs and "hg" is not found, $? will
                    # reflect the status of "cut", not hg not being
                    # found.  So test for hg specifically first.
                    hg --version > /dev/null 2>&1
                    if test $? = 0; then
                        $2_REPO_REV=hg`hg -v -R "$srcdir" tip | grep ^changeset: | head -n 1 | cut -d: -f3`
                    else
                        # The following is too long for Fortran
                        # $2_REPO_REV="unknown hg version (hg not found); $d"
                        $2_REPO_REV="? (no hg); $d"
                    fi
                elif test -d "$srcdir/.git" ; then
                    # By the same logic as above, check to see if we
                    # can find the "git" command.
                    git --version > /dev/null 2>&1
                    if test $? = 0; then
                        $2_REPO_REV=git`git log -1 "$srcdir" | grep ^commit | awk '{ print $2 }'`
                    else
                        # The following is too long for Fortran
                        # $2_REPO_REV="unknown hg version (hg not found); $d"
                        $2_REPO_REV="? (no git); $d"
                    fi
                fi
                if test "$2_REPO_REV" = ""; then
                    $2_REPO_REV="date$d"
                fi
                m4_ifdef([AC_MSG_RESULT],
                         [AC_MSG_RESULT([done])])
            fi
            $2_VERSION="${$2_VERSION}${$2_REPO_REV}"
        fi
    fi
])
